module Yatima.Parse.Literal where

import           Control.Monad
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.ByteString            as BS
import           Data.ByteString            (ByteString)
import           Data.Bits
import           Data.Word
import           Data.List                  (foldl')
import           Data.Int
import           Data.Char                  (isHexDigit, chr, ord, digitToInt)
import           Data.IPLD.CID

import           Numeric.Natural

import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

import           Yatima.Term
import           Yatima.Parse.Parser
import           Yatima.Parse.Integer

pLiteral :: (Ord e, Monad m) => Parser e m Literal
pLiteral = label "a literal" $ choice
  [ string "#world"     >> return VWorld
  , string "#exception" >> pString >>= (\s -> return $ VException s)
  , pBits
  , try $ VNatural <$> pNatural
  , try $ pFloat
  , pInt
  , try $ VString <$> pString
  , VChar <$> pChar
  ]

pLitType :: (Ord e, Monad m) => Parser e m LitType
pLitType = label "the type of a literal" $ choice
  [ string "#World"     >> return TWorld
  , string "#Exception" >> return TException
  , string "#Natural"   >> return TNatural
  , string "#String"    >> return TString
  , string "#Char"      >> return TChar
  , string "#I64"       >> return TI64
  , string "#I32"       >> return TI32
  , string "#F64"       >> return TF64
  , string "#F32"       >> return TF32
  , string "#BitVector" >> return TBitVector
  ]

pNum :: (Ord e, Monad m) => Parser e m Integer
pNum = choice
  [ string "0x" >> hexadecimal
  , string "0b" >> binary
  , decimal
  ]

pIntType :: (Ord e, Monad m) => Parser e m Text
pIntType = choice [string "i32",string "i64", string "u32",string "u64"]

pNatural :: (Ord e, Monad m) => Parser e m Natural
pNatural = do
  val <- notFollowedBy (string "_") >> pNum
  notFollowedBy (pIntType <|> choice [string "f32",string "f64", string "."])
  return $ fromIntegral val

pInt :: (Ord e, Monad m) => Parser e m Literal
pInt = do
  val <- L.signed (pure ()) $ notFollowedBy (string "_") >> pNum
  notFollowedBy (choice [string "f32",string "f64", string "."])
  typ  <- pIntType
  case typ of
    "i64" -> do
      when (val > (fromIntegral (maxBound :: Int64)))
        (customFailure $ I64Overflow val)
      when (val < (fromIntegral (minBound :: Int64)))
        (customFailure $ I64Underflow val)
      return $ VI64 $ fromIntegral val
    "i32" -> do
      when (val > (fromIntegral (maxBound :: Int32)))
        (customFailure $ I32Overflow val)
      when (val < (fromIntegral (minBound :: Int32)))
        (customFailure $ I32Underflow val)
      return $ VI32 $ fromIntegral val
    "u64" -> do
      when (val > (fromIntegral (maxBound :: Word64)))
        (customFailure $ U64Overflow val)
      when (val < (fromIntegral (minBound :: Word64)))
        (customFailure $ U64Underflow val)
      return $ VI64 $ fromIntegral val
    "u32" -> do
      when (val > (fromIntegral (maxBound :: Word32)))
        (customFailure $ U32Overflow val)
      when (val < (fromIntegral (minBound :: Word32)))
        (customFailure $ U32Underflow val)
      return $ VI32 $ fromIntegral val

pFloat :: (Ord e, Monad m) => Parser e m Literal
pFloat = choice
  [ try $ VF64 <$> (L.signed (pure ()) L.float <* string "f64")
  , try $ VF32 <$> (L.signed (pure ()) L.float <* string "f32")
  ]

pBits :: (Ord e, Monad m) => Parser e m Literal
pBits = do
  string "#"
  (bitsPer, ds) <- choice
    [ (4,) <$> (string "x" >> many (satisfy (\x -> isHexDigit x || x == '_')))
    , (1,) <$> (string "b" >> many (satisfy (\x -> isBinDigit x || x == '_')))
    ]
  let bits = fromIntegral $ length ds * bitsPer
  let words = mkWords bitsPer (digitToInt <$> (filter (/= '_') ds))
  return $ VBitVector bits (BS.pack words)
  where
    mkWords _ [] = []
    mkWords b cs = let (xs,rs) = splitAt (8 `div` b) cs in 
      (fromIntegral $ foldl' (step b) 0 xs) : mkWords b rs
    step b a c = a * 2^b + c
    isBinDigit x = x == '0' || x == '1'

pOpr :: (Ord e, Monad m) => Parser e m PrimOp
pOpr = label "a primitive operation" $ choice $ mkParse <$> [minBound..maxBound]
  where
    mkParse :: (Ord e, Monad m) => PrimOp -> Parser e m PrimOp
    mkParse x = try $ do
      string ("#" <> primOpName x)
      notFollowedBy alphaNumChar
      return x

pString :: (Ord e, Monad m) => Parser e m Text
pString = do
  string "\""
  str <- many $ choice
    [ pure <$> noneOf ['\\', '"']
    , string "\\&" >> return []
    , pure <$> pEscape
    ]
  string "\""
  return $ T.pack $ concat str

pChar :: (Ord e, Monad m) => Parser e m Char
pChar = do
  char '\''
  chr <- noneOf ['\\', '\''] <|> pEscape
  char '\''
  return chr

pEscape :: (Ord e, Monad m) => Parser e m Char
pEscape = do
  char '\\'
  choice
    [ string "\\"  >> return '\\'
    , string "\""  >> return '"'
    , string "\'"  >> return '\''
    , string "x"   >> chr <$> L.hexadecimal
    , string "o"   >> chr <$> L.octal
    , string "0"   >> return '\0'
    , string "n"   >> return '\n'
    , string "r"   >> return '\r'
    , string "v"   >> return '\v'
    , string "t"   >> return '\t'
    , string "b"   >> return '\b'
    , string "a"   >> return '\a'
    , string "f"   >> return '\f'
    , string "ACK" >> return '\ACK'
    , string "BEL" >> return '\BEL'
    , string "BS"  >> return '\BS'
    , string "CAN" >> return '\CAN'
    , string "CR"  >> return '\CR'
    , string "DEL" >> return '\DEL'
    , string "DC1" >> return '\DC1'
    , string "DC2" >> return '\DC2'
    , string "DC3" >> return '\DC3'
    , string "DC4" >> return '\DC4'
    , string "DLE" >> return '\DLE'
    , string "ENQ" >> return '\ENQ'
    , string "EOT" >> return '\EOT'
    , string "ESC" >> return '\ESC'
    , string "ETX" >> return '\ETX'
    , string "ETB" >> return '\ETB'
    , string "EM"  >> return '\EM'
    , string "FS"  >> return '\FS'
    , string "FF"  >> return '\FF'
    , string "GS"  >> return '\GS'
    , string "HT"  >> return '\HT'
    , string "LF"  >> return '\LF'
    , string "NUL" >> return '\NUL'
    , string "NAK" >> return '\NAK'
    , string "RS"  >> return '\RS'
    , string "SOH" >> return '\SOH'
    , string "STX" >> return '\STX'
    , string "SUB" >> return '\SUB'
    , string "SYN" >> return '\SYN'
    , string "SI"  >> return '\SI'
    , string "SO"  >> return '\SO'
    , string "SP"  >> return '\SP'
    , string "US"  >> return '\US'
    , string "VT"  >> return '\VT'
    , string "^@"  >> return '\0'
    , string "^["  >> return '\ESC'
    , string "^\\" >> return '\FS'
    , string "^]"  >> return '\GS'
    , string "^^"  >> return '\RS'
    , string "^_"  >> return '\US'
    , (\ c -> chr $ (ord c) - 64) <$> (string "^" >> oneOf ['A'..'Z'])
    , chr <$> L.decimal
    ]

pCID :: (Ord e, Monad m) => Parser e m CID
pCID = do
  txt <- T.pack <$> (many alphaNumChar)
  case cidFromText txt of
    Left  err  -> customFailure $ InvalidCID (T.pack err) txt
    Right cid  -> return $ cid

