module Yatima.Parse.Literal where

import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (chr, digitToInt, isDigit, isHexDigit, isOctDigit, ord)
import Data.IPLD.Cid
import Data.Int
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Numeric.Natural
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Yatima.Parse.Integer
import Yatima.Parse.Parser
import Yatima.Term

pLiteral :: (Ord e, Monad m) => Parser e m Literal
pLiteral =
  label "a literal" $
    choice
      [ string "#world" >> return VWorld,
        string "#exception " >> pString >>= (\s -> return $ VException s),
        pBits,
        try $ VNatural <$> pNatural,
        try $ pFloat,
        pInt,
        try $ VString <$> pString,
        VChar <$> pChar
      ]

pLitType :: (Ord e, Monad m) => Parser e m LitType
pLitType =
  label "the type of a literal" $
    choice
      [ string "#World" >> return TWorld,
        string "#Exception" >> return TException,
        string "#Natural" >> return TNatural,
        string "#String" >> return TString,
        string "#Char" >> return TChar,
        string "#I64" >> return TI64,
        string "#I32" >> return TI32,
        string "#F64" >> return TF64,
        string "#F32" >> return TF32,
        string "#BitVector" >> return TBitVector
      ]

pNum :: (Ord e, Monad m) => Parser e m Integer
pNum =
  choice
    [ string "0x" >> hexadecimal,
      string "0b" >> binary,
      decimal
    ]

pIntType :: (Ord e, Monad m) => Parser e m Text
pIntType = choice [string "i32", string "i64", string "u32", string "u64"]

pNatural :: (Ord e, Monad m) => Parser e m Natural
pNatural = do
  val <- notFollowedBy (string "_") >> pNum
  notFollowedBy (pIntType <|> choice [string "f32", string "f64", string "."])
  return $ fromIntegral val

pInt :: (Ord e, Monad m) => Parser e m Literal
pInt = do
  val <- L.signed (pure ()) $ notFollowedBy (string "_") >> pNum
  notFollowedBy (choice [string "f32", string "f64", string "."])
  typ <- pIntType
  case typ of
    "i64" -> do
      when
        (val > (fromIntegral (maxBound :: Int64)))
        (customFailure $ I64Overflow val)
      when
        (val < (fromIntegral (minBound :: Int64)))
        (customFailure $ I64Underflow val)
      return $ VI64 $ fromIntegral val
    "i32" -> do
      when
        (val > (fromIntegral (maxBound :: Int32)))
        (customFailure $ I32Overflow val)
      when
        (val < (fromIntegral (minBound :: Int32)))
        (customFailure $ I32Underflow val)
      return $ VI32 $ fromIntegral val
    "u64" -> do
      when
        (val > (fromIntegral (maxBound :: Word64)))
        (customFailure $ U64Overflow val)
      when
        (val < (fromIntegral (minBound :: Word64)))
        (customFailure $ U64Underflow val)
      return $ VI64 $ fromIntegral val
    "u32" -> do
      when
        (val > (fromIntegral (maxBound :: Word32)))
        (customFailure $ U32Overflow val)
      when
        (val < (fromIntegral (minBound :: Word32)))
        (customFailure $ U32Underflow val)
      return $ VI32 $ fromIntegral val

pFloat :: (Ord e, Monad m) => Parser e m Literal
pFloat =
  choice
    [ try $ VF64 <$> (L.signed (pure ()) L.float <* string "f64"),
      try $ VF32 <$> (L.signed (pure ()) L.float <* string "f32")
    ]

data Base = Bin | Oct | Dec | Hex deriving (Eq, Enum, Show)

pBits :: (Ord e, Monad m) => Parser e m Literal
pBits = do
  string "#"
  len <- (Just <$> decimal) <|> return Nothing
  base <-
    choice
      [ string "x" >> return Hex,
        string "o" >> return Oct,
        string "b" >> return Bin,
        string "d" >> return Dec
      ]
  ds <- removeSep <$> many (satisfy (\x -> isBaseDigit base x || x == '_'))
  let len' = fromIntegral <$> (length ds *) <$> baseBitSize base
  case len <|> len' of
    Just len -> do
      let bs = mkNum base ds
      when (bs >= 2 ^ len) (customFailure $ BitVectorOverflow bs len)
      return $ VBitVector len bs
    Nothing -> customFailure BaseNeedsLength
  where
    removeSep :: [Char] -> [Natural]
    removeSep ds = toEnum <$> digitToInt <$> (filter (/= '_') ds)
    isBaseDigit base = case base of
      Hex -> isHexDigit
      Dec -> isDigit
      Oct -> isOctDigit
      Bin -> \x -> x == '0' || x == '1'
    baseBitSize base = case base of
      Hex -> Just 4
      Dec -> Nothing
      Oct -> Just 3
      Bin -> Just 1
    baseExponent base = case base of
      Hex -> 16
      Dec -> 10
      Oct -> 8
      Bin -> 2
    mkNum base ds = foldl' (\acc d -> acc * (baseExponent base) + d) 0 ds

pOpr :: (Ord e, Monad m) => Parser e m PrimOp
pOpr = label "a primitive operation" $ choice $ mkParse <$> [minBound .. maxBound]
  where
    mkParse :: (Ord e, Monad m) => PrimOp -> Parser e m PrimOp
    mkParse x = try $ do
      string ("#" <> primOpName x)
      notFollowedBy alphaNumChar
      return x

pString :: (Ord e, Monad m) => Parser e m Text
pString = do
  string "\""
  str <-
    many $
      choice
        [ pure <$> noneOf ['\\', '"'],
          string "\\&" >> return [],
          pure <$> pEscape
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
    [ string "\\" >> return '\\',
      string "\"" >> return '"',
      string "\'" >> return '\'',
      string "x" >> chr <$> L.hexadecimal,
      string "o" >> chr <$> L.octal,
      string "0" >> return '\0',
      string "n" >> return '\n',
      string "r" >> return '\r',
      string "v" >> return '\v',
      string "t" >> return '\t',
      string "b" >> return '\b',
      string "a" >> return '\a',
      string "f" >> return '\f',
      string "ACK" >> return '\ACK',
      string "BEL" >> return '\BEL',
      string "BS" >> return '\BS',
      string "CAN" >> return '\CAN',
      string "CR" >> return '\CR',
      string "DEL" >> return '\DEL',
      string "DC1" >> return '\DC1',
      string "DC2" >> return '\DC2',
      string "DC3" >> return '\DC3',
      string "DC4" >> return '\DC4',
      string "DLE" >> return '\DLE',
      string "ENQ" >> return '\ENQ',
      string "EOT" >> return '\EOT',
      string "ESC" >> return '\ESC',
      string "ETX" >> return '\ETX',
      string "ETB" >> return '\ETB',
      string "EM" >> return '\EM',
      string "FS" >> return '\FS',
      string "FF" >> return '\FF',
      string "GS" >> return '\GS',
      string "HT" >> return '\HT',
      string "LF" >> return '\LF',
      string "NUL" >> return '\NUL',
      string "NAK" >> return '\NAK',
      string "RS" >> return '\RS',
      string "SOH" >> return '\SOH',
      string "STX" >> return '\STX',
      string "SUB" >> return '\SUB',
      string "SYN" >> return '\SYN',
      string "SI" >> return '\SI',
      string "SO" >> return '\SO',
      string "SP" >> return '\SP',
      string "US" >> return '\US',
      string "VT" >> return '\VT',
      string "^@" >> return '\0',
      string "^[" >> return '\ESC',
      string "^\\" >> return '\FS',
      string "^]" >> return '\GS',
      string "^^" >> return '\RS',
      string "^_" >> return '\US',
      (\c -> chr $ (ord c) - 64) <$> (string "^" >> oneOf ['A' .. 'Z']),
      chr <$> L.decimal
    ]

pCid :: (Ord e, Monad m) => Parser e m Cid
pCid = do
  txt <- T.pack <$> (many alphaNumChar)
  case cidFromText txt of
    Left err -> customFailure $ InvalidCid (T.pack err) txt
    Right cid -> return $ cid
