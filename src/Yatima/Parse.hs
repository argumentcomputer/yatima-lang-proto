{-|
Module      : Yatima.Parse
Description : Parsing expressions in the Yatima Language
Copyright   : (c) Sunshine Cybernetics, 2020
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental

This library implements a `Megaparsec` parser for the Yatima language using the
conventions specified in `Text.MegaParsec.Char.Lexer`. A helpful tutorial
explaining Megaparsec can be found on [Mark Karpov's
blog](https://markkarpov.com/tutorial/megaparsec.html)

Because the `Parser` type defined here is wrapped in an `RWST` and `IO` monad
transformer, if you wish to extend or modify it, you will find the `parseIO
function useful for testing and running the parsers defined here:

@
> parseIO (pExpr False) "λ y => (λ x => x) y"
Lam "y" (App (Lam "x" (Var "x" 0)) (Var "y" 0))
@
|-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}
module Yatima.Parse where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.RWS.Lazy     hiding (All, Typ)

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Bits
import           Data.Word
import           Data.Int
import           Data.Char                  (isHexDigit,isDigit, chr, ord, digitToInt)
import           Data.List                  (foldl')
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base16     as B16
import           Data.ByteString            (ByteString)
import           Data.Proxy

import           Numeric.Natural

import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

import           Yatima.Uses
import           Yatima.Term
import           Yatima.Print
import           Yatima.Parse.Integer

-- | The environment of a Parser
data ParseEnv = ParseEnv
  { -- | The binding context for local variables
    _context    :: Set Name
    -- | The global context for references
  , _refs       :: Set Name
  }

-- | A stub for a future parser state
type ParseState = ()

-- | A stub for a future parser log
type ParseLog = ()

-- | An empty parser environment, useful for testing
defaultParseEnv = ParseEnv Set.empty Set.empty

-- | Custom parser errrors with bespoke messaging
data ParseErr e
  = UndefinedReference Name
  | TopLevelRedefinition Name
  | ReservedKeyword Name
  | ReservedLeadingChar Char Name
  | I64Overflow Integer
  | I32Overflow Integer
  | I64Underflow Integer
  | I32Underflow Integer
  | U64Overflow Integer
  | U32Overflow Integer
  | U64Underflow Integer
  | U32Underflow Integer
  | LeadingDigit Name
  | ParseEnvironmentError e
  deriving (Eq, Ord,Show)

instance ShowErrorComponent e => ShowErrorComponent (ParseErr e) where
  showErrorComponent e = case e of
    UndefinedReference nam    -> "Undefined reference: " <> T.unpack nam
    TopLevelRedefinition nam  -> "Illegal redefinition of " <> T.unpack nam
    ReservedKeyword nam       -> "Reserved keyword: " <> T.unpack nam
    LeadingDigit nam          ->
      "Illegal leading digit in name: " <> T.unpack nam
    ReservedLeadingChar c nam ->
      "Illegal leading character " <> show c <> " in name: " <> T.unpack nam
    I64Overflow  i            -> "Overflow: "  <> show i <> "i64"
    I64Underflow i            -> "Underflow: " <> show i <> "i64"
    I32Overflow  i            -> "Overflow: "  <> show i <> "i32"
    I32Underflow i            -> "Underflow: " <> show i <> "i32"
    U64Overflow  i            -> "Overflow: "  <> show i <> "u64"
    U64Underflow i            -> "Underflow: " <> show i <> "u64"
    U32Overflow  i            -> "Overflow: "  <> show i <> "u32"
    U32Underflow i            -> "Underflow: " <> show i <> "u32"
    ParseEnvironmentError e   -> showErrorComponent e

instance ShowErrorComponent () where
  showErrorComponent () = "()"

-- | The type of the Yatima Parser. You can think of this as black box that can
-- turn into what is essentially `Text -> Either (ParseError ...) a` function
-- via `parse'`.  The parser is wrapped in the Reader-Writer-State monad
-- transformer `RWST` to give it the "ability" to have a local environment,
-- mutable state and an append only-loge
--
-- We currently only use `ParseEnv`. `ParseState` and `ParseLog` aliased to the
-- `()` unit types to "cancel" the State and Writer components. This is to aid
-- in future extension of the parser with mutable state and logging, since we
-- then only have to make those changes in one place (as opposed to also
-- changing all the unwrapping functions
type Parser e m a =
  RWST ParseEnv ParseLog ParseState (ParsecT (ParseErr e) Text m) a

parseDefault :: (Ord e, Monad m) => Parser e m a -> Text
             -> m (Either (ParseErrorBundle Text (ParseErr e)) a)
parseDefault p s = parseM p defaultParseEnv "" s

-- | A utility for running a `Parser`, since the `RWST` monad wraps `ParsecT`
parseM :: (Ord e, Monad m) => Parser e m a -> ParseEnv -> String -> Text
       -> m (Either (ParseErrorBundle Text (ParseErr e)) a)
parseM p env file txt =
  (fmap (\(x,y,z) -> x)) <$> runParserT (runRWST p env ()) file txt

-- | Parses a source-code to a Term, simplified API
parseTerm :: Text -> Maybe Term
parseTerm code = case (runIdentity $ parseDefault p code) of
  Left err  -> Nothing
  Right trm -> Just trm
  where
    p :: Parser () Identity Term
    p = pExpr False

-- | Parses a source-code to a Term, simplified API, throws
unsafeParseTerm :: Text -> Term
unsafeParseTerm code = case parseTerm code of
  Nothing  -> error "Bad parse."
  Just trm -> trm

pName :: (Ord e, Monad m) => Bool -> Parser e m Text
pName bind = label "a name: \"someFunc\",\"somFunc'\",\"x_15\", \"_1\"" $ do
  n  <- alphaNumChar <|> oneOf nameSymbol
  ns <- many (alphaNumChar <|> oneOf nameSymbol)
  let nam = T.pack (n : ns)
  if | isDigit n                 -> customFailure $ LeadingDigit nam
     | isReservedLead n          -> customFailure $ ReservedLeadingChar n nam
     | nam `Set.member` keywords -> customFailure $ ReservedKeyword nam
     | otherwise -> return nam
  where
    isReservedLead n = n `elem` ("'-" :: [Char])
    syms             = "_'-" :: [Char]
    nameSymbol       = if bind then syms else syms ++ "/"

keywords :: Set Text
keywords = Set.fromList $
  [ "let"
  , "if"
  , "where"
  , "def"
  , "Type"
  ]

-- | Consume whitespace, while skipping comments. Yatima line comments begin
-- with @--@, and block comments are bracketed by @{-@ and @-}@ symbols.
space :: (Ord e, Monad m) => Parser e m ()
space = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")


space' :: (Ord e, Monad m) => Parser e m ()
space' = space1 >> space

-- | A symbol is a string which can be followed by whitespace. The @sc@ argument
-- is for parsing indentation sensitive whitespace
symbol :: (Ord e, Monad m) => Text -> Parser e m Text
symbol txt = L.symbol space txt

symbol' :: (Ord e, Monad m) => Text -> Parser e m Text
symbol' txt = L.symbol space' txt

-- | Add a list of names to the binding context
bind :: (Ord e, Monad m) => [Name] -> Parser e m a -> Parser e m a
bind bs p =
  local (\e -> e { _context = Set.union (_context e) (Set.fromList bs) }) p

-- | Parse a quantitative usage semirig annotation. The absence of annotation is
-- considered to be the `Many` multiplicity.
pUses :: (Ord e, Monad m) => Parser e m Uses
pUses = pUsesAnnotation <|> return Many

pUsesAnnotation :: (Ord e, Monad m) => Parser e m Uses
pUsesAnnotation = choice
  [ symbol "0"       >> return None
  , symbol "&"       >> return Affi
  , symbol "1"       >> return Once
  ]

-- | Parse the type of types: @Type@
pTyp :: (Ord e, Monad m) => Parser e m Term
pTyp = label "a type: \"Type\"" $ do
  string "Type"
  return $ Typ

pBinder :: (Ord e, Monad m) => Bool -> Parser e m [(Name,Uses, Term)]
pBinder namOptional = choice
  [ try $ ann
  , if namOptional then unNam else empty
  ]
  where
    unNam = (\x -> [("", Many, x)]) <$> pTerm
    ann = do
      symbol "("
      uses  <- pUses
      names <- sepEndBy1 (pName True) space
      typ_  <- symbol ":" >> pExpr False
      string ")"
      return $ (,uses,typ_) <$> names


-- | Parse a hole: @?name@
pHol :: (Ord e, Monad m) => Parser e m Term
pHol = label "a hole: \"?name\"" $ do
  symbol "?"
  name <- pName True
  return (Hol name)

foldLam:: Term -> [Name] -> Term
foldLam body bs = foldr (\n x -> Lam n x) body bs

-- | Parse a lambda: @λ x y z => body@
pLam :: (Ord e, Monad m) => Parser e m Term
pLam = label "a lambda: \"λ x y => y\"" $ do
  symbol "λ" <|> symbol "\\" <|> symbol "lambda"
  vars <- sepEndBy1 (pName True) space
  symbol "=>"
  body <- bind vars (pExpr False)
  return (foldLam body vars)

foldAll :: Term -> [(Name, Uses, Term)] -> Term
foldAll body bs = foldr (\(n,u,t) x -> All n u t x) body bs

bindAll :: (Ord e, Monad m) => [(Name,Uses,Term)] -> Parser e m a -> Parser e m a
bindAll bs = bind (foldr (\(n,_,_) ns -> n:ns) [] bs)

fst3 (x,y,z) = x

-- | Parse a forall: ∀ (a: A) (b: B) (c: C) -> body@
pAll :: (Ord e, Monad m) => Parser e m Term
pAll = label "a forall: \"∀ (a: A) (b: B) -> A\"" $ do
  symbol "∀" <|> symbol "forall"
  binds <- binders <* space
  body  <- bindAll binds (pExpr False)
  return $ foldAll body binds
  where
    binder  = pBinder True
    binders = do
     b  <- binder <* space
     bs <- bindAll b $ ((symbol "->" >> return []) <|> binders)
     return $ b ++ bs

pSlf :: (Ord e, Monad m) => Parser e m Term
pSlf = label "a self type: \"@x A\"" $ do
  name <- symbol "@" >> (pName True) <* space
  body <- bind [name] $ pExpr False
  return $ Slf name body

pNew :: (Ord e, Monad m) => Parser e m Term
pNew = label "datatype introduction: \"data x\"" $ do
  symbol "data "
  expr <- pExpr True
  return $ New expr

pUse :: (Ord e, Monad m) => Parser e m Term
pUse = label "a case expression: \"case x\"" $ do
  symbol "case "
  expr <- pExpr True
  return $ Use expr

pDecl :: (Ord e, Monad m) => Bool -> Parser e m (Name, Term, Term)
pDecl shadow = do
  nam    <- (pName True) <* space
  refs   <- asks _refs
  when (not shadow && Set.member nam refs)
    (customFailure $ TopLevelRedefinition nam)
  bs      <- ((symbol ":" >> return []) <|> binders)
  let ns  = nam:(fst3 <$> bs)
  typBody <- bind ns (pExpr False)
  let typ = foldAll typBody bs
  expBody <- symbol "=" >> bind ns (pExpr False)
  let exp = foldLam expBody (fst3 <$> bs)
  return (nam, exp, typ)
  where
    binder  = pBinder False
    binders = do
     b  <- binder <* space
     bs <- bind (fst3 <$> b) $ ((symbol ":" >> return []) <|> binders)
     return $ b ++ bs

-- | Parse a local, possibly recursive, definition
pLet :: (Ord e, Monad m) => Parser e m Term
pLet = do
  rec <- (symbol "letrec" >> return True) <|> (symbol "let" >> return False)
  use   <- pUses
  (nam,exp,typ) <- pDecl True <* symbol ";"
  bdy <- bind [nam] $ pExpr False
  return $ Let rec nam use typ exp bdy

-- | Parse a local variable or a locally indexed alias of a global reference
pVar :: (Ord e, Monad m) => Parser e m Term
pVar = label "a local or global reference: \"x\", \"add\"" $ do
  env <- ask
  nam <- pName False
  if nam `Set.member` (_context env)
  then return (Var nam)
  else do
    unless (Set.member nam (_refs env))
      (customFailure $ UndefinedReference nam)
    return (Ref nam)

-- | Parse a term
pTerm :: (Ord e, Monad m) => Parser e m Term
pTerm = do
  from <- getOffset
  choice
    [ pLam
    , pAll
    , pSlf
    , pUse
    , pNew
    , pHol
    , pTyp
    , symbol "(" >> pExpr True <* space <* string ")"
    , pLet
    , Opr <$> pOpr
    , LTy <$> pLitType
    , Lit <$> pLiteral
    , pVar
    ]

-- | Parse a sequence of terms as an expression. The `Bool` switches whether or
-- not the sequence must be wrapped in parentheses.
pExpr :: (Ord e, Monad m) => Bool -> Parser e m Term
pExpr annotatable = label "an expression" $ do
  fun  <- pTerm <* space
  args <- args
  let tele = foldl (\t a -> App t a) fun args
  choice
    [ if annotatable then (Ann tele <$> (symbol "::" >> pExpr False)) else empty
    , return tele
    ]
  where
    args = next <|> (return [])
    next = do
      notFollowedBy terminator
      t  <- pTerm <* space
      ts <- args
      return (t:ts)
    terminator = choice
      [ void (string "def")
      , void (string "::")
      , void (string "{|")
      , void eof
      ]

pAnn :: (Ord e, Monad m) => Parser e m Term
pAnn = do
  symbol "("
  val <- pExpr False <* space
  symbol "::"
  typ <- pExpr False <* space
  string ")"
  return $ Ann val typ

pDoc :: (Ord e, Monad m) => Parser e m Text
pDoc = do
  d <- optional (string "{|" >> T.pack <$> (manyTill anySingle (symbol "|}")))
  return $ maybe "" id d

-- | Parse a definition
pDef :: (Ord e, Monad m) => Parser e m (Name, Def)
pDef = label "a definition" $ do
  doc <- pDoc
  symbol "def"
  (nam,exp,typ) <- pDecl False
  return $ (nam, Def doc exp typ)

-- | Parse a sequence of definitions, e.g. in a file
pDefs :: (Ord e, Monad m) => Parser e m [(Name,Def)]
pDefs = (space >> next) <|> (space >> eof >> (return []))
  where
  next = do
    (n,d) <- pDef
    ds    <- local (\e -> e { _refs = Set.insert n (_refs e) }) pDefs
    return $ (n,d):ds

pLiteral :: (Ord e, Monad m) => Parser e m Literal
pLiteral = label "a literal" $ choice
  [ string "#world"     >> return VWorld
  , string "#exception" >> return VException
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

