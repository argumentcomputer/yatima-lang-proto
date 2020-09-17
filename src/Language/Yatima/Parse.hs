{-|
Module      : Language.Yatima.Parse
Description : Parsing expressions in the Yatima Language
Copyright   : (c) Sunshine Cybernetics, 2020
License     : AGPL-3
Maintainer  : john@sunshinecybernetics.com
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
module Language.Yatima.Parse
  ( ParseErr(..)
  , ParseEnv(..)
  , Parser
  , parseDefault
  , parseIO
  , pName
  , pLam
  , pVar
  , pTerm
  , pExpr
  , pFile
  --, prettyFile
  , pDef
  , symbol
  , space
  , defaultParseEnv
  ) where

import           Codec.Serialise
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.RWS.Lazy     hiding (All)

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as BSL
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Char                  (isDigit)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text.IO               as TIO

import           System.Exit
import           System.Directory

import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

import           Language.Yatima.Term
import           Language.Yatima.Print
import           Language.Yatima.Defs

-- | The environment of a Parser
data ParseEnv = ParseEnv
  { -- | The binding context for local variables
    _context  :: Set Name
  , _index    :: Map Name CID
  -- , _packs    :: Map Name CID
  }

-- | A stub for a future parser state
type ParseState = ()

-- | A stub for a future parser log
type ParseLog = ()

-- | An empty parser environment, useful for testing
defaultParseEnv = ParseEnv Set.empty M.empty -- M.empty

-- | Custom parser errrors with bespoke messaging
data ParseErr
  = UndefinedReference Name
  | UnknownPackage Name
  | MisnamedPackageImport Name Name CID
  | MisnamedPackageFile   FilePath Name
  | ConflictingImportNames Name CID [Name]
  | TopLevelRedefinition Name
  | ReservedKeyword Name
  | LeadingDigit Name
  | LeadingApostrophe Name
  | CorruptDefs DerefErr
  | InvalidCID String Text
  deriving (Eq, Ord,Show)

instance ShowErrorComponent ParseErr where
  showErrorComponent (UndefinedReference nam) =
    "Undefined reference: " ++ T.unpack nam
  showErrorComponent (UnknownPackage nam) =
    "Unknown package: " ++ T.unpack nam
  showErrorComponent (MisnamedPackageImport a b c) = concat
    [ "Package was imported declaring name ", show a
    , " but the package titles itself " , show b 
    , " at CID ", show c
    ]
  showErrorComponent (MisnamedPackageFile a b) = concat
    [ "Package is declared in file ", show a
    , " but the package titles itself ", show b 
    ]
  showErrorComponent (ConflictingImportNames n cid ns) = concat
    [ "Imported package, ", show n
    , " from CID, ", show cid
    , " which contains the following conflicting names", show ns
    ]
  showErrorComponent (TopLevelRedefinition nam) =
    "illegal redefinition of " ++ T.unpack nam
  showErrorComponent (ReservedKeyword nam) =
    "Reserved keyword: " ++ T.unpack nam
  showErrorComponent (LeadingDigit nam) = 
    "illegal leading digit in name: " ++ T.unpack nam
  showErrorComponent (LeadingApostrophe nam) =
    "illegal leading apostrophe in name: " ++ T.unpack nam
  showErrorComponent (CorruptDefs e) =
    "ERR: The defined environment is corrupt: " ++ show e
  showErrorComponent (InvalidCID err txt) =
    "Inalid CID: " ++ show txt ++ ", " ++ err

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
--
-- Additionally, ParsecT wraps around the `IO` monad in order to be able to
-- resolve package imports.
type Parser a = RWST ParseEnv ParseLog ParseState (ParsecT ParseErr Text IO) a

-- | A top level parser with default env and state
parseDefault :: Show a => Parser a -> Text -> IO a
parseDefault p s = parseIO p defaultParseEnv "" s

-- | A utility for running a `Parser`, since the `RWST` monad wraps `ParsecT`
parseIO :: Show a => Parser a -> ParseEnv -> String -> Text -> IO a
parseIO p env file txt = do
  a <- runParserT (runRWST p env ()) file txt
  case a of
    Left  e       -> putStr (errorBundlePretty e) >> exitFailure
    Right (m,_,_) -> return m

pName :: Bool -> Parser Text
pName bind = label "a name: \"someFunc\",\"somFunc'\",\"x_15\", \"_1\"" $ do
  n  <- alphaNumChar <|> oneOf nameSymbol
  ns <- many (alphaNumChar <|> oneOf nameSymbol)
  let nam = T.pack (n : ns)
  if | isDigit n                 -> customFailure $ LeadingDigit nam
     | n == '\''                 -> customFailure $ LeadingApostrophe nam
     | nam `Set.member` keywords -> customFailure $ ReservedKeyword nam
     | otherwise -> return nam
  where
    nameSymbol = if bind then "_'" else "_'/" :: [Char]

    keywords :: Set Text
    keywords = Set.fromList $
      [ "let"
      , "if"
      , "for"
      , "var"
      , "then"
      , "else"
      , "where"
      , "case"
      , "forall"
      , "all"
      , "lam"
      , "lambda"
      , "def"
      , "define"
      ]


-- | Consume whitespace, while skipping comments. Yatima line comments begin
-- with @--@, and block comments are bracketed by @{-@ and @-}@ symbols.
space :: Parser ()
space = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | A symbol is a string which can be followed by whitespace. The @sc@ argument
-- is for parsing indentation sensitive whitespace
symbol :: Text -> Parser Text
symbol txt = L.symbol space txt

-- | Add a list of names to the binding context
bind :: [Name] -> Parser a -> Parser a
bind bs p =
  local (\e -> e { _context = Set.union (_context e) (Set.fromList bs) }) p

-- | Parse a quantitative usage semirig annotation. The absence of annotation is
-- considered to be the `Many` multiplicity.
pUses ::  Parser Uses
pUses = pUsesAnnotation <|> return Many

pUsesAnnotation :: Parser Uses
pUsesAnnotation = choice
  [ symbol "0"       >> return None
  , symbol "&"       >> return Affi
  , symbol "1"       >> return Once
  ]

-- | Parse the type of types: `Type`
pTyp :: Parser Term
pTyp = do
  string "Type"
  return $ Typ

pBinder :: Bool -> Bool -> Parser [(Name, Maybe (Uses, Term))]
pBinder annOptional namOptional = choice
  [ ann
  , if namOptional then unNam else empty
  , if annOptional then unAnn else empty
  ]
  where
    unNam = (\x -> [("", Just (Many, x))]) <$> pTerm
    unAnn = (\x -> [(x , Nothing       )]) <$> pName True
    ann = do
      symbol "("
      uses  <- pUses
      names <- sepEndBy1 (pName True) space
      typ_  <- symbol ":" >> pExpr False
      string ")"
      return $ (,Just (uses,typ_)) <$> names

foldLam:: Term -> [(Name, Maybe (Uses, Term))] -> Term
foldLam body bs = foldr (\(n,ut) x -> Lam n ut x) body bs

-- | Parse a lambda: @λ x y z => body@
pLam :: Parser Term
pLam = label "a lambda: \"λ x y => y\"" $ do
  symbol "λ" <|> symbol "lam" <|> symbol "lambda"
  bs   <- binders
  body <- bind (fst <$> bs) (pExpr False)
  return $ foldLam body bs
  where
    binder  = pBinder True False
    binders = do
     b  <- binder <* space
     bs <- bind (fst <$> b) $ ((symbol "=> " >> return []) <|> binders)
     return $ b ++ bs

foldAll :: Term -> [(Name, Name, Maybe (Uses, Term))] -> Term
foldAll body bs = foldr (\(s,n,Just (u,t)) x -> All s n u t x) body bs

bindAll :: [(Name,Name,Maybe (Uses,Term))] -> Parser a -> Parser a
bindAll bs = bind (foldr (\(s,n,_) ns -> s:n:ns) [] bs)

pAll :: Parser Term
pAll = do
  self <- (symbol "@" >> (pName True) <* space) <|> return ""
  symbol "∀" <|> symbol "all" <|> symbol "forall"
  bs   <- binders self <* space
  body <- bindAll bs (pExpr False)
  return $ foldAll body bs
  where
    binder  = pBinder False True

    binders self = do
     ((n,ut):ns)  <- binder <* space
     let b  = ((self,n,ut) : ((\(n,ut) -> ("",n,ut)) <$> ns))
     bs <- bindAll b $ ((symbol "->" >> return []) <|> binders "")
     return $ b ++ bs

pDecl :: Bool -> Parser (Name, Term, Term)
pDecl shadow = do
  nam     <- (pName True) <* space
  index   <- asks _index
  when (not shadow && M.member nam index)
    (customFailure $ TopLevelRedefinition nam)
  bs      <- ((symbol ":" >> return []) <|> binders)
  let ns  = nam:(fst <$> bs)
  typBody <- bind ns (pExpr False)
  let typ = foldAll typBody ((\(n,ut) -> ("",n,ut)) <$> bs)
  expBody <- symbol "=" >> bind ns (pExpr False)
  let exp = foldLam expBody bs
  return (nam, exp, typ)
  where
    binder  = pBinder False False
    binders = do
     b  <- binder <* space
     bs <- bind (fst <$> b) $ ((symbol ":" >> return []) <|> binders)
     return $ b ++ bs

-- | Parse a local, possibly recursive, definition
pLet :: Parser Term
pLet = do
  symbol "let"
  use  <- pUses
  (nam,exp,typ) <- pDecl True <* symbol ";"
  bdy <- bind [nam] $ pExpr False
  return $ Let nam use typ exp bdy

-- | Parse a local variable or a locally indexed alias of a global reference
pVar :: Parser Term
pVar = label "a local or global reference: \"x\", \"add\"" $ do
  env <- ask
  nam <- pName False
  case nam `Set.member` (_context env) of
    True  -> return $ Var nam
    False -> case M.lookup nam (_index env) of
      Nothing -> customFailure $ UndefinedReference nam
      Just _  -> return $ Ref nam

-- | Parse a term
pTerm :: Parser Term
pTerm = do
  from <- getOffset
  choice
    [ pTyp
    , pLam
    , pAll
    , pExpr True
    , pLet
    , pVar
    ]

-- | Parse a sequence of terms as an expression. The `Bool` switches whether or
-- not the sequence must be wrapped in parentheses.
pExpr :: Bool -> Parser Term
pExpr parens = do
  when parens (void $ symbol "(")
  fun  <- pTerm <* space
  args <- args
  space
  when parens (void $ string ")")
  return $ foldl (\t a -> App t a) fun args
  where 
    args = next <|> (return [])
    next :: Parser [Term]
    next = do
      notFollowedBy (void (symbol "def") <|> (void (symbol "{|")) <|> eof)
      t  <- pTerm <* space
      ts <- args
      return (t:ts)

pDoc :: Parser Text
pDoc = do
  d <- optional (string "{|" >> T.pack <$> (manyTill anySingle (symbol "|}")))
  return $ maybe "" id d

-- | Parse a definition
pDef :: Parser Def
pDef = label "a definition" $ do
  doc <- pDoc
  symbol "def"
  (nam,exp,typ) <- pDecl False
  return $ Def nam "" exp typ

pInsertDef :: Def -> Parser Index
pInsertDef def = do
  index <- asks _index
  cache <- liftIO $ readCache
  case runExcept (insertDef def index cache) of
    Left  e -> customFailure $ CorruptDefs e
    Right (cid,cac) -> do
      liftIO $ writeCache cac
      return $ M.insert (_name def) cid index

-- | Parse a sequence of definitions, e.g. in a file
pDefs :: Parser Index
pDefs = (space >> next) <|> (space >> eof >> asks _index)
  where
  next = do
    def  <- pDef
    ind <- pInsertDef def
    local (\e -> e { _index = ind }) pDefs

pCid :: Parser CID
pCid = do
  space
  txt <- T.pack <$> (many alphaNumChar)
  case cidFromText txt of
    Left  err  -> customFailure $ InvalidCID err txt
    Right cid  -> return $ cid

pPackageName :: Parser Text
pPackageName = label "a package name" $ do
  n  <- letterChar
  ns <- many (alphaNumChar <|> oneOf ("-" :: [Char]))
  let nam = T.pack (n : ns)
  return nam

data Import
  = IPFS  Name (Maybe Name) CID
  | Local Name (Maybe Name)

pDeserial :: forall a. Serialise a => CID -> Parser a
pDeserial cid = do
  cache <- liftIO $ readCache
  case runExcept (deserial @a cid cache) of
    Left  e -> customFailure $ CorruptDefs $ e
    Right a -> return a

pImport :: Parser (Name,CID,Index)
pImport = label "an import" $ do
  symbol "with"
  nam <- pPackageName
  ali <- optional $ try $ (space >> symbol "as" >> pName False)
  imp <- choice
    [ try $ (space >> symbol "from" >> IPFS nam ali <$> pCid)
    , return $ Local nam ali
    ]
  case imp of
    IPFS nam ali cid -> do
      p <- pDeserial @Package cid
      unless (nam == (_title p))
        (customFailure $ MisnamedPackageImport nam (_title p) cid)
      let pre = maybe "" (\x -> T.append x "/") ali
      return (nam,cid, M.mapKeys (T.append pre) (_packInd p))
    Local nam ali -> do
      (cid,p) <- liftIO $ pFile "" (T.unpack nam ++ ".ya")
      unless (nam == (_title p))
        (customFailure $ MisnamedPackageImport nam (_title p) cid)
      let pre = maybe "" (\x -> T.append x "/") ali
      return (nam,cid, M.mapKeys (T.append pre) (_packInd p))

pImports :: Parser ([CID], Index)
pImports = next <|> (([],) <$> asks _index)
  where
  next = do
    (nam,cid,imp) <- pImport <* space
    index <- asks _index
    let conflict = M.keys $ M.intersection imp index
    unless (conflict == [])
      (customFailure $ ConflictingImportNames nam cid conflict)
    (cs,ind) <- local (\e -> e { _index = M.union imp index }) pImports
    return (cid:cs, ind)

pPackage :: Parser Package
pPackage = do
  space
  doc     <- maybe "" id <$> (optional $ pDoc)
  title   <- symbol "package" >> pPackageName <* space
  (imports, index) <- pImports <* space
  sn <- sourceName <$> getSourcePos
  when (title /= "" && ((T.unpack title) ++ ".ya") /= sn)
    (customFailure $ MisnamedPackageFile sn title)
  when (title /= "") (void $ symbol "where")
  index <- local (\e -> e { _index = index }) pDefs
  return $ Package title doc (Set.fromList imports) index

-- | Parse a file
pFile :: FilePath -> FilePath -> IO (CID,Package)
pFile root file = do
  unless (root == "") (setCurrentDirectory root)
  createDirectoryIfMissing True ".yatima/cache"
  txt   <- TIO.readFile file
  pack  <- parseIO pPackage defaultParseEnv file txt
  cache <- readCache
  let (cid,cache') = insertPackage pack cache
  writeCache cache'
  return (cid,pack)

-- | Parse and pretty-print a file
--prettyFile :: FilePath -> IO ()
--prettyFile file = do
--  defs <- pFile file
--  case prettyDefs defs of
--    Left e  -> putStrLn $ show e
--    Right t -> putStrLn $ T.unpack t
