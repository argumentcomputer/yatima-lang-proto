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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}
module Language.Yatima.Parse
  ( ParseErr(..)
  , ParseEnv(..)
  , Parser
  , parseDefault
  , parseM
  , pName
  , pLam
  , pAll
  , pTyp
  , pLet
  , pVar
  , pBinder
  , pDecl
  , pHol
  , pTerm
  , pExpr
  , pDef
  , pDefs
  , pDoc
  , symbol
  , space
  , defaultParseEnv
  , parseTerm
  , unsafeParseTerm
  ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.RWS.Lazy     hiding (All, Typ)

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Char                  (isDigit)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Map                   (Map)
import qualified Data.Map                   as M

import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

import           Language.Yatima.Uses
import           Language.Yatima.Term
import           Language.Yatima.Print

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
  | LeadingDigit Name
  | LeadingApostrophe Name
  | ParseEnvironmentError e
  deriving (Eq, Ord,Show)

instance ShowErrorComponent e => ShowErrorComponent (ParseErr e) where
  showErrorComponent (UndefinedReference nam) =
    "Undefined reference: " ++ T.unpack nam
  showErrorComponent (TopLevelRedefinition nam) =
    "illegal redefinition of " ++ T.unpack nam
  showErrorComponent (ReservedKeyword nam) =
    "Reserved keyword: " ++ T.unpack nam
  showErrorComponent (LeadingDigit nam) = 
    "illegal leading digit in name: " ++ T.unpack nam
  showErrorComponent (LeadingApostrophe nam) =
    "illegal leading apostrophe in name: " ++ T.unpack nam
  showErrorComponent (ParseEnvironmentError e) = showErrorComponent e

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
  , "*"
  ]




-- | Consume whitespace, while skipping comments. Yatima line comments begin
-- with @--@, and block comments are bracketed by @{-@ and @-}@ symbols.
space :: (Ord e, Monad m) => Parser e m ()
space = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | A symbol is a string which can be followed by whitespace. The @sc@ argument
-- is for parsing indentation sensitive whitespace
symbol :: (Ord e, Monad m) => Text -> Parser e m Text
symbol txt = L.symbol space txt

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

-- | Parse an any: @*@
pTyp :: (Ord e, Monad m) => Parser e m Term
pTyp = label "an any: \"*\"" $ do
  string "*"
  return $ Typ

pBinder :: (Ord e, Monad m) => Bool -> Parser e m [(Name,Uses, Term)]
pBinder namOptional = choice
  [ ann
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
  symbol "λ" <|> symbol "lambda" <|> symbol "lam"
  vars <- sepEndBy1 (pName True) space
  symbol "=>"
  body <- bind vars (pExpr False)
  return (foldLam body vars)

foldAll :: Term -> [(Name, Name, Uses, Term)] -> Term
-- TODO foldAll body bs = foldr (\(s,n,u,t) x -> All s n u t x) body bs
foldAll = foldAll

bindAll :: (Ord e, Monad m) => [(Name,Name,Uses,Term)] -> Parser e m a -> Parser e m a
-- TODO bindAll bs = bind (foldr (\(s,n,_,_) ns -> s:n:ns) [] bs)
bindAll = bindAll

fst3 (x,y,z) = x

-- | Parse a forall: @∀ (a: A) (b: B) (c: C) -> body@
pAll :: (Ord e, Monad m) => Parser e m Term
pAll = pAll
-- pAll = label "a forall: \"∀ (a: A) (b: B) -> A\"" $ do
--   self <- (symbol "@" >> (pName True) <* space) <|> return ""
--   symbol "∀" <|> symbol "all" <|> symbol "forall"
--   binds <- binders self <* space
--   body  <- bindAll binds (pExpr False)
--   return $ foldAll body binds
--   where
--     binder  = pBinder True
--     binders self = do
--      ((n,u,t):ns)  <- binder <* space
--      let b  = ((self,n,u,t) : ((\(n,u,t) -> ("",n,u,t)) <$> ns))
--      bs <- bindAll b $ ((symbol "->" >> return []) <|> binders "")
--      return $ b ++ bs

pDecl :: (Ord e, Monad m) => Bool -> Parser e m (Name, Term, Term)
pDecl shadow = do
  nam    <- (pName True) <* space
  refs   <- asks _refs
  when (not shadow && Set.member nam refs)
    (customFailure $ TopLevelRedefinition nam)
  bs      <- ((symbol ":" >> return []) <|> binders)
  let ns  = nam:(fst3 <$> bs)
  typBody <- bind ns (pExpr False)
  let typ = foldAll typBody ((\(n,u,t) -> ("",n,u,t)) <$> bs)
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
  symbol "let"
  use  <- pUses
  (nam,exp,typ) <- pDecl True <* symbol ";"
  bdy <- bind [nam] $ pExpr False
  return $ Let nam use typ exp bdy

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
    , pHol
    , pTyp
    , symbol "(" >> pExpr True <* space <* string ")"
    , pLet
    , pVar
    ]

-- | Parse a sequence of terms as an expression. The `Bool` switches whether or
-- not the sequence must be wrapped in parentheses.
pExpr :: (Ord e, Monad m) => Bool -> Parser e m Term
pExpr annotatable = do
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
