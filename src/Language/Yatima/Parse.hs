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

Because the `Parser` type defined here is wrapped in a `RWST` transformer, if
you wish to extend or modify it, you will find the `parserTest` and `parse'`
functions useful for testing and running the parsers defined here:

@
> parserTest (pExpr False) "λ y => (λ x => x) y"
Lam "y" (App (Lam "x" (Var "x" 0)) (Var "y" 0))
@

`parserTest` only prints the output though, if you want to get the resulting
datatype you need to use `parse'`

@
> :t parse' pTerm defaultParseEnv ""
parse' pTerm defaultParseEnv ""
:: Text -> Either (ParseErrorBundle Text ParseErr) Term
@

Here's an example on correct input:

@
> parse' pTerm defaultParseEnv "" "λ x => x" 
Right (Lam "x" (Var "x" 0))
@

And on input that errors:
@
> parse' pTerm defaultParseEnv "" "λ x => y" 
Left (ParseErrorBundle 
(UndefinedReference "y")]) :| [], bundlePosState = PosState {pstateInput =
"\955 x => y", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName =
"", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8,
pstateLinePrefix = ""}})
@

`ParseErrorBundle` is a big illegible blob with a lot of stuff in it, which is why 
we have `parserTest` to prettily print the errror:

@
> parserTest pTerm  "λ x => y" 
1:9:
  |
1 | λ x => y
  |         ^
Undefined reference: y
@

|-}
module Language.Yatima.Parse 
  ( ParseErr(..)
  , Parser
  , parseDefault
  , parserTest
  , parse'
  , pName
  , pLam
  , pVar
  , pTerm
  , pExpr
  , pFile
  , prettyFile
  ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.RWS.Lazy     hiding (All)

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Char                  (isDigit)
import           Data.List                  (intersperse)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.Text.IO               as TIO

import           System.Exit

import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

import           Language.Yatima.Term
import           Language.Yatima.Print


-- | The environment of a Parser
data ParseEnv = ParseEnv
  { -- | The binding context for local variables
    _context :: Set Name
  }

-- | A stub for a future parser state
type ParseState = ()

-- | A stub for a future parser log
type ParseLog = ()

-- | An empty parser environment, useful for testing
defaultParseEnv = ParseEnv Set.empty

-- | Custom parser errrors with bespoke messaging
data ParseErr
  = UndefinedReference Name
  | ReservedKeyword Name
  | LeadingDigit Name
  | LeadingApostrophe Name
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ParseErr where
  showErrorComponent (UndefinedReference nam) =
    "Undefined reference: " ++ T.unpack nam
  showErrorComponent (ReservedKeyword nam) =
    "Reserved keyword: " ++ T.unpack nam
  showErrorComponent (LeadingDigit nam) = 
    "illegal leading digit in name: " ++ T.unpack nam
  showErrorComponent (LeadingApostrophe nam) =
    "illegal leading apostrophe in name: " ++ T.unpack nam

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
type Parser a
  = RWST ParseEnv ParseLog ParseState (ParsecT ParseErr Text Identity) a

-- | A top level parser with default env and state
parseDefault :: Show a => Parser a -> Text
             -> Either (ParseErrorBundle Text ParseErr) a
parseDefault p s = do
  (a,_,_) <- runIdentity $ runParserT (runRWST p defaultParseEnv ()) "" s
  return a

-- | A useful testing function
parserTest :: Show a => Parser a -> Text -> IO ()
parserTest p s = case parseDefault p s of
  Left  e -> putStr (errorBundlePretty e)
  Right x -> print x

-- | A utility for running a `Parser`, since the `RWST` monad wraps `ParsecT`
parse' :: Show a => Parser a -> ParseEnv -> String -> Text
       -> Either (ParseErrorBundle Text ParseErr) a
parse' p env file txt = do
  (a,_,_) <- runIdentity $ runParserT (runRWST p env ()) file txt
  return a

pName :: Parser Text
pName = label "a name: \"someFunc\",\"somFunc'\",\"x_15\", \"_1\"" $ do
  n  <- alphaNumChar <|> oneOf nameSymbol
  ns <- many (alphaNumChar <|> oneOf nameSymbol)
  let nam = T.pack (n : ns)
  if | isDigit n                 -> customFailure $ LeadingDigit nam
     | n == '\''                 -> customFailure $ LeadingApostrophe nam
     | nam `Set.member` keywords -> customFailure $ ReservedKeyword nam
     | otherwise -> return nam
  where
    nameSymbol = "_'" :: [Char]

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
-- with @//@, and block comments are bracketed by @*/@ and @*/@ symbols.
space :: Parser ()
space = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

-- | A symbol is a string which can be followed by whitespace. The @sc@ argument
-- is for parsing indentation sensitive whitespace
symbol :: Text -> Parser Text
symbol txt = L.symbol space txt

-- | Add a list of names to the binding context
bind :: [Name] -> Parser a -> Parser a
bind bs p = local (\e -> e { _context = Set.union (_context e) (Set.fromList bs) }) p

-- | Find a name in the binding context and return its index
find :: Name -> [Name] -> Maybe Int
find n cs = go n cs 0
  where
    go n (c:cs) i
      | n == c    = Just i
      | otherwise = go n cs (i+1)
    go _ [] _     = Nothing

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
    unAnn = (\x -> [(x , Nothing       )]) <$> pName
    ann = do
      symbol "("
      uses  <- pUses
      names <- sepEndBy1 pName space
      typ_  <- symbol ":" >> pExpr False
      string ")"
      return $ (,Just (uses,typ_)) <$> names

foldLam:: Term -> [(Name, Maybe (Uses, Term))] -> Term
foldLam body bs = foldr (\(n,ut) x -> Lam n ut x) body bs

-- | Parse a lambda: @λ (x) (y) (z) => body@
pLam :: Parser Term
pLam = label "a lambda: \"λ (x) (y) => y\"" $ do
  symbol "λ" <|> symbol "lam" <|> symbol "lambda"
  bs   <- binders <* space
  symbol "=>"
  body <- bind (fst <$> bs) (pExpr False)
  return $ foldLam body bs
  where
    binder  = pBinder True False
    binders = do
     b  <- binder <* space
     bs <- bind (fst <$> b) $ ((try binders) <|> return [])
     return $ b ++ bs

foldAll :: Term -> [(Name, Name, Maybe (Uses, Term))] -> Term
foldAll body bs = foldr (\(s,n,Just (u,t)) x -> All s n u t x) body bs

bindAll :: [(Name,Name,Maybe (Uses,Term))] -> Parser a -> Parser a
bindAll bs = bind (foldr (\(s,n,_) ns -> s:n:ns) [] bs)

pAll :: Parser Term
pAll = do
  self <- (try $ symbol "@" >> pName <* space) <|> return ""
  symbol "∀" <|> symbol "all" <|> symbol "forall"
  bs   <- binders self <* space
  symbol "->"
  body <- bindAll bs (pExpr False)
  return $ foldAll body bs
  where
    binder  = pBinder False True

    binders self = do
     ((n,ut):ns)  <- binder <* space
     let b  = ((self,n,ut) : ((\(n,ut) -> ("",n,ut)) <$> ns))
     bs <- bindAll b $ ((try $ binders "") <|> return [])
     return $ b ++ bs

pDecl :: Parser (Name, Term, Term)
pDecl = do
  nam     <- pName <* space
  bs      <- (try binders <|> return []) <* space
  let ns  = nam:(fst <$> bs)
  typBody <- symbol ":" >> bind ns (pExpr False)
  let typ = foldAll typBody ((\(n,ut) -> ("",n,ut)) <$> bs)
  expBody <- symbol "=" >> bind ns (pExpr False)
  let exp = foldLam expBody bs
  return (nam, typ, exp)
  where
    binder  = pBinder False False
    binders = do
     b  <- binder <* space
     bs <- bind (fst <$> b) $ ((try binders) <|> return [])
     return $ b ++ bs

-- | Parse a local, possibly recursive, definition
pLet :: Parser Term
pLet = do
  symbol "let"
  use  <- pUses
  (nam,typ,exp) <- pDecl <* symbol ";"
  bdy <- bind [nam] $ pExpr False
  return $ Let nam use typ exp bdy

-- | Parse a local variable or a locally indexed alias of a global reference
pVar :: Parser Term
pVar = label "a local or global reference: \"x\", \"add\"" $ do
  ctx <- asks _context
  nam <- pName
  case nam `Set.member` ctx of
    True  -> return $ Var nam
    False -> customFailure $ UndefinedReference nam

-- | Parse a term
pTerm :: Parser Term
pTerm = do
  from <- getOffset
  choice
    [ pTyp
    , pLam
    , pAll
    , pLet
    , pExpr True
    , pVar
    ]

-- | Parse a sequence of terms as an expression. The `Bool` switches whether or
-- not the sequence must be wrapped in parentheses.
pExpr :: Bool -> Parser Term
pExpr parens = do
  when parens (void $ symbol "(")
  fun  <- pTerm <* space
  -- need to add `observing` to get the error from the `try $ pTerm`, and/or
  -- need to manually unwrap the sepEndBy
  args <- sepEndBy (try $ pTerm) space
  when parens (void $ string ")")
  return $ foldl (\t a -> App t a) fun args

-- | Parse a file
pFile :: FilePath -> IO Term
pFile file = do
  txt <- TIO.readFile file
  case parse' (pExpr False) defaultParseEnv file txt of
    Left  e -> putStr (errorBundlePretty e) >> exitFailure
    Right m -> return m

-- | Parse and pretty-print a file
prettyFile :: FilePath -> IO ()
prettyFile file = do
  term <- pFile file
  putStrLn $ T.unpack $ prettyTerm term
