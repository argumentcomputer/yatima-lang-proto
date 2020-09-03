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

-}
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
    _context :: [Name]
  }

-- | A stub for a future parser state
type ParseState = ()

-- | A stub for a future parser log
type ParseLog = ()

-- | An empty parser environment, useful for testing
defaultParseEnv = ParseEnv []

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
  if | isDigit n                -> customFailure $ LeadingDigit nam
     | n == '\''                -> customFailure $ LeadingApostrophe nam
     | nam `elem` reservedWords -> customFailure $ ReservedKeyword nam
     | otherwise -> return nam
  where
    nameSymbol = "_'" :: [Char]

    reservedWords :: [Text]
    reservedWords = [ "let"
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
bind bs p = local (\e -> e { _context = (reverse bs) ++ _context e }) p

-- | Find a name in the binding context and return its index
find :: Name -> [Name] -> Maybe Int
find n cs = go n cs 0
  where
    go n (c:cs) i
      | n == c    = Just i
      | otherwise = go n cs (i+1)
    go _ [] _     = Nothing

foldLam:: Term -> [Name] -> Term
foldLam body bs = foldr (\n x -> Lam n x) body bs

-- | Parse a lambda: @λ (x) (y) (z) => body@
pLam :: Parser Term
pLam = label "a lambda: \"λ (x) (y) => y\"" $ do
  symbol "λ" <|> symbol "lam" <|> symbol "lambda"
  bs   <- pBinder <* space
  symbol "=>"
  body <- bind bs (pExpr False)
  return $ foldLam body bs

---- | Parse an untyped binding sequence @x y z@ within a lambda
pBinder :: Parser [Name]
pBinder = label "a single binder in lambda" $ do
  --symbol "("
  names <- sepEndBy1 pName space
  --string ")"
  return names

-- TODO: Use this when adding types to lambdas (if desired in future)
--pBinders :: Parser [Name]
--pBinders = label "a binding sequence in a lambda" $ do
--  (try $ next) <|> pBinder
--  where
--   next = do
--     b  <- pBinder <* space
--     bs <- bind b $ pBinders
--     return $ b ++ bs

-- | Parse a local variable or a locally indexed alias of a global reference
pVar :: Parser Term
pVar = label "a local or global reference: \"x\", \"add\"" $ do
  ctx <- asks _context
  nam <- pName
  case find nam ctx of
    Just i  -> return $ Var nam i
    Nothing -> customFailure $ UndefinedReference nam

-- | Parse a term
pTerm :: Parser Term
pTerm = do
  choice
    [ pLam
    , pExpr True
    , pVar
    ]

-- | Parse a sequence of terms as an expression. The `Bool` switches whether or
-- not the sequence must be wrapped in parentheses.
pExpr :: Bool -> Parser Term
pExpr parens = do
  when parens (void $ symbol "(")
  fun  <- pTerm <* space
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
