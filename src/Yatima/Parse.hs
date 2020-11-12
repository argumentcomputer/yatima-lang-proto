module Yatima.Parse where

import Control.Monad.Identity
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Yatima.Parse.Literal
import Yatima.Parse.Package hiding (parseDefault)
import Yatima.Parse.Parser
import Yatima.Parse.Term
import Yatima.Term

-- | Parses a source-code to a Term, simplified API
parseTerm :: Text -> Maybe Term
parseTerm code = case (runIdentity $ parseDefault p code) of
  Left err -> Nothing
  Right trm -> Just trm
  where
    p :: Parser () Identity Term
    p = pExpr True

-- | Parses a source-code to a Term, simplified API, throws
unsafeParseTerm :: Text -> Term
unsafeParseTerm code = case parseTerm code of
  Nothing -> error "Bad parse."
  Just trm -> trm
