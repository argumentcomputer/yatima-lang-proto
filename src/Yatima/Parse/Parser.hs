module Yatima.Parse.Parser where

import           Control.Monad.RWS.Lazy     hiding (All, Typ)

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.IPLD.CID

import           Numeric.Natural

import           Text.Megaparsec            hiding (State, ParseError)
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

import           Yatima.Term

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
  RWST ParseEnv ParseLog ParseState (ParsecT (ParseError e) Text m) a

parseDefault :: (Ord e, Monad m) => Parser e m a -> Text
             -> m (Either (ParseErrorBundle Text (ParseError e)) a)
parseDefault p s = parseM p defaultParseEnv "" s

-- | A utility for running a `Parser`, since the `RWST` monad wraps `ParsecT`
parseM :: (Ord e, Monad m) => Parser e m a -> ParseEnv -> String -> Text
       -> m (Either (ParseErrorBundle Text (ParseError e)) a)
parseM p env file txt =
  (fmap (\(x,y,z) -> x)) <$> runParserT (runRWST p env ()) file txt

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
bind bs p = local (\e -> e { _context = (reverse bs) ++ (_context e) }) p

-- | The environment of a Parser
data ParseEnv = ParseEnv
  { -- | The binding context for local variables
    _context    :: [Name]
    -- | The global context for references
  , _refs       :: Map Name (CID,CID)
  }

-- | A stub for a future parser state
type ParseState = ()

-- | A stub for a future parser log
type ParseLog = ()

-- | An empty parser environment, useful for testing
defaultParseEnv = ParseEnv [] M.empty

-- | Custom parser errrors with bespoke messaging
data ParseError e
  = UndefinedReference Name
  | TopLevelRedefinition Name
  | ReservedKeyword Name
  | ReservedLeadingChar Char Name
  | BitVectorOverflow Natural Natural
  | I64Overflow Integer
  | I32Overflow Integer
  | I64Underflow Integer
  | I32Underflow Integer
  | U64Overflow Integer
  | U32Overflow Integer
  | U64Underflow Integer
  | U32Underflow Integer
  | InvalidCID Text Text
  | LeadingDigit Name
  | ParseEnvironmentError e
  | BaseNeedsLength
  deriving (Eq, Ord,Show)

instance ShowErrorComponent e => ShowErrorComponent (ParseError e) where
  showErrorComponent e = case e of
    UndefinedReference nam    -> "Undefined reference: " <> T.unpack nam
    TopLevelRedefinition nam  -> "Illegal redefinition of " <> T.unpack nam
    ReservedKeyword nam       -> "Reserved keyword: " <> T.unpack nam
    LeadingDigit nam          ->
      "Illegal leading digit in name: " <> T.unpack nam
    ReservedLeadingChar c nam ->
      "Illegal leading character " <> show c <> " in name: " <> T.unpack nam
    BitVectorOverflow i l     -> "Overflow: "  <> show i <> " exceeded length of " <> show l
    I64Overflow  i            -> "Overflow: "  <> show i <> "i64"
    I64Underflow i            -> "Underflow: " <> show i <> "i64"
    I32Overflow  i            -> "Overflow: "  <> show i <> "i32"
    I32Underflow i            -> "Underflow: " <> show i <> "i32"
    U64Overflow  i            -> "Overflow: "  <> show i <> "u64"
    U64Underflow i            -> "Underflow: " <> show i <> "u64"
    U32Overflow  i            -> "Overflow: "  <> show i <> "u32"
    U32Underflow i            -> "Underflow: " <> show i <> "u32"
    InvalidCID err txt        -> "Invalid CID: " <> show txt <> ", " <> show err
    ParseEnvironmentError e   -> showErrorComponent e
    BaseNeedsLength           -> "Cannot infer bit vector length. Please set an explicit length between '#' and the base code."

instance ShowErrorComponent () where
  showErrorComponent () = "()"
