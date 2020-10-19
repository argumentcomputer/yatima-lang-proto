{-|
Module      : Language.Yatima.Term
Description : Defines expressions in the Yatima language
Copyright   : (c) Sunshine Cybernetics, 2020
License     : GPL-3
Maintainer  : contact@sunshinecybernetics.com
Stability   : experimental

This module defines `Term`, the type of expressions in the Yatima language.

-}
module Language.Yatima.Term
  ( -- | IPFS content-identifiers
    module Language.Yatima.Uses
  , Name(..)
  , Term(..)
  , Def(..)
  , Defs
  , Constant(..)
  ) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T hiding (find)
import           Data.Map                   (Map)
import qualified Data.Map                   as M

import           Language.Yatima.Uses
import           Language.Yatima.Constant

-- * Yatima expressions

-- | An abstract name used in Yatima term for parsing and printing
type Name = Text

-- | A Yatima term with names and source locations
data Term where
  -- | A hole
  Hol :: Name -> Term
  -- | Local variable
  Var :: Name -> Term
  -- | A forall
  All :: Name -> Uses -> Term -> Term -> Term
  -- | A lambda
  Lam :: Name -> Term -> Term
  -- | An application of a function to an argument
  App :: Term -> Term -> Term
  -- | Self type
  Slf :: Name -> Term -> Term
  -- | Self introduction
  New :: Term -> Term
  -- | Self elimination
  Use :: Term -> Term
  -- | An reference
  Ref :: Name -> Term
  -- | An inline local definition,
  Let :: Name -> Uses -> Term -> Term -> Term -> Term
  -- | The type of types.
  Typ :: Term
  -- | Type annotation
  Ann :: Term -> Term -> Term
  -- | Literal
  Lit :: Constant -> Term

deriving instance Show Term
deriving instance Eq Term

-- | A type annotated definition
data Def = Def
  { _doc  :: Text
  , _term :: Term
  , _type :: Term
  } deriving Show

type Defs = Map Name Def

