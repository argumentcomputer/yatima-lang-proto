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
    Name(..)
  , Term(..)
  , Def(..)
  ) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T hiding (find)

-- | An abstract name used in Yatima term for parsing and printing
type Name = Text

-- | A Yatima term with names and source locations
data Term where
  -- | Local variable implemented via De Bruijn indices
  Var :: Name -> Int -> Term
  -- | A lambda
  Lam :: Name -> Term -> Term
  -- | An application of a function to an argument
  App :: Term -> Term -> Term
  -- | An local reference to a term in the def cache
  Ref :: Name -> Term

deriving instance Show Term

-- | A type annotated definition
data Def = Def
  { _name :: Name
  , _term :: Term
  , _type :: Term
  } deriving Show
