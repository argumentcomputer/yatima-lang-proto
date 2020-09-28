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
  ( Name(..)
  , Term(..)
  ) where

import           Data.Text (Text)
import qualified Data.Text as T hiding (find)
import Language.Yatima.Uses

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
  -- | The type of types
  Any :: Term

deriving instance Show Term


