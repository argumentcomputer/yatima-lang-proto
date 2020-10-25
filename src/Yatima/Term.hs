{-|
Module      : Yatima.Term
Description : Defines expressions in the Yatima language
Copyright   : (c) Sunshine Cybernetics, 2020
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental

This module defines `Term`, the type of expressions in the Yatima language.

-}

{-# LANGUAGE DeriveDataTypeable #-}
module Yatima.Term
  ( module Yatima.Uses
  , module Yatima.Literal
  , module Yatima.PrimOp
  , Name
  , Term (..)
  , Def  (..)
  , Defs 
  ) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T hiding (find)
import           Data.Map                   (Map)
import qualified Data.Map                   as M

import           Data.Data

import           Yatima.Uses
import           Yatima.PrimOp
import           Yatima.Literal

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
  Let :: Bool -> Name -> Uses -> Term -> Term -> Term -> Term
  -- | The type of types.
  Typ :: Term
  -- | Type annotation
  Ann :: Term -> Term -> Term
  -- | a primitive literal
  Lit :: Literal -> Term
  -- | the type of a primitive literal
  LTy :: LitType -> Term
  -- | a primitive operation
  Opr :: PrimOp -> Term

deriving instance Show Term
deriving instance Eq Term
deriving instance Data Term

-- | A type annotated definition
data Def = Def
  { _doc    :: Text
  , _term   :: Term
  , _type   :: Term
  } deriving Show

type Defs = Map Name Def

