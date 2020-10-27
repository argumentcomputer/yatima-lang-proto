module Yatima.Core.IR where

import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Yatima.Print
import           Yatima.Term

data IR where
  RefI :: Name -> IR
  VarI :: Name -> IR
  LamI :: Uses -> Name -> IR -> IR
  AppI :: Uses -> IR -> IR -> IR
  NewI :: IR -> IR
  UseI :: IR -> Maybe LitType -> IR
  LetI :: Bool -> Uses -> Name -> IR -> IR -> IR
  -- Typing constructors
  AllI :: Name -> Uses -> IR -> IR -> IR
  SlfI :: Name -> IR -> IR
  TypI :: IR
  -- Native datatypes
  LitI :: Literal -> IR
  LTyI :: LitType -> IR
  OprI :: PrimOp  -> IR
  deriving (Show, Eq)
