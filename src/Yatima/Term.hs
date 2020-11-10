{-|
Module      : Yatima.Term
Description : Defines expressions in the Yatima language
Copyright   : 2020 Yatima Inc.
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental

This module defines `Term`, the type of expressions in the Yatima language.

-}

{-# LANGUAGE DeriveDataTypeable #-}
module Yatima.Term
  ( module Yatima.Term.Uses
  , module Yatima.Term.Literal
  , module Yatima.Term.PrimOp
  , Name
  , Term (..)
  , Def  (..)
  , Defs
  , findByName
  , findByInt
  , shift
  ) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Data
import           Data.IPLD.CID

import           Yatima.Term.Uses
import           Yatima.Term.PrimOp
import           Yatima.Term.Literal

-- * Yatima expressions

-- | An abstract name used in Yatima term for parsing and printing
type Name = Text

-- | A Yatima term with names and source locations
data Term where
  -- | Local variable
  Var :: Name -> Int -> Term
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
  -- | An immutable global reference to a named definition and anonymous term
  Ref :: Name -> CID -> CID -> Term
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
  { _defTitle :: Text
  , _doc      :: Text
  , _term     :: Term
  , _type     :: Term
  } deriving (Show,Eq)

type Defs = Map CID Def

-- | Find a name in a binding context and return its index
findByName :: Name -> [Name] -> Maybe Int
findByName n cs = go n cs 0
  where
    go n (c:cs) i
      | n == c    = Just i
      | otherwise = go n cs (i+1)
    go _ [] _     = Nothing

findByInt :: Int -> [Name] -> Maybe Name
findByInt i []     = Nothing
findByInt i (x:xs)
  | i < 0     = Nothing
  | i == 0    = Just x
  | otherwise = findByInt (i - 1) xs

shift :: Int -> Int -> Term -> Term
shift inc dep term = case term of
  Var n i         -> Var n (if i < dep then i else (i + inc))
  All n u t b     -> All n u (go t) (bind b)
  Lam n b         -> Lam n (bind b)
  App f a         -> App (go f) (go a)
  Slf n b         -> Slf n (bind b)
  New x           -> New (go x)
  Use x           -> Use (go x)
  Ann t x         -> Ann (go t) (go x)
  Let r n u t x b -> Let r n u (go t) (bind x) (bind b)
  x               -> x
  where
    go x   = shift inc dep x
    bind x = shift inc (dep+1) x



