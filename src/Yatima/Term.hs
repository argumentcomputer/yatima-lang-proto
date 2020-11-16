{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Yatima.Term
-- Description : Defines expressions in the Yatima language
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
--
-- This module defines `Term`, the type of expressions in the Yatima language.
module Yatima.Term
  ( module Yatima.Term.Uses,
    module Yatima.Term.Literal,
    module Yatima.Term.PrimOp,
    Name,
    Term (..),
    Def (..),
    Defs,
    Pos (..),
    Loc (..),
    findByName,
    findByInt,
    shift,
    setTermLoc,
    termLoc,
    termNoLoc,
    _Var,
    _All,
    _Lam,
    _App,
    _Slf,
    _New,
    _Use,
    _Ref,
    _Let,
    _Typ,
    _Ann,
    _Lit,
    _LTy,
    _Opr,
  )
where

import Data.Data
import Data.IPLD.Cid
import Data.Map (Map)
import Data.Text (Text)
import Yatima.Term.Literal
import Yatima.Term.PrimOp
import Yatima.Term.Uses

-- * Yatima expressions

-- | An name used in a Yatima term for parsing and printing. Yatima terms are
-- name-irrelevant, meaning @Name@ does not affect computational semantics in
-- any way.
type Name = Text

-- | A source position representing the offset of a constructor in a file
data Pos = Pos Int Int deriving (Eq, Ord, Data)

instance Show Pos where
  show (Pos line col) = show line ++ ":" ++ show col

-- | A range of source locations in a specific file
data Loc
  = Loc {_from :: Pos, _upto :: Pos}
  | NoLoc
  deriving (Show, Eq, Ord, Data)

-- | A Yatima term with names and source locations
data Term where
  -- | Local variable
  Var :: Loc -> Name -> Int -> Term
  -- | A forall
  All :: Loc -> Name -> Uses -> Term -> Term -> Term
  -- | A lambda
  Lam :: Loc -> Name -> Term -> Term
  -- | An application of a function to an argument
  App :: Loc -> Term -> Term -> Term
  -- | Self type
  Slf :: Loc -> Name -> Term -> Term
  -- | Self introduction
  New :: Loc -> Term -> Term
  -- | Self elimination
  Use :: Loc -> Term -> Term
  -- | An immutable global reference to a named definition and anonymous term
  Ref :: Loc -> Name -> Cid -> Cid -> Term
  -- | An inline local definition,
  Let :: Loc -> Bool -> Name -> Uses -> Term -> Term -> Term -> Term
  -- | The type of types.
  Typ :: Loc -> Term
  -- | Type annotation
  Ann :: Loc -> Term -> Term -> Term
  -- | a primitive literal
  Lit :: Loc -> Literal -> Term
  -- | the type of a primitive literal
  LTy :: Loc -> LitType -> Term
  -- | a primitive operation
  Opr :: Loc -> PrimOp -> Term

instance Eq Term where
  (==) (Var _ n i) (Var _ n' i') = n == n' && i == i'
  (==) (All _ n u t b) (All _ n' u' t' b') =
    and [n == n', u == u', t == t', b == b']
  (==) (Lam _ n b) (Lam _ n' b') = n == n' && b == b'
  (==) (App _ f a) (App _ f' a') = f == f' && a == a'
  (==) (Slf _ n b) (Slf _ n' b') = n == n' && b == b'
  (==) (New _ x) (New _ x') = x == x'
  (==) (Use _ x) (Use _ x') = x == x'
  (==) (Ref _ n d t) (Ref _ n' d' t') = n == n' && d == d' && t == t'
  (==) (Let _ r n u t x b) (Let _ r' n' u' t' x' b') =
    and [r == r', n == n', u == u', t == t', x == x', b == b']
  (==) (Typ _) (Typ _) = True
  (==) (Ann _ t x) (Ann _ t' x') = t == t' && x == x'
  (==) (Lit _ v) (Lit _ v') = v == v'
  (==) (LTy _ t) (LTy _ t') = t == t'
  (==) (Opr _ o) (Opr _ o') = o == o'
  (==) _ _ = False

deriving instance Show Term

deriving instance Data Term

-- | A type annotated definition
data Def = Def
  { _defLoc :: Loc,
    _defTitle :: Text,
    _doc :: Text,
    _term :: Term,
    _type :: Term
  }
  deriving (Eq, Show)

type Defs = Map Cid Def

-- | Find a name in a binding context and return its index
findByName :: Name -> [Name] -> Maybe Int
findByName n cs = go n cs 0
  where
    go n (c : cs) i
      | n == c = Just i
      | otherwise = go n cs (i + 1)
    go _ [] _ = Nothing

findByInt :: Int -> [Name] -> Maybe Name
findByInt _ [] = Nothing
findByInt i (x : xs)
  | i < 0 = Nothing
  | i == 0 = Just x
  | otherwise = findByInt (i - 1) xs

shift :: Int -> Int -> Term -> Term
shift inc dep term = case term of
  Var p n i -> Var p n (if i < dep then i else (i + inc))
  All p n u t b -> All p n u (go t) (bind b)
  Lam p n b -> Lam p n (bind b)
  App p f a -> App p (go f) (go a)
  Slf p n b -> Slf p n (bind b)
  New p x -> New p (go x)
  Use p x -> Use p (go x)
  Ann p t x -> Ann p (go t) (go x)
  Let p r n u t x b -> Let p r n u (go t) (bind x) (bind b)
  x -> x
  where
    go x = shift inc dep x
    bind x = shift inc (dep + 1) x

setTermLoc :: Loc -> Term -> Term
setTermLoc p t = case t of
  Var _ n i -> Var p n i
  All _ n u t b -> All p n u (go t) (go b)
  Lam _ n b -> Lam p n (go b)
  App _ f a -> App p (go f) (go a)
  Slf _ n b -> Slf p n (go b)
  New _ x -> New p (go x)
  Use _ x -> Use p (go x)
  Ann _ t x -> Ann p (go t) (go x)
  Let _ r n u t x b -> Let p r n u (go t) (go x) (go b)
  LTy _ t -> LTy p t
  Lit _ v -> Lit p v
  Opr _ o -> Opr p o
  Ref _ n d t -> Ref p n d t
  where
    go = setTermLoc p

termLoc :: Term -> Loc
termLoc t = case t of
  Var l _ _ -> l
  All l _ _ _ _ -> l
  Lam l _ _ -> l
  App l _ _ -> l
  Slf l _ _ -> l
  New l _ -> l
  Use l _ -> l
  Ann l _ _ -> l
  Let l _ _ _ _ _ _ -> l
  LTy l _ -> l
  Lit l _ -> l
  Opr l _ -> l
  Ref l _ _ _ -> l

termNoLoc :: Term -> Term
termNoLoc = setTermLoc NoLoc

_Var :: Name -> Int -> Term
_Var = Var NoLoc

_All :: Name -> Uses -> Term -> Term -> Term
_All = All NoLoc

_Lam :: Name -> Term -> Term
_Lam = Lam NoLoc

_App :: Term -> Term -> Term
_App = App NoLoc

_Slf :: Name -> Term -> Term
_Slf = Slf NoLoc

_New :: Term -> Term
_New = New NoLoc

_Use :: Term -> Term
_Use = Use NoLoc

_Ref :: Name -> Cid -> Cid -> Term
_Ref = Ref NoLoc

_Let :: Bool -> Name -> Uses -> Term -> Term -> Term -> Term
_Let = Let NoLoc

_Typ :: Term
_Typ = Typ NoLoc

_Ann :: Term -> Term -> Term
_Ann = Ann NoLoc

_Lit :: Literal -> Term
_Lit = Lit NoLoc

_LTy :: LitType -> Term
_LTy = LTy NoLoc

_Opr :: PrimOp -> Term
_Opr = Opr NoLoc
