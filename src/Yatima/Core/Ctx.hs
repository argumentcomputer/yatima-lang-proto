-- |
-- Module      : Yatima.Core.Ctx
-- Description : Defines a named sequence datatype used as a context for
-- typechecking.
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
module Yatima.Core.Ctx where

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Yatima.Term

-- | A generalized context
newtype Ctx a = Ctx {_ctx :: Seq (Name, a)}

instance Functor Ctx where
  fmap f (Ctx seq) = Ctx $ fmap (\(n, a) -> (n, f a)) seq

instance Show a => Show (Ctx a) where
  show ctx = show (_ctx ctx)

depth :: Ctx a -> Int
depth = length . _ctx

(<|) :: (Name, a) -> Ctx a -> Ctx a
(<|) x (Ctx ctx) = Ctx $ x Seq.<| ctx

infixr 5 <|

zipWith :: (a -> b -> c) -> Ctx a -> Ctx b -> Ctx c
zipWith f (Ctx a) (Ctx b) = Ctx $ Seq.zipWith (\(n, x) (_, y) -> (n, f x y)) a b

singleton :: (Name, a) -> Ctx a
singleton a = Ctx (Seq.singleton a)

empty :: Ctx a
empty = Ctx (Seq.empty)

find :: Name -> Ctx a -> Maybe a
find nam (Ctx ((n, a) :<| cs))
  | n == nam = Just a
  | otherwise = find nam (Ctx cs)
find nam (Ctx Empty) = Nothing

-- | Modifies the context at a single place and returns the old value
adjust :: Int -> Ctx a -> (a -> a) -> Maybe (a, Ctx a)
adjust lvl (Ctx Empty) f = Nothing
adjust lvl (Ctx (ctx :|> (name, a))) f
  | lvl == 0 = return $ (a, Ctx (ctx :|> (name, f a)))
  | otherwise = do
    (a', Ctx ctx') <- adjust (lvl - 1) (Ctx ctx) f
    return (a', Ctx (ctx' :|> (name, a)))
