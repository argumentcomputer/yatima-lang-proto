{-|
Module      : Language.Yatima.HOAS
Description : Evaluate and typecheck exprassions in the Yatima Language using
higher-order-abstract-syntax
Copyright   : (c) Sunshine Cybernetics, 2020
License     : GPL-3
Maintainer  : john@sunshinecybernetics.com
Stability   : experimental
-}
module Language.Yatima.HOAS
  ( HOAS(..)
  , findCtx
  , toHOAS
  , fromHOAS
  , printHOAS
  , whnf
  , norm
  , evalFile
  , evalPrintFile
  ) where

import           Data.Text             (Text)
import qualified Data.Text             as T

import           Language.Yatima.Parse
import           Language.Yatima.Print
import           Language.Yatima.Term

-- | Higher-Order Abstract Syntax
data HOAS where
  VarH :: Name -> Int -> HOAS
  LamH :: Name -> (HOAS -> HOAS) -> HOAS
  AppH :: HOAS -> HOAS -> HOAS

-- | Find a term in a context
findCtx :: Int -> [HOAS] -> Maybe HOAS
findCtx i cs = go cs 0
  where
    go (c:cs) j
      | i == j   = Just c
      | otherwise = go cs (j+1)
    go [] _     = Nothing

-- | Convert a lower-order `Term` to a GHC higher-order one
toHOAS :: Term -> [HOAS] -> Int -> HOAS
toHOAS t ctx dep = case t of
  Var n i       -> case findCtx i ctx of
    Just trm -> trm
    Nothing  -> VarH n (dep - i - 1)
  Lam n b       -> LamH n  (\x -> bind x b)
  App f a       -> AppH (go f) (go a)
  where
    bind x t = toHOAS t (x:ctx) (dep + 1)
    go t     = toHOAS t ctx dep

-- | Convert a GHC higher-order representation to a lower-order one
fromHOAS :: HOAS -> Int -> Term
fromHOAS t dep = case t of
  VarH n i   -> Var n (dep - i - 1)
  LamH n b   -> Lam n (unbind n b)
  AppH f a   -> App (go f) (go a)
  where
    go t       = fromHOAS t dep
    unbind n b = fromHOAS (b (VarH n dep)) (dep + 1)

-- | Pretty-print a `HOAS`
printHOAS :: HOAS -> Text
printHOAS = prettyTerm . (\x -> fromHOAS x 0)

instance Show HOAS where
  show t = T.unpack $ printHOAS t

-- | Reduce a HOAS to weak-head-normal-form
whnf :: HOAS -> HOAS
whnf t = case t of
  AppH f a       -> case whnf f of
    LamH _ b -> whnf (b a)
    x        -> AppH f a
  x              -> x

-- | Normalize a HOAS term
norm :: HOAS -> HOAS
norm t = case whnf t of
  LamH n b -> LamH n (\ x -> norm (b x))
  AppH f a -> AppH (norm f) (norm a)
  x        -> x

-- | Read and evaluate a `HOAS` from a file
evalFile :: FilePath -> IO HOAS
evalFile file = do
  term <- pFile file
  return $ norm $ toHOAS term [] 0

-- | Read, eval and print a `HOAS` from a file
evalPrintFile :: FilePath -> IO ()
evalPrintFile file = do
  term <- evalFile file
  putStrLn $ T.unpack $ printHOAS term
