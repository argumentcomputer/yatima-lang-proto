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
findCtx :: Name -> [(Name,HOAS)] -> Maybe HOAS
findCtx n cs = go cs
  where
    go ((m,c):cs)
      | n == m   = Just c
      | otherwise = go cs
    go []        = Nothing

-- | Convert a lower-order `Term` to a GHC higher-order one
toHOAS :: Term -> [(Name,HOAS)] -> HOAS
toHOAS t ctx = case t of
  Var n         -> case findCtx n ctx of
    Just trm -> trm
    Nothing  -> VarH n 0
  Lam n b       -> LamH n  (\x -> bind (n,x) b)
  App f a       -> AppH (go f) (go a)
  where
    bind n t = toHOAS t (n:ctx)
    go t     = toHOAS t ctx

-- | Convert a GHC higher-order representation to a lower-order one
fromHOAS :: HOAS -> Term
fromHOAS t = case t of
  VarH n _   -> Var n
  LamH n b   -> Lam n (unbind n b)
  AppH f a   -> App (go f) (go a)
  where
    go t       = fromHOAS t
    unbind n b = fromHOAS (b (VarH n 0))

-- | Pretty-print a `HOAS`
printHOAS :: HOAS -> Text
printHOAS = prettyTerm . (\x -> fromHOAS x)

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
  return $ norm $ toHOAS term []

-- | Read, eval and print a `HOAS` from a file
evalPrintFile :: FilePath -> IO ()
evalPrintFile file = do
  term <- evalFile file
  putStrLn $ T.unpack $ printHOAS term
