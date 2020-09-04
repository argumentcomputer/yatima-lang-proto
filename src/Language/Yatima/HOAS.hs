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
--  , fromHOAS
--  , printHOAS
--  , whnf
--  , norm
--  , evalFile
--  , evalPrintFile
  ) where

import           Data.Text             (Text)
import qualified Data.Text             as T

import           Language.Yatima.Parse
import           Language.Yatima.Print
import           Language.Yatima.Term

-- | Higher-Order Abstract Syntax
data HOAS where
  VarH :: Name -> Int -> HOAS
  LamH :: Name -> Maybe (Uses,HOAS) -> (HOAS -> HOAS) -> HOAS
  AppH :: HOAS -> HOAS -> HOAS
  LetH :: Name -> Uses -> HOAS -> HOAS -> (HOAS -> HOAS) -> HOAS
  AllH :: Name -> Name -> Uses -> HOAS -> (HOAS -> HOAS -> HOAS) -> HOAS
  TypH :: HOAS
  FixH :: Name -> (HOAS -> HOAS) -> HOAS


-- | Find a term in a context
findCtx :: Name -> [(Name,HOAS)] -> Maybe HOAS
findCtx n cs = go cs
  where
    go ((m,c):cs)
      | n == m   = Just c
      | otherwise = go cs
    go []        = Nothing

-- | Convert a lower-order `Term` to a higher-order representation
toHOAS :: Term -> [(Name,HOAS)] -> Int -> HOAS
toHOAS t ctx dep = case t of
  Var n          -> case findCtx n ctx of
    Just trm -> trm
    Nothing  -> VarH n 0
  Lam n ut b     -> case ut of
    Just (u,t) -> LamH n (Just (u,go t)) (\x -> bind (n,x) b)
    Nothing    -> LamH n Nothing         (\x -> bind (n,x) b)
  App f a        -> AppH (go f) (go a)
  Typ            -> TypH
  All s n u t b  -> AllH s n u (go t) (\w x -> (bind2 (n,x) (s,w) b))
  Let n u t d b  -> LetH n u (go t) (FixH n (\x -> bind (n,x) d)) (\x -> bind (n,x) b)
  where
    bind  n t   = toHOAS t (n:ctx)   (dep + 1)
    bind2 n s t = toHOAS t (n:s:ctx) (dep + 2)
    go t        = toHOAS t ctx dep

-- | Convert a higher-order representation to a lower-order one
fromHOAS :: HOAS -> Int -> Term
fromHOAS t dep = case t of
  VarH n i              -> Var n
  LamH n (Just (u,t)) b -> Lam n (Just (u,go t)) (unbind n b)
  LamH n Nothing      b -> Lam n Nothing (unbind n b)
  AppH f a              -> App (go f) (go a)
  TypH                  -> Typ
  AllH s n u t b        -> All s n u (go t) (unbind2 s n b)
  LetH n u t d b        -> Let n u (go t) (go d) (unbind n b)
  FixH n b              -> go (b (FixH n b))
  where
    go t          = fromHOAS t dep
    unbind n b    = fromHOAS (b (VarH n dep)) (dep + 1)
    unbind2 s n b = fromHOAS (b (VarH s dep) (VarH n (dep+1))) (dep + 2)

-- | Pretty-print a `HOAS`
printHOAS :: HOAS -> Text
printHOAS = prettyTerm . (\x -> fromHOAS x 0)

instance Show HOAS where
  show t = T.unpack $ printHOAS t

-- | Reduce a HOAS to weak-head-normal-form
whnf :: HOAS -> HOAS
whnf t = case t of
  FixH n b       -> go (b (FixH n b))
  LetH n u t d b -> go (b d)
  AppH f a       -> case go f of
    LamH _ _ b -> go (b a)
    x          -> AppH f a
  x              -> x
  where
    go = whnf

-- | Normalize a HOAS term
norm :: HOAS -> HOAS
norm t = case whnf t of
  LamH n ut b    -> LamH n ut (\ x -> go (b x))
  AppH f a       -> AppH (go f) (go a)
  AllH s n u t b -> AllH s n u (norm t) (\s x -> go (b s x))
  x              -> x
  where
    go = norm

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
