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
  --, findCtx
  , toHOAS
  , fromHOAS
  , LOAS(..)
  , toLOAS
  , fromLOAS
  , printHOAS
  , whnf
  , norm
  , catchDerefErr
--  , evalFile
--  , evalPrintFile
  ) where

import           Control.Monad.ST
import           Control.Monad.Identity
import           Control.Monad.ST.UnsafePerform
import           Control.Monad.Except

import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.STRef

import           Data.Text             (Text)
import qualified Data.Text             as T

import           Language.Yatima.Parse
import           Language.Yatima.Print
import           Language.Yatima.Term
import           Language.Yatima.Defs

-- | Lower-Order Abstract Syntax
data LOAS where
  VarL :: Name -> Int  -> LOAS
  RefL :: Name -> CID  -> LOAS
  LamL :: Name -> LOAS -> LOAS
  AppL :: LOAS -> LOAS -> LOAS

deriving instance Show LOAS

toLOAS :: Term -> [Name] -> Defs -> Except DerefErr LOAS
toLOAS t ctx ds = go t ctx
  where
    go t ctx = case t of
      Var n       -> case find n ctx of
        Just i -> return $ VarL n i
        _      -> throwError $ FreeVariable n ctx
      Ref n       -> RefL n <$> anonymizeRef n ds
      Lam n b     -> LamL n <$> go b (n:ctx)
      App f a     -> AppL <$> go f ctx <*> go a ctx

-- | Convert a GHC higher-order representation to a lower-order one
fromLOAS :: LOAS -> Term
fromLOAS t = case t of
  VarL n _   -> Var n
  RefL n _   -> Ref n
  LamL n b   -> Lam n (go b)
  AppL f a   -> App (go f) (go a)
  where
    go = fromLOAS

-- | Higher-Order Abstract Syntax
data HOAS where
  VarH :: Name -> Int -> HOAS
  RefH :: Name -> CID -> HOAS
  LamH :: Name -> (HOAS -> HOAS) -> HOAS
  AppH :: HOAS -> HOAS -> HOAS
  FixH :: Name -> (HOAS -> HOAS) -> HOAS

-- | Find a term in a context
findCtx :: Int -> [HOAS] -> Maybe HOAS
findCtx i cs = go cs 0
  where
    go (c:cs) j
      | i == j   = Just c
      | otherwise = go cs (j+1)
    go [] _      = Nothing

-- | Convert a lower-order `Term` to a GHC higher-order one
toHOAS :: LOAS -> [HOAS] -> HOAS
toHOAS t ctx = case t of
  VarL n i       -> case findCtx i ctx of
    Just trm -> trm
    Nothing  -> VarH n 0
  RefL n c       -> RefH n c
  LamL n b       -> LamH n (\x -> bind x b)
  AppL f a       -> AppH (go f) (go a)
  where
    bind n t = toHOAS t (n:ctx)
    go t     = toHOAS t ctx

-- | Convert a GHC higher-order representation to a lower-order one
fromHOAS :: HOAS -> LOAS
fromHOAS t = case t of
  VarH n i   -> VarL n i
  LamH n b   -> LamL n (unbind n b)
  RefH n h   -> RefL n h
  AppH f a   -> AppL (go f) (go a)
  FixH n b   -> go (b (FixH n b))
  where
    go t       = fromHOAS t
    unbind n b = fromHOAS (b (VarH n 0))

termFromHOAS :: HOAS -> Term
termFromHOAS = fromLOAS . fromHOAS

anonymizeHOAS:: HOAS -> Anon
anonymizeHOAS h = case h of
  VarH _ i   -> VarA i
  RefH _ c   -> RefA c
  LamH n b   -> LamA (unbind n b)
  AppH f a   -> AppA (go f) (go a)
  FixH n b   -> go (b (FixH n b))
  where
    go t       = anonymizeHOAS t
    unbind n b = anonymizeHOAS (b (VarH n 0))

defToHOAS :: Name -> Term -> Defs -> Except DerefErr HOAS
defToHOAS name term ds = do
  loas <- toLOAS term [name] ds
  return $ FixH name (\s -> toHOAS loas [s])

-- | Pretty-print a `HOAS`
printHOAS :: HOAS -> Text
printHOAS = prettyTerm . termFromHOAS

instance Show HOAS where
  show t = T.unpack $ printHOAS t

derefHOAS :: Name -> CID -> Defs -> Except DerefErr HOAS
derefHOAS name cid ds = do
  term <- deref name cid ds
  defToHOAS name term ds

-- | Reduce a HOAS to weak-head-normal-form
whnf :: HOAS -> Defs -> HOAS
whnf t ds = case t of
  FixH n b     -> go (b (FixH n b))
  RefH n c     -> case runExcept (derefHOAS n c ds) of
    Right t    -> go t
    Left e     -> error $ "BAD: Undefined Reference during reduction: " ++ show e
  LamH n b     -> LamH n b
  AppH f a     -> case go f of
    LamH _ b -> go (b a)
    x        -> AppH f a
  x            -> x
  where
    go x = whnf x ds

hash :: HOAS -> CID
hash term = makeCID $ anonymizeHOAS term

-- | Normalize a HOAS term
norm :: HOAS -> Defs -> HOAS
norm term defs = runST (top $ term)
  where
    top :: HOAS -> ST s HOAS
    top term = do
      seen <- newSTRef (Set.empty)
      go term seen

    go :: HOAS -> (STRef s (Set CID)) -> ST s HOAS
    go term seen = do
      let step = whnf term defs
      let termHash = hash term
      let stepHash = hash step
      seenSet <- readSTRef seen
      if | termHash `Set.member` seenSet -> return step
         | stepHash `Set.member` seenSet -> return step
         | otherwise -> do
             modifySTRef' seen ((Set.insert termHash) . (Set.insert stepHash))
             case step of
               LamH n b -> return $ LamH n (\x -> unsafePerformST (go (b x) seen))
               AppH f a -> AppH <$> (go f seen) <*> (go a seen)
               _        -> return step

catchDerefErr :: Except DerefErr a -> IO a
catchDerefErr x = do
  case runExcept x of
    Right x -> return x
    Left  e -> error $ "Runtime DerefErr: " ++ show e

-- | Read and evaluate a `HOAS` from a file
readDef :: Name -> FilePath -> IO HOAS
readDef name file = do
  defs  <- pFile file
  cid   <- catchDerefErr (indexLookup name defs)
  def   <- catchDerefErr (derefHOAS name cid defs)
  return $ def

normDef :: Name -> FilePath -> IO HOAS
normDef name file = do
  defs  <- pFile file
  cid   <- catchDerefErr (indexLookup name defs)
  def   <- catchDerefErr (derefHOAS name cid defs)
  return $ whnf def defs

-- | Read, eval and print a `HOAS` from a file
--evalPrintFile :: FilePath -> IO ()
--evalPrintFile file = do
--  term <- evalFile file
--  putStrLn $ T.unpack $ printHOAS term
