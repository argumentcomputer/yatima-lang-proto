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
--  , findCtx
--  , toHOAS
--  , fromHOAS
--  , printHOAS
--  , whnf
--  , norm
--  , evalFile
--  , evalPrintFile
  ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.ST
import           Control.Monad.ST.UnsafePerform

import           Data.Sequence (Seq, ViewR ((:>), EmptyR), viewr, (|>))
import qualified Data.Sequence as Seq
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.STRef

import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Language.Yatima.Defs
import           Language.Yatima.Parse
import           Language.Yatima.Print
import           Language.Yatima.Term

-- | Lower-Order Abstract Syntax
data LOAS where
  VarL :: Name -> Int  -> LOAS
  RefL :: Name -> CID  -> LOAS
  LamL :: Name -> Maybe (Uses, LOAS) -> LOAS -> LOAS
  AppL :: LOAS -> LOAS -> LOAS
  LetL :: Name -> Uses -> LOAS -> LOAS -> LOAS -> LOAS
  AllL :: Name -> Name -> Uses -> LOAS -> LOAS -> LOAS
  TypL :: LOAS

toLOAS :: Term -> [Name] -> Defs -> Except DerefErr LOAS
toLOAS t ctx ds = case t of
  Var n       -> case find n ctx of
    Just i -> return $ VarL n i
    _      -> throwError $ FreeVariable n ctx
  Ref n       -> RefL n <$> anonymizeRef n ds
  Lam n ut b  -> case ut of
    Just (u,t) -> go t >>= \t -> LamL n (Just (u,t)) <$> bind n b
    Nothing    -> LamL n Nothing <$> bind n b
  App f a       -> AppL <$> go f <*> go a
  Let n u t x b -> LetL n u <$> go t <*> bind n x <*> bind n b
  Typ           -> return TypL
  All s n u t b -> AllL s n u <$> go t <*> bind2 s n b
  where
    go t        = toLOAS t ctx ds
    bind    n t = toLOAS t (n:ctx) ds
    bind2 s n t = toLOAS t (n:s:ctx) ds

-- | Convert a GHC higher-order representation to a lower-order one
fromLOAS :: LOAS -> Term
fromLOAS t = case t of
  VarL n _               -> Var n
  RefL n _               -> Ref n
  LamL n (Just (u,t)) b  -> Lam n (Just (u,go t)) (go b)
  LamL n Nothing      b  -> Lam n Nothing (go b)
  AppL f a               -> App (go f) (go a)
  LetL n u t x b         -> Let n u (go t) (go x) (go b)
  AllL s n u t b         -> All s n u (go t) (go b)
  TypL                   -> Typ
  where
    go = fromLOAS

-- | Higher-Order Abstract Syntax
data HOAS where
  VarH :: Name -> Int -> HOAS
  RefH :: Name -> CID -> HOAS
  LamH :: Name -> Maybe (Uses,HOAS) -> (HOAS -> HOAS) -> HOAS
  AppH :: HOAS -> HOAS -> HOAS
  LetH :: Name -> Uses -> HOAS -> HOAS -> (HOAS -> HOAS) -> HOAS
  AllH :: Name -> Name -> Uses -> HOAS -> (HOAS -> HOAS -> HOAS) -> HOAS
  TypH :: HOAS
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
toHOAS :: LOAS -> [HOAS] -> Int -> HOAS
toHOAS t ctx dep = case t of
  VarL n i       -> case findCtx i ctx of
    Just trm -> trm
    Nothing  -> VarH n (dep - i - 1)
  RefL n c       -> RefH n c
  LamL n ut b    -> case ut of
    Just (u,t) -> LamH n (Just (u, go t)) (\x -> bind x b)
    _          -> LamH n Nothing (\x -> bind x b)
  AppL f a       -> AppH (go f) (go a)
  LetL n u t d b -> LetH n u (go t) (FixH n (\x -> bind x d)) (\x -> bind x b)
  AllL s n u t b -> AllH s n u (go t) (\s x -> bind2 s x b)
  TypL           -> TypH
  where
    go t        = toHOAS t ctx       dep
    bind n t    = toHOAS t (n:ctx)   (dep + 1)
    bind2 s n t = toHOAS t (n:s:ctx) (dep + 2)

-- | Convert a GHC higher-order representation to a lower-order one
fromHOAS :: HOAS -> Int -> LOAS
fromHOAS t dep = case t of
  VarH n i       -> VarL n i
  LamH n ut b    -> case ut of
    Just (u,t) -> LamL n (Just (u,go t)) (unbind n b)
    _          -> LamL n Nothing (unbind n b)
  AppH f a       -> AppL (go f) (go a)
  RefH n c       -> RefL n c
  LetH n u t x b -> LetL n u (go t) (go x) (unbind n b)
  AllH s n u t b -> AllL s n u (go t) (unbind2 s n b)
  TypH           -> TypL
  FixH n b       -> unbind n b
  where
    go t          = fromHOAS t dep
    unbind n b    = fromHOAS (b (VarH n dep)) (dep + 1)
    unbind2 s n b = fromHOAS (b (VarH s dep) (VarH n (dep+1))) (dep + 2)

termFromHOAS :: HOAS -> Term
termFromHOAS t = fromLOAS $ fromHOAS t 0

anonymizeHOAS:: HOAS -> Int -> Anon
anonymizeHOAS h dep = case h of
  VarH n i       -> VarA i
  LamH n ut b    -> case ut of
    Just (u,t) -> LamA (Just (u,go t)) (unbind n b)
    _          -> LamA Nothing (unbind n b)
  AppH f a       -> AppA (go f) (go a)
  RefH n c       -> RefA c
  LetH n u t x b -> LetA u (go t) (go x) (unbind n b)
  AllH s n u t b -> AllA u (go t) (unbind2 s n b)
  TypH           -> TypA
  FixH n b       -> unbind n b
  where
    go t          = anonymizeHOAS t dep
    unbind n b    = anonymizeHOAS (b (VarH n dep)) (dep + 1)
    unbind2 s n b = anonymizeHOAS (b (VarH s dep) (VarH n (dep+1))) (dep + 2)

defToHOAS :: Name -> Term -> Defs -> Except DerefErr HOAS
defToHOAS name term ds = do
  loas <- toLOAS term [name] ds
  return $ FixH name (\s -> toHOAS loas [s] 1)

-- | Pretty-print a `HOAS`
printHOAS :: HOAS -> Text
printHOAS = prettyTerm . termFromHOAS

instance Show HOAS where
  show t = T.unpack $ printHOAS t

derefHOAS :: Name -> CID -> Defs -> Except DerefErr HOAS
derefHOAS name cid ds = do
  term <- deref name cid ds
  defToHOAS name term ds

-- * Evaluation

-- | Reduce a HOAS to weak-head-normal-form
whnf :: HOAS -> Defs -> HOAS
whnf t ds = case t of
  FixH n b       -> go (b (FixH n b))
  RefH n c       -> case runExcept (derefHOAS n c ds) of
    Right t  -> go t
    Left e   -> error $ "BAD: Undefined Reference during reduction: " ++ show e
  AppH f a  -> case go f of
    LamH _ _ b -> go (b a)
    x          -> AppH f a
  LetH n u t d b -> go (b d)
  x              -> x
  where
    go x = whnf x ds

hash :: HOAS -> Int -> CID
hash term dep = makeCID $ anonymizeHOAS term dep

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
      let termHash = hash term 0
      let stepHash = hash step 0
      seenSet <- readSTRef seen
      if | termHash `Set.member` seenSet -> return step
         | stepHash `Set.member` seenSet -> return step
         | otherwise -> do
             modifySTRef' seen ((Set.insert termHash) . (Set.insert stepHash))
             next step seen

    next :: HOAS -> (STRef s (Set CID)) -> ST s HOAS
    next step seen = case step of
      LamH n (Just (u,t)) b   -> do
        t' <- Just . (u,) <$> go t seen
        return $ LamH n t' (\x -> unsafePerformST $ go (b x) seen)
      AllH s n u t b   -> do
        t' <- go t seen
        return $ AllH s n u t' (\s x -> unsafePerformST $ go (b s x) seen)
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
  return $ norm def defs

---- | Read, eval and print a `HOAS` from a file
----evalPrintFile :: FilePath -> IO ()
----evalPrintFile file = do
----  term <- evalFile file
----  putStrLn $ T.unpack $ printHOAS term

-- * Type-checking

equal :: HOAS -> HOAS -> Defs -> Int -> Bool
equal a b defs dep = runST $ top a b dep
  where
    top :: HOAS -> HOAS -> Int -> ST s Bool
    top a b dep = do
      seen <- newSTRef (Set.empty)
      go a b dep seen

    go :: HOAS -> HOAS -> Int -> STRef s (Set (CID,CID)) -> ST s Bool
    go a b dep seen = do
      let a1 = whnf a defs
      let b1 = whnf b defs
      let ah = makeCID $ anonymizeHOAS a1 0
      let bh = makeCID $ anonymizeHOAS b1 0
      s' <- readSTRef seen
      if | (ah == bh)              -> return True
         | (ah,bh) `Set.member` s' -> return True
         | (bh,ah) `Set.member` s' -> return True
         | otherwise -> do
             modifySTRef' seen ((Set.insert (ah,bh)) . (Set.insert (bh,ah)))
             next a1 b1 dep seen

    next :: HOAS -> HOAS -> Int -> STRef s (Set (CID,CID)) -> ST s Bool
    next a b dep seen = case (a,b) of
     (AllH as an au at ab, AllH bs bn bu bt bb) -> do
       let a1_body = ab (VarH as dep) (VarH an (dep + 1))
       let b1_body = bb (VarH bs dep) (VarH bn (dep + 1))
       let rig_eq  = au == bu
       bind_eq <- go at bt dep seen
       body_eq <- go a1_body b1_body (dep+2) seen
       return $ rig_eq && bind_eq && body_eq
     (LamH an (Just (au,at)) ab, LamH bn (Just (bu,bt)) bb) -> do
       let a1_body = ab (VarH an dep)
       let b1_body = bb (VarH bn dep)
       let rig_eq  = au == bu
       bind_eq <- go at bt dep seen
       body_eq <- go a1_body b1_body (dep+1) seen
       return $ rig_eq && bind_eq && body_eq
     (LamH an Nothing ab, LamH bn Nothing bb) -> do
       let a1_body = ab (VarH an dep)
       let b1_body = bb (VarH bn dep)
       body_eq <- go a1_body b1_body (dep+1) seen
       return $ body_eq
     (AppH af aa, AppH bf ba) -> do
       func_eq <- go af bf dep seen
       argm_eq <- go aa ba dep seen
       return $ func_eq && argm_eq
     (LetH _ au at ax ab, LetH _ bu bt bx bb) -> do
       let a1_body = ab ax
       let b1_body = bb bx
       let rig_eq  = au == bu
       bind_eq <- go at bt dep seen
       expr_eq <- go ax bx dep seen
       body_eq <- go a1_body b1_body (dep+1) seen
       return $ rig_eq && bind_eq && expr_eq && body_eq
     (FixH _ ab, FixH _ bb) -> go (ab a) (bb b) (dep+1) seen
     _ -> return False

type Ctx = Seq (Uses,HOAS)

multiplyCtx :: Uses -> Ctx -> Ctx
multiplyCtx rho ctx = fmap mul ctx
  where mul (pi, typ) = (rho *# pi, typ)

-- Assumes both context are compatible (different only by quantities)
addCtx :: Ctx -> Ctx -> Ctx
addCtx ctx ctx' = Seq.zipWith add ctx ctx'
  where add (pi, typ) (pi', _) = (pi +# pi', typ)

data CheckErr
  = QuantityMismatch Ctx Uses Uses
  | TypeMismatch Ctx HOAS HOAS
  | EmptyContext Ctx
  | DerefError Ctx Name CID DerefErr
  | LambdaNonFunctionType  Ctx HOAS HOAS
  | NonFunctionApplication Ctx HOAS HOAS
  | CustomErr Ctx Text
  deriving Show

check :: Ctx -> Uses -> HOAS -> HOAS -> Defs -> Except CheckErr Ctx
check ctx ρ trm typ defs = case trm of
  LamH name ut termBody -> case whnf typ defs of
    AllH _ _ π bind typeBody -> do
      maybe (pure ()) (\(φ,bind') -> do
        unless (π == φ) (throwError (QuantityMismatch ctx π φ))
        unless (equal bind bind' defs (length ctx))
          (throwError (TypeMismatch ctx bind bind'))
        pure ()) ut
      let var = VarH name (length ctx)
      let ctx' = (ctx |> (None,bind))
      ctx' <- check ctx' Once (termBody var) (typeBody trm var) defs
      case viewr ctx' of
        EmptyR -> throwError $ EmptyContext ctx
        ctx :> (π', _) -> do
          unless (π' ≤# π) (throwError (QuantityMismatch ctx π' π))
          return $ multiplyCtx ρ ctx
    x -> throwError $ LambdaNonFunctionType ctx trm x
  LetH name π exprType expr body -> do
    check ctx π expr exprType defs
    let var = VarH name (length ctx)
    let ctx' = ctx |> (None, exprType)
    ctx' <- check ctx' Once (body var) typ defs
    case viewr ctx' of
      EmptyR -> throwError $ EmptyContext ctx
      ctx :> (π', _) -> do
        unless (π' ≤# π) (throwError (QuantityMismatch ctx π' π))
        return $ multiplyCtx ρ (addCtx ctx ctx')
  FixH n b -> check ctx ρ (b trm) typ defs
  _ -> do
    (ctx, infr) <- infer ctx ρ trm defs
    if equal typ infr defs (length ctx)
      then return ctx
      else throwError (TypeMismatch ctx typ infr)

infer :: Ctx -> Uses -> HOAS -> Defs -> Except CheckErr (Ctx, HOAS)
infer ctx ρ term defs = case term of
  VarH n idx -> do
    let (_, typ) = Seq.index ctx idx
    let ctx' = Seq.update idx (ρ, typ) ctx
    return (ctx', typ)
  RefH n c -> do
    let mapE = mapExcept (either (\e -> throwError $ DerefError ctx n c e) pure)
    def <- mapE (deref n c defs)
    trm <- mapE (defToHOAS n def defs)
    return (ctx,trm)
  AppH func argm -> do
    (ctx, funcType) <- infer ctx ρ func defs
    case whnf funcType defs of
      AllH _ _ π bind body -> do
        ctx' <- check ctx (ρ *# π) argm bind defs
        return (addCtx ctx ctx', body func argm)
      x -> throwError $ NonFunctionApplication ctx func x
  AllH self name pi bind body -> do
    let self_var = VarH self $ length ctx
    let name_var = VarH name $ length ctx + 1
    let ctx'     = ctx |> (None, bind)
    check ctx  None bind (TypH) defs
    check ctx' None (body self_var name_var) (TypH) defs
    return (ctx, TypH)
  LetH name π exprType expr body -> do
    check ctx π expr exprType defs
    let exprVar = VarH name (length ctx)
    let ctx' = ctx |> (None, exprType)
    (ctx', typ) <- infer ctx' Once (body exprVar) defs
    case viewr ctx' of
      EmptyR                -> throwError (EmptyContext ctx)
      ctx' :> (π', _) -> do
        unless (π' ≤# π) (throwError (QuantityMismatch ctx π' π))
        return (multiplyCtx ρ (addCtx ctx ctx'), typ)
  TypH -> return (ctx, TypH)
  _ -> throwError $ CustomErr ctx "can't infer type"
