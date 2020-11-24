{-# LANGUAGE DerivingVia #-}

-- |
-- Module      : Yatima.Core
-- Description : Evaluate and typecheck expressions in the Yatima Language using
-- higher-order-abstract-syntax
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
module Yatima.Core where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.ST
import Data.IPLD.Cid
import Data.List (foldl')
import qualified Data.Map as M
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Yatima.Core.CheckError
import Yatima.Core.Ctx (Ctx (..), (<|))
import qualified Yatima.Core.Ctx as Ctx
import Yatima.Core.Hoas
import Yatima.Core.IR
import Yatima.Core.Prim
import Yatima.Core.UnionFind
import Yatima.IPLD
import Yatima.Term

whnf :: Defs -> Hoas -> Hoas
whnf defs trm = go trm []
  where
    go :: Hoas -> [(Loc, Hoas)] -> Hoas
    go trm args = case trm of
      RefH _ nam cid _ -> case defs M.!? cid of
        Just d -> go (fst (defToHoas nam d)) args
        Nothing -> apply trm args
      FixH _ _ bod -> go (bod trm) args
      AppH l fun arg -> go fun ((l, arg) : args)
      LamH _ _ bod -> case args of
        [] -> trm
        ((_, a) : args') -> go (bod a) args'
      OprH l opr -> reduceOpr l opr ((\(_, arg) -> go arg []) <$> args)
      UseH _ arg -> case go arg [] of
        NewH _ exp -> go exp args
        LitH l' val -> maybe (apply trm args) (\t -> go t args) (expandLit l' val)
        _ -> foldl' (\t (l, a) -> AppH l t a) trm args
      AnnH _ a _ -> go a args
      UnrH _ _ _ a _ -> go a args
      LetH _ _ _ _ exp bod -> go (bod exp) args
      _ -> apply trm args
    apply = foldl' (\t (l, a) -> AppH l t a)

norm :: Defs -> Hoas -> Hoas
norm defs term = go term 0 Set.empty
  where
    go :: Hoas -> Int -> Set Cid -> Hoas
    go term lvl seen =
      let step = whnf defs term
          hash = makeCid $ termToAST $ hoasToTerm lvl term
          hash' = makeCid $ termToAST $ hoasToTerm lvl step
       in if
              | hash `Set.member` seen -> step
              | hash' `Set.member` seen -> step
              | otherwise -> next step lvl (Set.insert hash' (Set.insert hash seen))

    next :: Hoas -> Int -> Set Cid -> Hoas
    next step lvl seen = case step of
      AllH l nam use typ bod ->
        AllH l nam use (go typ lvl seen) (\x -> go (bod x) (lvl + 1) seen)
      LamH l nam bod -> LamH l nam (\x -> go (bod x) (lvl + 1) seen)
      AppH l fun arg -> go (AppH l (go fun lvl seen) (go arg lvl seen)) lvl seen
      FixH l nam bod -> go (bod (FixH l nam bod)) lvl seen
      SlfH l nam bod -> SlfH l nam (\x -> go (bod x) (lvl + 1) seen)
      NewH l exp -> NewH l (go exp lvl seen)
      UseH l exp -> UseH l (go exp lvl seen)
      step -> step

-- Equality
-- ========
congruent :: Equiv s -> Term -> Term -> ST s Bool
congruent eq a b = do
  let getHash = makeCid . termToAST
  t <- equivalent eq (getHash a) (getHash b)
  if t
    then return True
    else do
      let go = congruent eq
      case (a, b) of
        (All _ _ u h b, All _ _ u' h' b') -> pure (u == u') &&* go h h' &&* go b b'
        (Lam _ _ b, Lam _ _ b') -> go b b'
        (App _ f a, App _ f' a') -> go f f' &&* go a a'
        (Let _ r _ u _ x b, Let _ r' _ u' _ x' b') -> pure (r == r' && u == u') &&* go x x' &&* go b b'
        (Ann _ x _, Ann _ x' _) -> go x x'
        (Slf _ _ x, Slf _ _ x') -> go x x'
        (New _ x, New _ x') -> go x x'
        (Use _ x, Use _ x') -> go x x'
        _ -> return False
  where
    (&&*) = liftA2 (&&)

equal :: Defs -> Hoas -> Hoas -> Int -> Bool
equal defs term1 term2 dep = runST $ do
  eq <- newEquiv
  go eq (Seq.singleton (term1, term2, dep))
  where
    go :: Equiv s -> Seq (Hoas, Hoas, Int) -> ST s Bool
    go _ Seq.Empty = return True
    go eq ((t1, t2, dep) :<| tris) = do
      let term1 = whnf defs t1
      let term2 = whnf defs t2
      let hash1 = getHash term1
      let hash2 = getHash term2
      equate eq (getHash t1) hash1
      equate eq (getHash t2) hash2
      b <- congruent eq (hoasToTerm dep term1) (hoasToTerm dep term2)
      equate eq hash1 hash2
      if b
        then go eq tris
        else case (term1, term2) of
          (AppH _ f a, AppH _ f' a') ->
            go eq $ tris >< Seq.fromList [(f, f', dep + 1), (a, a', dep + 1)]
          (LamH l n b, LamH l' n' b') ->
            go eq $ tris >< Seq.singleton (b (VarH l n dep), b' (VarH l' n' dep), dep + 1)
          (AllH l n u h b, AllH l' n' u' h' b') ->
            go eq $ tris >< Seq.fromList [(h, h', dep + 1), (b (VarH l n dep), b' (VarH l' n' dep), dep + 1)]
          (SlfH l n b, SlfH l' n' b') ->
            go eq $ tris >< Seq.singleton (b (VarH l n dep), b' (VarH l' n' dep), dep + 1)
          (NewH _ b, NewH _ b') ->
            go eq $ tris >< Seq.singleton (b, b', dep + 1)
          (UseH _ b, UseH _ b') ->
            go eq $ tris >< Seq.singleton (b, b', dep + 1)
          _ ->
            return False
    getHash = makeCid . termToAST . hoasToTerm dep

-- * Type System

check ::
  Maybe Text ->
  Defs ->
  PreContext ->
  Uses ->
  Hoas ->
  Hoas ->
  Except CheckError (Context, Hoas, IR)
check file defs pre use term typ = case term of
  LamH l name body -> case whnf defs typ of
    AllH l bindName bindUse bind typeBody -> do
      let bodyType = typeBody (VarH l name (Ctx.depth pre))
      let bodyTerm = body (VarH l name (Ctx.depth pre))
      (bodyCtx, _, bodyIR) <- check file defs ((name, bind) <| pre) Once bodyTerm bodyType
      case _ctx bodyCtx of
        Empty -> throwError $ EmptyContext file l
        ((name', (bindUse', bind')) :<| bodyCtx') -> do
          unless
            (bindUse' ≤# bindUse)
            ( do
                let original = (name, bindUse, bind)
                let checked = (name', bindUse', bind')
                throwError (CheckQuantityMismatch (Ctx bodyCtx') original checked)
            )
          let ir = LamI bindUse name bodyIR
          return (mulCtx use (Ctx bodyCtx'), typ, ir)
    x -> throwError $ LambdaNonFunctionType file l pre term typ x
  NewH l expr -> case whnf defs typ of
    SlfH l slfName slfBody -> do
      (exprCtx, exprTyp, exprIR) <- check file defs pre use expr (slfBody term)
      return (exprCtx, exprTyp, NewI exprIR)
    x -> throwError $ NewNonSelfType file l pre term typ x
  LetH l name exprUse exprTyp expr body -> do
    (exprCtx, _, exprIR) <- check file defs pre exprUse expr exprTyp
    let var = VarH l name (Ctx.depth pre)
    (bodyCtx, _, bodyIR) <- check file defs ((name, exprTyp) <| pre) Once (body var) typ
    case _ctx bodyCtx of
      Empty -> throwError $ EmptyContext file l
      ((name', (exprUse', exprTyp')) :<| bodyCtx') -> do
        unless
          (exprUse' ≤# exprUse)
          ( do
              let original = (name, exprUse, exprTyp)
              let checked = (name', exprUse', exprTyp')
              throwError (CheckQuantityMismatch file l (Ctx bodyCtx') original checked)
          )
        let isFix = case expr of
              FixH _ _ _ -> True
              _ -> False
        let ir = LetI isFix exprUse name exprIR bodyIR
        return (mulCtx use (addCtx exprCtx (Ctx bodyCtx')), typ, ir)
  FixH l name body -> do
    let unroll = body (UnrH l name (Ctx.depth pre) (FixH l name body) typ)
    (bodyCtx, _, bodyIR) <- check file defs ((name, typ) <| pre) use unroll typ
    case _ctx bodyCtx of
      Empty -> throwError $ EmptyContext file l
      (_, (None, _)) :<| bodyCtx' -> return (Ctx bodyCtx', typ, bodyIR)
      (_, (use, _)) :<| bodyCtx' -> return (mulCtx Many (Ctx bodyCtx'), typ, bodyIR)
  x -> do
    (ctx, termTyp, termIR) <- infer file defs pre use term
    case equal defs typ termTyp (Ctx.depth pre) of
      False -> throwError (TypeMismatch file (hoasLoc x) pre typ termTyp)
      True -> return (ctx, typ, termIR)

-- | Infers the type of a term
infer ::
  Maybe Text ->
  Defs ->
  PreContext ->
  Uses ->
  Hoas ->
  Except CheckError (Context, Hoas, IR)
infer file defs pre use term = case term of
  VarH l nam lvl -> do
    let ir = VarI nam
    case Ctx.adjust lvl (toContext pre) (\(_, typ) -> (use, typ)) of
      Nothing -> throwError $ UnboundVariable file l nam lvl
      Just ((_, typ), ctx) -> return (ctx, typ, ir)
<<<<<<< HEAD
  RefH l nam cid _ -> do
    --traceM ("RefH " ++ show nam)
    let mapMaybe = maybe (throwError $ UndefinedReference file l nam) pure
=======
  RefH nam cid _ -> do
    let mapMaybe = maybe (throwError $ UndefinedReference nam) pure
>>>>>>> 7cdc8594c569c69ef1e63818c1ed2e4520bdaec3
    def <- mapMaybe (defs M.!? cid)
    let (_, typ) = (defToHoas nam def)
    let ir = RefI nam
    return (toContext pre, typ, ir)
  LamH l name body -> throwError $ UntypedLambda file l
  AppH l func argm -> do
    (funcCtx, funcTyp, funcIR) <- infer file defs pre use func
    case whnf defs funcTyp of
      AllH l _ argmUse bind body -> do
        (argmCtx, _, argmIR) <- check file defs pre (argmUse *# use) argm bind
        let ir = AppI argmUse funcIR argmIR
        return (addCtx funcCtx argmCtx, body argm, ir)
      x -> throwError $ NonFunctionApplication file l funcCtx func funcTyp x
  UseH l expr -> do
    (exprCtx, exprTyp, exprIR) <- infer file defs pre use expr
    case whnf defs exprTyp of
      SlfH l _ body -> do
        return (exprCtx, body expr, UseI exprIR Nothing)
      LTyH l typ -> do
        when (typ /= TNatural && typ /= TString) $
          throwError $ UseOnNonInductiveType file l exprCtx expr typ
        return (exprCtx, litInduction l typ expr, UseI exprIR (Just typ))
      AppH l (LTyH _ TBitVector) (LitH _ (VNatural n)) -> do
        let expr' = AppH l (litInduction l TBitVector (LitH l (VNatural n))) expr
        return (exprCtx, expr', UseI exprIR (Just TBitVector))
      -- TODO: Make sure this is right
      x -> throwError $ NonSelfUse file l exprCtx expr exprTyp x
  AllH l name bindUse bind body -> do
    let nameVar = VarH l name $ Ctx.depth pre
    (_, _, bindIR) <- check file defs pre None bind (TypH l)
    (_, _, bodyIR) <- check file defs ((name, bind) <| pre) None (body nameVar) (TypH l)
    let ir = AllI name bindUse bindIR bodyIR
    return (toContext pre, TypH l, ir)
  SlfH l name body -> do
    let selfVar = VarH l name $ Ctx.depth pre
    (_, _, bodyIR) <- check file defs ((name, term) <| pre) None (body selfVar) (TypH l)
    let ir = SlfI name bodyIR
    return (toContext pre, TypH l, ir)
  LetH l name exprUse exprTyp expr body -> do
    (exprCtx, _, exprIR) <- check file defs pre exprUse expr exprTyp
    let var = VarH l name (Ctx.depth pre)
    (bodyCtx, typ, bodyIR) <- infer file defs ((name, exprTyp) <| pre) Once (body var)
    case _ctx bodyCtx of
      Empty -> throwError (EmptyContext file l)
      ((name', (exprUse', exprTyp')) :<| bodyCtx') -> do
        unless
          (exprUse' ≤# exprUse)
          ( do
              let original = (name, exprUse, exprTyp)
              let inferred = (name', exprUse', exprTyp')
              throwError (InferQuantityMismatch file l (Ctx bodyCtx') original inferred)
          )
        let isFix = case expr of
              FixH _ _ _ -> True
              _ -> False
        let ir = LetI isFix exprUse name exprIR bodyIR
        return (mulCtx use (addCtx exprCtx (Ctx bodyCtx')), typ, ir)
  TypH l -> return (toContext pre, TypH l, TypI)
  UnrH l nam lvl val typ -> do
    case Ctx.adjust lvl (toContext pre) (\(_, typ) -> (use, typ)) of
      Nothing -> throwError $ EmptyContext file l
      Just ((_, typ), ctx) -> do
        let ir = VarI nam
        return (ctx, typ, ir)
  AnnH _ val typ -> check file defs pre use val typ
  LitH l lit -> return (toContext pre, typeOfLit l lit, LitI lit)
  LTyH l lty -> return (toContext pre, typeOfLTy l lty, LTyI lty)
  OprH l opr -> return (toContext pre, typeOfOpr l opr, OprI opr)
  x -> throwError $ CustomErr file (hoasLoc x) pre "can't infer type"
