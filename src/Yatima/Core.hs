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
import Data.IPLD.CID
import Data.List (foldl')
import qualified Data.Map as M
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
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
    go :: Hoas -> [Hoas] -> Hoas
    go trm args = case trm of
      RefH nam cid _ -> case defs M.!? cid of
        Just d -> go (fst (defToHoas nam d)) args
        Nothing -> foldl' AppH trm args
      FixH nam bod -> go (bod trm) args
      AppH fun arg -> go fun (arg : args)
      LamH _ bod -> case args of
        [] -> trm
        (a : args') -> go (bod a) args'
      OprH opr -> reduceOpr opr args
      UseH arg -> case go arg [] of
        NewH exp -> go exp args
        LitH val -> go (expandLit val) args
        _ -> foldl' AppH (UseH arg) args
      AnnH a _ -> go a args
      UnrH _ _ a _ -> go a args
      LetH _ _ _ exp bod -> go (bod exp) args
      _ -> foldl' AppH trm args

norm :: Defs -> Hoas -> Hoas
norm defs term = go term 0 Set.empty
  where
    go :: Hoas -> Int -> Set CID -> Hoas
    go term lvl seen =
      let step = whnf defs term
          hash = makeCid $ termToAST $ hoasToTerm lvl term
          hash' = makeCid $ termToAST $ hoasToTerm lvl step
       in if
              | hash `Set.member` seen -> step
              | hash' `Set.member` seen -> step
              | otherwise -> next step lvl (Set.insert hash' (Set.insert hash seen))

    next :: Hoas -> Int -> Set CID -> Hoas
    next step lvl seen = case step of
      AllH nam use typ bod ->
        AllH nam use (go typ lvl seen) (\x -> go (bod x) (lvl + 1) seen)
      LamH nam bod -> LamH nam (\x -> go (bod x) (lvl + 1) seen)
      AppH fun arg -> go (AppH (go fun lvl seen) (go arg lvl seen)) lvl seen
      FixH nam bod -> go (bod (FixH nam bod)) lvl seen
      SlfH nam bod -> SlfH nam (\x -> go (bod x) (lvl + 1) seen)
      NewH exp -> NewH (go exp lvl seen)
      UseH exp -> UseH (go exp lvl seen)
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
        (All _ u h b, All _ u' h' b') -> pure (u == u') &&* go h h' &&* go b b'
        (Lam _ b, Lam _ b') -> go b b'
        (App f a, App f' a') -> go f f' &&* go a a'
        (Let r _ u _ x b, Let r' _ u' _ x' b') -> pure (r == r' && u == u') &&* go x x' &&* go b b'
        (Ann x _, Ann x' _) -> go x x'
        (Slf _ x, Slf _ x') -> go x x'
        (New x, New x') -> go x x'
        (Use x, Use x') -> go x x'
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
          (AppH f a, AppH f' a') ->
            go eq $ tris >< Seq.fromList [(f, f', dep + 1), (a, a', dep + 1)]
          (LamH n b, LamH n' b') ->
            go eq $ tris >< Seq.singleton (b (VarH n dep), b' (VarH n' dep), dep + 1)
          (AllH n u h b, AllH n' u' h' b') ->
            go eq $ tris >< Seq.fromList [(h, h', dep + 1), (b (VarH n dep), b' (VarH n' dep), dep + 1)]
          (SlfH n b, SlfH n' b') ->
            go eq $ tris >< Seq.singleton (b (VarH n dep), b' (VarH n' dep), dep + 1)
          (NewH b, NewH b') ->
            go eq $ tris >< Seq.singleton (b, b', dep + 1)
          (UseH b, UseH b') ->
            go eq $ tris >< Seq.singleton (b, b', dep + 1)
          _ ->
            return False
    getHash = makeCid . termToAST . hoasToTerm dep

-- * Type System

check ::
  Defs ->
  PreContext ->
  Uses ->
  Hoas ->
  Hoas ->
  Except CheckError (Context, Hoas, IR)
check defs pre use term typ = case term of
  LamH name body -> case whnf defs typ of
    AllH bindName bindUse bind typeBody -> do
      let bodyType = typeBody (VarH name (Ctx.depth pre))
      let bodyTerm = body (VarH name (Ctx.depth pre))
      (bodyCtx, _, bodyIR) <- check defs ((name, bind) <| pre) Once bodyTerm bodyType
      case _ctx bodyCtx of
        Empty -> throwError $ EmptyContext
        ((name', (bindUse', bind')) :<| bodyCtx') -> do
          unless
            (bindUse' ≤# bindUse)
            ( do
                let original = (name, bindUse, bind)
                let checked = (name', use, bind')
                throwError (CheckQuantityMismatch (Ctx bodyCtx') original checked)
            )
          let ir = LamI bindUse name bodyIR
          return (mulCtx use (Ctx bodyCtx'), typ, ir)
    x -> throwError $ LambdaNonFunctionType pre term typ x
  NewH expr -> case whnf defs typ of
    SlfH slfName slfBody -> do
      (exprCtx, exprTyp, exprIR) <- check defs pre use expr (slfBody term)
      return (exprCtx, exprTyp, NewI exprIR)
    x -> throwError $ NewNonSelfType pre term typ x
  LetH name exprUse exprTyp expr body -> do
    (exprCtx, _, exprIR) <- check defs pre exprUse expr exprTyp
    let var = VarH name (Ctx.depth pre)
    (bodyCtx, _, bodyIR) <- check defs ((name, exprTyp) <| pre) Once (body var) typ
    case _ctx bodyCtx of
      Empty -> throwError $ EmptyContext
      ((name', (exprUse', exprTyp')) :<| bodyCtx') -> do
        unless
          (exprUse' ≤# exprUse)
          ( do
              let original = (name, exprUse, exprTyp)
              let checked = (name', exprUse', exprTyp')
              throwError (CheckQuantityMismatch (Ctx bodyCtx') original checked)
          )
        let isFix = case expr of
              FixH _ _ -> True
              _ -> False
        let ir = LetI isFix exprUse name exprIR bodyIR
        return (mulCtx use (addCtx exprCtx (Ctx bodyCtx')), typ, ir)
  FixH name body -> do
    let unroll = body (UnrH name (Ctx.depth pre) (FixH name body) typ)
    (bodyCtx, _, bodyIR) <- check defs ((name, typ) <| pre) use unroll typ
    case _ctx bodyCtx of
      Empty -> throwError $ EmptyContext
      (_, (None, _)) :<| bodyCtx' -> return (Ctx bodyCtx', typ, bodyIR)
      (_, (use, _)) :<| bodyCtx' -> return (mulCtx Many (Ctx bodyCtx'), typ, bodyIR)
  _ -> do
    (ctx, termTyp, termIR) <- infer defs pre use term
    case equal defs typ termTyp (Ctx.depth pre) of
      False -> throwError (TypeMismatch pre typ termTyp)
      True -> return (ctx, typ, termIR)

-- | Infers the type of a term
infer ::
  Defs ->
  PreContext ->
  Uses ->
  Hoas ->
  Except CheckError (Context, Hoas, IR)
infer defs pre use term = case term of
  VarH nam lvl -> do
    let ir = VarI nam
    case Ctx.adjust lvl (toContext pre) (\(_, typ) -> (use, typ)) of
      Nothing -> throwError $ UnboundVariable nam lvl
      Just ((_, typ), ctx) -> return (ctx, typ, ir)
  RefH nam cid _ -> do
    --traceM ("RefH " ++ show nam)
    let mapMaybe = maybe (throwError $ UndefinedReference nam) pure
    def <- mapMaybe (defs M.!? cid)
    let (_, typ) = (defToHoas nam def)
    let ir = RefI nam
    return (toContext pre, typ, ir)
  LamH name body -> throwError $ UntypedLambda
  AppH func argm -> do
    (funcCtx, funcTyp, funcIR) <- infer defs pre use func
    case whnf defs funcTyp of
      AllH _ argmUse bind body -> do
        (argmCtx, _, argmIR) <- check defs pre (argmUse *# use) argm bind
        let ir = AppI argmUse funcIR argmIR
        return (addCtx funcCtx argmCtx, body argm, ir)
      x -> throwError $ NonFunctionApplication funcCtx func funcTyp x
  UseH expr -> do
    (exprCtx, exprTyp, exprIR) <- infer defs pre use expr
    case whnf defs exprTyp of
      SlfH _ body -> do
        return (exprCtx, body expr, UseI exprIR Nothing)
      LTyH typ -> do
        return (exprCtx, litInduction typ expr, UseI exprIR (Just typ))
      AppH (LTyH TBitVector) (LitH (VNatural n)) -> do
        let expr' = AppH (litInduction TBitVector (LitH (VNatural n))) expr
        return (exprCtx, expr', UseI exprIR (Just TBitVector))
      -- TODO: Make sure this is right
      x -> throwError $ NonSelfUse exprCtx expr exprTyp x
  AllH name bindUse bind body -> do
    let nameVar = VarH name $ Ctx.depth pre
    (_, _, bindIR) <- check defs pre None bind TypH
    (_, _, bodyIR) <- check defs ((name, bind) <| pre) None (body nameVar) TypH
    let ir = AllI name bindUse bindIR bodyIR
    return (toContext pre, TypH, ir)
  SlfH name body -> do
    let selfVar = VarH name $ Ctx.depth pre
    (_, _, bodyIR) <- check defs ((name, term) <| pre) None (body selfVar) TypH
    let ir = SlfI name bodyIR
    return (toContext pre, TypH, ir)
  LetH name exprUse exprTyp expr body -> do
    (exprCtx, _, exprIR) <- check defs pre exprUse expr exprTyp
    let var = VarH name (Ctx.depth pre)
    (bodyCtx, typ, bodyIR) <- infer defs ((name, exprTyp) <| pre) Once (body var)
    case _ctx bodyCtx of
      Empty -> throwError EmptyContext
      ((name', (exprUse', exprTyp')) :<| bodyCtx') -> do
        unless
          (exprUse' ≤# exprUse)
          ( do
              let original = (name, exprUse, exprTyp)
              let inferred = (name', exprUse', exprTyp')
              throwError (InferQuantityMismatch (Ctx bodyCtx') original inferred)
          )
        let isFix = case expr of
              FixH _ _ -> True
              _ -> False
        let ir = LetI isFix exprUse name exprIR bodyIR
        return (mulCtx use (addCtx exprCtx (Ctx bodyCtx')), typ, ir)
  TypH -> return (toContext pre, TypH, TypI)
  UnrH nam lvl val typ -> do
    case Ctx.adjust lvl (toContext pre) (\(_, typ) -> (use, typ)) of
      Nothing -> throwError $ EmptyContext
      Just ((_, typ), ctx) -> do
        let ir = VarI nam
        return (ctx, typ, ir)
  AnnH val typ -> do
    check defs pre use val typ
  LitH lit -> return (toContext pre, typeOfLit lit, LitI lit)
  LTyH lty -> return (toContext pre, typeOfLTy lty, LTyI lty)
  OprH opr -> return (toContext pre, typeOfOpr opr, OprI opr)
  _ -> throwError $ CustomErr pre "can't infer type"
