{-|
Module      : Yatima.Core
Description : Evaluate and typecheck expressions in the Yatima Language using
higher-order-abstract-syntax
Copyright   : (c) Sunshine Cybernetics, 2020
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental
-}
{-# LANGUAGE DerivingVia #-}
module Yatima.Core where

import           Control.Monad.Except
import           Control.Monad.Identity

import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Sequence                  (Seq (..))
import qualified Data.Sequence                  as Seq
import           Data.Set                       (Set)
import qualified Data.Set                       as Set

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TB

import           Yatima.Core.Ctx            (Ctx (..), (<|))
import qualified Yatima.Core.Ctx            as Ctx
import           Yatima.Core.Hoas
import           Yatima.Core.Prim
import           Yatima.Print
import           Yatima.Term
import           Yatima.IR

whnf :: Defs -> Hoas -> Hoas
whnf defs trm = case trm of
  RefH nam           -> case defs M.!? nam of
    Just d  -> go $ fst (defToHoas nam d)
    Nothing -> RefH nam
  FixH nam bod       -> go (bod trm)
  AppH fun arg       -> case go fun of
    LamH _ bod -> go (bod arg)
    OprH opr   -> reduceOpr opr arg
    x          -> AppH fun arg
  UseH arg           -> case go arg of
    NewH exp     -> go exp
    LitH val     -> expandLit val
    x            -> UseH arg
  AnnH a _     -> go a
  UnrH _ _ a _ -> go a
  LetH _ _ _ exp bod -> go (bod exp)
  WhnH x             -> x

  x                  -> x
  where
    go x = whnf defs x

-- | Normalize a Hoas term
--norm :: Defs -> Hoas -> Hoas
--norm defs term = case whnf defs term of
--  AllH nam use typ bod -> AllH nam use (go typ) (\x -> go (bod x))
--  LamH nam bod         -> LamH nam (\x -> go (bod x))
--  AppH fun arg         -> AppH (go fun) (go arg)
--  FixH nam bod         -> go (bod (FixH nam bod))
--  SlfH nam bod         -> SlfH nam (\x -> go (bod x))
--  NewH exp             -> NewH (go exp)
--  UseH exp             -> UseH (go exp)
--  step                 -> step

norm :: Defs -> Hoas -> Hoas
norm defs term = go term 0 Set.empty
  where
    go :: Hoas -> Int -> Set LT.Text -> Hoas
    go term lvl seen =
      let step  = whnf defs term
          hash  = serialize lvl term
          hash' = serialize lvl step
       in
       if | hash  `Set.member` seen -> step
          | hash' `Set.member` seen -> step
          | otherwise -> next step lvl (Set.insert hash' (Set.insert hash seen))

    next :: Hoas -> Int -> Set LT.Text -> Hoas
    next step lvl seen = case step of
      AllH nam use typ bod ->
        AllH nam use (go typ lvl seen) (\x -> go (bod x) (lvl+1) seen)
      LamH nam bod         -> LamH nam (\x -> go (bod x) (lvl+1) seen)
      AppH fun arg         -> AppH (go fun lvl seen) (go arg lvl seen)
      FixH nam bod         -> go (bod (FixH nam bod)) lvl seen
      SlfH nam bod         -> SlfH nam (\x -> go (bod x) (lvl+1) seen)
      NewH exp             -> NewH (go exp lvl seen)
      UseH exp             -> UseH (go exp lvl seen)
      step                 -> step

-- Converts a term to a unique string representation. This is used by equal.
-- TODO: use a hash function instead
serialize :: Int -> Hoas -> LT.Text
serialize lvl term = TB.toLazyText (go term lvl lvl)
  where
    name :: Name -> TB.Builder
    name "" = "_"
    name x  = TB.fromText x

    uses :: Uses -> TB.Builder
    uses None = "0"
    uses Affi = "&"
    uses Once = "1"
    uses Many = "ω"

    go :: Hoas -> Int -> Int -> TB.Builder
    go term lvl ini = case term of
      TypH                     -> "*"
      VarH _ idx               ->
        if idx >= ini
        then "^" <> TB.decimal (lvl-idx-1)
        else "#" <> TB.decimal idx
      RefH nam -> "$" <> name nam
      AllH nam use typ bod     ->
        "∀" <> uses use <> name nam
        <> go typ lvl ini
        <> go (bod (VarH nam lvl)) (lvl+1) ini
      SlfH nam bod             ->
        "@" <> name nam <> go (bod (VarH nam lvl)) (lvl+1) ini
      LamH nam bod             ->
        "λ" <> name nam <> go (bod (VarH nam lvl)) (lvl+1) ini
      AppH fun arg             -> "+" <> go fun lvl ini <> go arg lvl ini
      NewH exp                 -> "ν" <> go exp lvl ini
      UseH exp                 -> "σ" <> go exp lvl ini
      LetH nam use typ exp bod ->
        "~" <> uses use <> name nam
        <> go typ lvl ini <> go exp lvl ini
        <> go (bod (VarH nam lvl)) (lvl+1) ini
      FixH nam bod             ->
        "%" <> name nam <> go (bod (VarH nam lvl)) (lvl+1) ini
      AnnH trm _               -> go trm lvl ini
      UnrH _ _ trm _           -> go trm lvl ini
      HolH nam                 -> "?" <> name nam
      LitH lit                 -> "(" <> TB.fromText (prettyLiteral lit) <> ")"
      LTyH lit                 -> "<" <> TB.fromText (prettyLitType lit) <> ">"
      OprH opr                 -> "{" <> TB.fromText (prettyPrimOp opr) <> "}"

equal :: Defs -> Hoas -> Hoas -> Int -> Either Hole Bool
equal defs a b lvl = go a b lvl Set.empty
  where
    go :: Hoas -> Hoas -> Int -> Set (LT.Text,LT.Text) -> Either Hole Bool
    go a b lvl seen = do
      let aWhnf = whnf defs a
      let bWhnf = whnf defs b
      let aHash = serialize lvl aWhnf
      let bHash = serialize lvl bWhnf
      if | (aHash == bHash)                -> return True
         | (aHash,bHash) `Set.member` seen -> return True
         | (bHash,aHash) `Set.member` seen -> return True
         | otherwise -> do
             let seen' = Set.insert (aHash,bHash) seen
             next aWhnf bWhnf lvl seen'

    next :: Hoas -> Hoas -> Int -> Set (LT.Text, LT.Text) -> Either Hole Bool
    next a b lvl seen = case (a, b) of
      (AllH aNam aUse aTyp aBod, AllH bNam bUse bTyp bBod) -> do
        let aBod' = aBod (VarH aNam lvl)
        let bBod' = bBod (VarH bNam lvl)
        let useEq = aUse == bUse
        typEq <- go aTyp bTyp lvl seen
        bodEq <- go aBod' bBod' (lvl+1) seen
        return $ useEq && typEq && bodEq
      (SlfH aNam aBod, SlfH bNam bBod) -> do
        let aBod' = aBod (VarH aNam lvl)
        let bBod' = bBod (VarH bNam lvl)
        go aBod' bBod' (lvl+1) seen
      (LamH aNam aBod, LamH bNam bBod) -> do
        let aBod' = aBod (VarH aNam lvl)
        let bBod' = bBod (VarH bNam lvl)
        go aBod' bBod' (lvl+1) seen
      (NewH aExp, NewH bExp) -> do
        go aExp bExp lvl seen
      (UseH aExp, UseH bExp) -> do
        go aExp bExp lvl seen
      (AppH aFun aArg, AppH bFun bArg) -> do
        funEq <- go aFun bFun lvl seen
        argEq <- go aArg bArg lvl seen
        return $ funEq && argEq
      (HolH name, b) -> Left (name, b)
      (a, HolH name) -> Left (name, a)
      _         -> return False

data CheckErr
  = CheckQuantityMismatch Context (Name,Uses,Hoas) (Name,Uses,Hoas)
  | InferQuantityMismatch Context (Name,Uses,Hoas) (Name,Uses,Hoas)
  | TypeMismatch PreContext Hoas Hoas
  | UnboundVariable Name Int
  | FoundHole Hole
  | EmptyContext
  | UntypedLambda
  | UndefinedReference Name
  | LambdaNonFunctionType PreContext Hoas Hoas Hoas
  | NewNonSelfType PreContext Hoas Hoas Hoas
  | NonFunctionApplication Context Hoas Hoas Hoas
  | NonSelfUse  Context Hoas Hoas Hoas
  | CustomErr PreContext Text

-- | Fills holes of a term until it is complete
synth :: Defs -> Hoas -> Hoas -> Except CheckErr (Hoas, Hoas)
synth defs term typ_ = do
  case runExcept (check  defs Ctx.empty Once term typ_) of
    Left (FoundHole hole) -> synth  defs (fill hole term) (fill hole typ_)
    Left error            -> throwError error
    Right (_,typ_,_)        -> return (term, typ_)

fill :: Hole -> Hoas -> Hoas
fill hole term = case term of
  TypH                 -> TypH
  VarH nam idx         -> VarH nam idx
  LamH nam bod         -> LamH nam (\x -> go (bod x))
  AllH nam use typ bod -> AllH nam use (go typ) (\x -> go (bod x))
  SlfH nam bod         -> SlfH nam (\x -> go (bod x))
  NewH exp             -> NewH (go exp)
  UseH exp             -> UseH (go exp)
  AppH fun arg         -> AppH (go fun) (go arg)
  HolH nam             -> if fst hole == nam then snd hole else HolH nam
  _                    -> term
  where
    go x = fill hole x

-- * Type System
check :: Defs -> PreContext -> Uses -> Hoas -> Hoas
      -> Except CheckErr (Context, Hoas, IR)
check defs pre use term typ = case term of
  LamH name body -> case whnf defs typ of
    AllH bindName bindUse bind typeBody -> do
      let bodyType = typeBody (VarH name (Ctx.depth pre))
      let bodyTerm = body (VarH name (Ctx.depth pre))
      (bodyCtx,_,bodyIR) <- check defs ((name,bind) <| pre) Once bodyTerm bodyType
      case _ctx bodyCtx of
        Empty -> throwError $ EmptyContext
        ((name',(bindUse',bind')) :<| bodyCtx') -> do
          unless (bindUse' ≤# bindUse) (do
            let original = (name,bindUse,bind)
            let checked  = (name',use,bind')
            throwError (CheckQuantityMismatch (Ctx bodyCtx') original checked))
          let ir = LamI bindUse name bodyIR
          return (mulCtx use (Ctx bodyCtx'),typ,ir)
    x -> throwError $ LambdaNonFunctionType pre term typ x
  NewH expr -> case whnf defs typ of
    SlfH slfName slfBody -> do
      (exprCtx,exprTyp,exprIR) <- check defs pre use expr (slfBody term)
      return (exprCtx,exprTyp,NewI exprIR)
    x -> throwError $ NewNonSelfType pre term typ x
  LetH name exprUse exprTyp expr body -> do
    (exprCtx,_,exprIR) <- check defs pre exprUse expr exprTyp
    let var = VarH name (Ctx.depth pre)
    (bodyCtx,_,bodyIR) <- check defs ((name,exprTyp) <| pre) Once (body var) typ
    case _ctx bodyCtx of
      Empty -> throwError $ EmptyContext
      ((name',(exprUse',exprTyp')) :<| bodyCtx') -> do
        unless (exprUse' ≤# exprUse) (do
          let original = (name,exprUse,exprTyp)
          let checked  = (name',exprUse',exprTyp')
          throwError (CheckQuantityMismatch (Ctx bodyCtx') original checked))
        let isFix = case expr of
              FixH _ _ -> True
              _        -> False
        let ir    = LetI isFix exprUse name exprIR bodyIR
        return (mulCtx use (addCtx exprCtx (Ctx bodyCtx')),typ,ir)
  FixH name body -> do
    let unroll = body (UnrH name (Ctx.depth pre) (FixH name body) typ)
    (bodyCtx,_,bodyIR) <- check defs ((name,typ) <| pre) use unroll typ
    case _ctx bodyCtx of
     Empty -> throwError $ EmptyContext
     (_,(None,_)) :<| bodyCtx' -> return (Ctx bodyCtx',typ,bodyIR)
     (_,(use,_))  :<| bodyCtx' -> return (mulCtx Many (Ctx bodyCtx'),typ,bodyIR)
  _ -> do
    (ctx,termTyp,termIR) <- infer defs pre use term
    case equal defs typ termTyp (Ctx.depth pre) of
      Left hole   -> throwError (FoundHole hole)
      Right False -> throwError (TypeMismatch pre typ termTyp)
      Right True  -> return (ctx,typ,termIR)

-- | Infers the type of a term
infer :: Defs -> PreContext -> Uses -> Hoas
      -> Except CheckErr (Context, Hoas, IR)
infer defs pre use term = case term of
  VarH nam lvl -> do
    let ir = VarI nam
    case Ctx.adjust lvl (toContext pre) (\(_,typ) -> (use,typ)) of
      Nothing            -> throwError $ UnboundVariable nam lvl
      Just ((_,typ),ctx) -> return (ctx,typ,ir)
  RefH nam -> do
    --traceM ("RefH " ++ show nam)
    let mapMaybe = maybe (throwError $ UndefinedReference nam) pure
    def         <- mapMaybe (defs M.!? nam)
    let (_,typ) = (defToHoas nam def)
    let ir = RefI nam
    return (toContext pre,typ,ir)
  LamH name body -> throwError $ UntypedLambda
  AppH func argm -> do
    (funcCtx,funcTyp,funcIR) <- infer defs pre use func
    case whnf defs funcTyp of
      AllH _ argmUse bind body -> do
        (argmCtx,_,argmIR) <- check defs pre (argmUse *# use) argm bind
        let ir = AppI argmUse funcIR argmIR
        return (addCtx funcCtx argmCtx,body argm,ir)
      x -> throwError $ NonFunctionApplication funcCtx func funcTyp x
  UseH expr -> do
    (exprCtx,exprTyp,exprIR) <- infer defs pre use expr
    case whnf defs exprTyp of
      SlfH _ body -> do
        return (exprCtx,body expr,UseI exprIR Nothing)
      LTyH typ -> do
        return (exprCtx, litInduction typ expr,UseI exprIR (Just typ))
      x -> throwError $ NonSelfUse exprCtx expr exprTyp x
  AllH name bindUse bind body -> do
    let nameVar = VarH name $ Ctx.depth pre
    (_,_,bindIR) <- check defs pre None bind TypH
    (_,_,bodyIR) <- check defs ((name,bind)<|pre) None (body nameVar) TypH
    let ir = AllI name bindUse bindIR bodyIR
    return (toContext pre,TypH,ir)
  SlfH name body -> do
    let selfVar = VarH name $ Ctx.depth pre
    (_,_,bodyIR) <- check defs ((name,term)<|pre) None (body selfVar) TypH
    let ir = SlfI name bodyIR
    return (toContext pre,TypH,ir)
  LetH name exprUse exprTyp expr body -> do
    (exprCtx,_,exprIR)    <- check defs pre exprUse expr exprTyp
    let var = VarH name (Ctx.depth pre)
    (bodyCtx,typ,bodyIR) <- infer defs ((name,exprTyp) <| pre) Once (body var)
    case _ctx bodyCtx of
      Empty -> throwError EmptyContext
      ((name',(exprUse',exprTyp')) :<| bodyCtx') -> do
        unless (exprUse' ≤# exprUse) (do
          let original = (name,exprUse,exprTyp)
          let inferred = (name',exprUse',exprTyp')
          throwError (InferQuantityMismatch (Ctx bodyCtx') original inferred))
        let isFix = case expr of
              FixH _ _ -> True
              _        -> False
        let ir    = LetI isFix exprUse name exprIR bodyIR
        return (mulCtx use (addCtx exprCtx (Ctx bodyCtx')),typ,ir)
  -- Hole
  TypH           -> return (toContext pre,TypH,TypI)
  UnrH nam lvl val typ -> do
    case Ctx.adjust lvl (toContext pre) (\(_,typ) -> (use,typ)) of
      Nothing -> throwError $ EmptyContext
      Just ((_,typ),ctx) -> do
        let ir = VarI nam
        return (ctx,typ,ir)
  AnnH val typ -> do
    check defs pre use val typ
  LitH lit  -> return (toContext pre, typeOfLit lit, LitI lit)
  LTyH lty  -> return (toContext pre, TypH, LTyI lty)
  OprH opr  -> return (toContext pre, typeOfOpr opr, OprI opr)
  HolH name -> return (toContext pre,TypH,TypI)
  _ -> throwError $ CustomErr pre "can't infer type"

---- * Pretty Printing Errors
prettyUses :: Uses -> Text
prettyUses None = "0"
prettyUses Affi = "&"
prettyUses Once = "1"
prettyUses Many = "ω"

prettyCtx :: Context -> Text
prettyCtx (Ctx ctx) = foldr go "" ctx
  where
    go :: (Name,(Uses,Hoas)) -> Text -> Text
    go (n,(u,t)) txt = T.concat ["- ", prettyCtxElem (n,u,t),"\n",txt]

prettyCtxElem :: (Name,Uses,Hoas) -> Text
prettyCtxElem ("",u,t) = T.concat [prettyUses u, " _: ", printHoas t]
prettyCtxElem (n,u,t)  = T.concat [prettyUses u , " ", n, ": ", printHoas t]

prettyPre :: PreContext -> Text
prettyPre (Ctx ctx) = foldr go "" ctx
  where
    go :: (Name,Hoas) -> Text -> Text
    go (n,t) txt = T.concat ["- ", prettyPreElem (n,t),"\n",txt]

prettyPreElem :: (Name,Hoas) -> Text
prettyPreElem ("",t) = T.concat [" _: ", printHoas t]
prettyPreElem (n,t)  = T.concat [" ", n, ": ", printHoas t]

prettyError :: CheckErr -> Text
prettyError e = case e of
    CheckQuantityMismatch ctx a b -> T.concat
      ["Type checking quantity mismatch: \n"
      , "- Expected: ", prettyCtxElem a, "\n"
      , "- Detected: ", prettyCtxElem b, "\n"
      , "With context:\n"
      , prettyCtx ctx
      ]
    InferQuantityMismatch ctx a b -> T.concat
      ["Type inference quantity mismatch: \n"
      , "- Expected: ", prettyCtxElem a, "\n"
      , "- Inferred: ", prettyCtxElem b, "\n"
      , "With context:\n"
      , prettyCtx ctx
      ]
    TypeMismatch ctx a b -> T.concat
      ["Type Mismatch: \n"
      , "- Expected: ", prettyPreElem ("",a), "\n"
      , "- Detected: ", prettyPreElem ("",b), "\n"
      , "With context:\n"
      , prettyPre ctx
      ]
    LambdaNonFunctionType ctx trm typ typ' -> T.concat
      ["The type of a lambda must be a forall: \n"
      , "  Checked term: ", printHoas trm,"\n"
      , "  Against type: ", printHoas typ, "\n"
      , "  Reduced type: ",  printHoas typ',"\n"
      , "With context:\n"
      , prettyPre ctx
      ]
    NewNonSelfType ctx trm typ typ' -> T.concat
      ["The type of data must be a self: \n"
      , "  Checked term: ", printHoas trm,"\n"
      , "  Against type: ", printHoas typ, "\n"
      , "  Reduced type: ",  printHoas typ',"\n"
      , "With context:\n"
      , prettyPre ctx
      ]
    UnboundVariable nam idx -> T.concat
      ["Unbound free variable: ", T.pack $ show nam, " at level ", T.pack $ show idx]
    UntypedLambda -> "Can't infer the type of a lambda"
    FoundHole (name,term) -> T.concat
      ["Can't fill hole: ?", T.pack $ show name, " : ", printHoas term]
    NonFunctionApplication ctx trm typ typ' -> T.concat
      ["Tried to apply something that wasn't a lambda: \n"
      , "  Checked term: ", printHoas trm,"\n"
      , "  Against type: ", printHoas typ, "\n"
      , "  Reduced type: ",  printHoas typ',"\n"
      , "With context:\n"
      , prettyCtx ctx
      ]
    NonSelfUse ctx trm typ typ' -> T.concat
      ["Tried to case on something that wasn't data: \n"
      , "  Checked term: ", printHoas trm,"\n"
      , "  Against type: ", printHoas typ, "\n"
      , "  Reduced type: ",  printHoas typ',"\n"
      , "With context:\n"
      , prettyCtx ctx
      ]
    EmptyContext -> "Empty Context"
    UndefinedReference name -> T.concat
      ["UndefinedReference error: \n"
      , "Name: ", T.pack $ show name, "\n"
      ]
    CustomErr ctx txt -> T.concat
      ["Custom Error:\n"
      , txt,"\n"
      , "With context:\n"
      , prettyPre ctx
      ]

instance Show CheckErr where
  show e = T.unpack $ prettyError e
