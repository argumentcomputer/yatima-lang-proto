{-|
Module      : Language.Yatima.HOAS
Description : Evaluate and typecheck expressions in the Yatima Language using
higher-order-abstract-syntax
Copyright   : (c) Sunshine Cybernetics, 2020
License     : GPL-3
Maintainer  : john@sunshinecybernetics.com
Stability   : experimental
-}
{-# LANGUAGE DerivingVia #-}
module Language.Yatima.Core where

import           Control.Monad.Except
import           Control.Monad.Identity

import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import           Data.Word

import           Data.Map                       (Map)
import qualified Data.Map                       as M
import           Data.Sequence                  (Seq (..))
import qualified Data.Sequence                  as Seq
import           Data.Set                       (Set)
import qualified Data.Set                       as Set

import           Data.Text                      (Text)
import qualified Data.Text                      as T

import qualified Data.Text.Lazy                 as LT
import qualified Data.Text.Lazy.Builder         as TB
import qualified Data.Text.Lazy.Builder.Int     as TB

import           Language.Yatima.Ctx            (Ctx (..), (<|))
import qualified Language.Yatima.Ctx            as Ctx
import           Language.Yatima.Print
import           Language.Yatima.Term

import           Debug.Trace

-- | Higher-Order Abstract Syntax
data HOAS where
  VarH :: Name -> Int -> HOAS
  RefH :: Name -> HOAS
  LamH :: Name -> (HOAS -> HOAS) -> HOAS
  AppH :: HOAS -> HOAS -> HOAS
  NewH :: HOAS -> HOAS
  UseH :: HOAS -> HOAS
  LetH :: Name -> Uses -> HOAS -> HOAS -> (HOAS -> HOAS) -> HOAS
  AllH :: Name -> Uses -> HOAS -> (HOAS -> HOAS) -> HOAS
  SlfH :: Name -> (HOAS -> HOAS) -> HOAS
  FixH :: Name -> (HOAS -> HOAS) -> HOAS
  AnnH :: HOAS -> HOAS -> HOAS
  UnrH :: Int  -> HOAS -> HOAS -> HOAS
  TypH :: HOAS
  HolH :: Name -> HOAS
  LitH :: Constant -> HOAS

type PreContext = Ctx HOAS
type Context    = Ctx (Uses,HOAS)

mulCtx :: Uses -> Context -> Context
mulCtx Once ctx = ctx
mulCtx uses ctx = fmap (\(uses', typ) -> (uses *# uses', typ)) ctx

-- Assumes both context are compatible
addCtx :: Context -> Context -> Context
addCtx = Ctx.zipWith (\(uses,typ) (uses',_) -> (uses +# uses', typ))

toContext :: PreContext -> Context
toContext = fmap (\(term) -> (None, term))

-- | A filled hole
type Hole = (Name, HOAS)

-- | Convert a lower-order `Term` to a GHC higher-order one
termToHoas :: PreContext -> Term -> HOAS
termToHoas ctx t = case t of
  Typ                     -> TypH
  Hol nam                 -> HolH nam
  Var nam                 -> maybe (VarH nam 0) id (Ctx.find nam ctx)
  Ref nam                 -> RefH nam
  Lam nam bod             -> LamH nam (bind nam bod)
  App fun arg             -> AppH (go fun) (go arg)
  New exp                 -> NewH (go exp)
  Use exp                 -> UseH (go exp)
  Ann val typ             -> AnnH (go val) (go typ)
  Let nam use typ exp bod -> LetH nam use (go typ) (rec nam exp) (bind nam bod)
  All nam use typ bod     -> AllH nam use (go typ) (bind nam bod)
  Slf nam bod             -> SlfH nam (bind nam bod)
  Lit con                 -> LitH con
  where
    go      t   = termToHoas ctx t
    bind  n t   = (\x   -> termToHoas ((n,x)<|ctx) t)
    rec n t = FixH n (bind n t)

-- | Convert a GHC higher-order representation to a lower-order one
hoasToTerm :: PreContext -> HOAS -> Term
hoasToTerm ctx t = case t of
  TypH                     -> Typ
  HolH nam                 -> Hol nam
  RefH nam                 -> Ref nam
  VarH nam idx             -> Var nam
  LamH nam bod             -> Lam nam (bind nam bod)
  AppH fun arg             -> App (go fun) (go arg)
  UseH exp                 -> Use (go exp)
  NewH exp                 -> New (go exp)
  LetH nam use typ exp bod -> Let nam use (go typ) (go exp) (bind nam bod)
  AllH nam use typ bod     -> All nam use (go typ) (bind nam bod)
  SlfH nam bod             -> Slf nam (bind nam bod)
  FixH nam bod             -> bind nam bod
  AnnH trm typ             -> Ann (go trm) (go typ)
  UnrH _   trm _           -> go trm
  LitH con                 -> Lit con
  where
    dep          = Ctx.depth ctx
    go t         = hoasToTerm ctx t
    bind n b     = hoasToTerm ((n,TypH)<|ctx) (b (VarH n dep))

printHOAS :: HOAS -> Text
printHOAS = prettyTerm . (hoasToTerm Ctx.empty)

instance Show HOAS where
 show t = T.unpack $ printHOAS t

defToHoas :: Name -> Def -> (HOAS,HOAS)
defToHoas name (Def _ term typ_) =
  ( FixH name (\s -> termToHoas (Ctx.singleton (name,s)) term)
  , FixH name (\s -> termToHoas (Ctx.singleton (name,s)) typ_)
  )

whnf :: Defs -> HOAS -> HOAS
whnf defs trm = case trm of
  RefH nam           -> case defs M.!? nam of
    Just d  -> go $ fst (defToHoas nam d)
    Nothing -> RefH nam
  FixH nam bod       -> go (bod trm)
  AppH fun arg       -> case go fun of
    LamH _ bod -> go (bod arg)
    x          -> AppH fun arg
  AnnH a _     -> go a
  UnrH _ a _   -> go a
  LetH _ _ _ exp bod -> go (bod exp)
  x                  -> x
  where
    go x = whnf defs x

-- | Normalize a HOAS term
--norm :: Defs -> HOAS -> HOAS
--norm defs term = case whnf defs term of
--  AllH nam use typ bod -> AllH nam use (go typ) (\x -> go (bod x))
--  LamH nam bod         -> LamH nam (\x -> go (bod x))
--  AppH fun arg         -> AppH (go fun) (go arg)
--  FixH nam bod         -> go (bod (FixH nam bod))
--  SlfH nam bod         -> SlfH nam (\x -> go (bod x))
--  NewH exp             -> NewH (go exp)
--  UseH exp             -> UseH (go exp)
--  step                 -> step

norm :: Defs -> HOAS -> HOAS
norm defs term = go term 0 Set.empty
  where
    go :: HOAS -> Int -> Set LT.Text -> HOAS
    go term lvl seen =
      let step  = whnf defs term
          hash  = serialize lvl term
          hash' = serialize lvl step
       in
       if | hash  `Set.member` seen -> step
          | hash' `Set.member` seen -> step
          | otherwise -> next step lvl (Set.insert hash' (Set.insert hash seen))

    next :: HOAS -> Int -> Set LT.Text -> HOAS
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
serialize :: Int -> HOAS -> LT.Text
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

    go :: HOAS -> Int -> Int -> TB.Builder
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
      UnrH _ trm _             -> go trm lvl ini
      HolH nam                 -> "?" <> name nam

equal :: Defs -> HOAS -> HOAS -> Int -> Either Hole Bool
equal defs a b lvl = go a b lvl Set.empty
  where
    go :: HOAS -> HOAS -> Int -> Set (LT.Text,LT.Text) -> Either Hole Bool
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

    next :: HOAS -> HOAS -> Int -> Set (LT.Text, LT.Text) -> Either Hole Bool
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
--      (TypH, b) -> return True
--      (a, TypH) -> return True
      _         -> return False

data CheckErr
  = CheckQuantityMismatch Context (Name,Uses,HOAS) (Name,Uses,HOAS)
  | InferQuantityMismatch Context (Name,Uses,HOAS) (Name,Uses,HOAS)
  | TypeMismatch PreContext HOAS HOAS
  | UnboundVariable Name Int
  | FoundHole Hole
  | EmptyContext
  | UntypedLambda
  | UndefinedReference Name
  | LambdaNonFunctionType PreContext HOAS HOAS HOAS
  | NewNonSelfType PreContext HOAS HOAS HOAS
  | NonFunctionApplication Context HOAS HOAS HOAS
  | NonSelfUse  Context HOAS HOAS HOAS
  | CustomErr PreContext Text

-- | Fills holes of a term until it is complete
synth :: Defs -> HOAS -> HOAS -> Except CheckErr (HOAS, HOAS)
synth defs term typ_ = do
  case runExcept (check  defs Ctx.empty Once term typ_) of
    Left (FoundHole hole) -> synth  defs (fill hole term) (fill hole typ_)
    Left error            -> throwError error
    Right (_,typ_)        -> return (term, typ_)

fill :: Hole -> HOAS -> HOAS
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
  where
    go x = fill hole x

-- * Type System
check :: Defs -> PreContext -> Uses -> HOAS -> HOAS
      -> Except CheckErr (Context, HOAS)
check defs pre use term typ = case term of
  LamH name body -> case whnf defs typ of
    AllH bindName bindUse bind typeBody -> do
      let bodyType = typeBody (VarH name (Ctx.depth pre))
      let bodyTerm = body (VarH name (Ctx.depth pre))
      (bodyCtx,_) <- check defs ((name,bind) <| pre) Once bodyTerm bodyType
      case _ctx bodyCtx of
        Empty -> throwError $ EmptyContext
        ((name',(bindUse',bind')) :<| bodyCtx') -> do
          unless (bindUse' ≤# bindUse) (do
            let original = (name,bindUse,bind)
            let checked  = (name',use,bind')
            throwError (CheckQuantityMismatch (Ctx bodyCtx') original checked))
          return (mulCtx use (Ctx bodyCtx'),typ)
    x -> throwError $ LambdaNonFunctionType pre term typ x
  NewH expr -> case whnf defs typ of
    SlfH slfName slfBody -> do
      check defs pre use expr (slfBody term)
    x -> throwError $ NewNonSelfType pre term typ x
  LetH name exprUse exprTyp expr body -> do
    (exprCtx,_) <- check defs pre exprUse expr exprTyp
    let var = VarH name (Ctx.depth pre)
    (bodyCtx,_) <- check defs ((name,exprTyp) <| pre) Once (body var) typ
    case _ctx bodyCtx of
      Empty -> throwError $ EmptyContext
      ((name',(exprUse',exprTyp')) :<| bodyCtx') -> do
        unless (exprUse' ≤# exprUse) (do
          let original = (name,exprUse,exprTyp)
          let checked  = (name',exprUse',exprTyp')
          throwError (CheckQuantityMismatch (Ctx bodyCtx') original checked))
        return $ (mulCtx use (addCtx exprCtx (Ctx bodyCtx')), typ)
  FixH name body -> do
    let unroll = body (UnrH (Ctx.depth pre) (FixH name body) typ)
    (bodyCtx,_) <- check defs ((name,typ) <| pre) use unroll typ
    case _ctx bodyCtx of
     Empty -> throwError $ EmptyContext
     (_,(None,_)) :<| bodyCtx' -> return (Ctx bodyCtx', typ)
     (_,(use,_))  :<| bodyCtx' -> return (mulCtx Many (Ctx bodyCtx'),typ)
  _ -> do
    (ctx, termTyp) <- infer defs pre use term
    case equal defs typ termTyp (Ctx.depth pre) of
      Left hole   -> throwError (FoundHole hole)
      Right False -> throwError (TypeMismatch pre typ termTyp)
      Right True  -> return (ctx, typ)

-- | Infers the type of a term
infer :: Defs -> PreContext -> Uses -> HOAS
      -> Except CheckErr (Context, HOAS)
infer defs pre use term = case term of
  VarH nam lvl -> do
    case Ctx.adjust lvl (toContext pre) (\(_,typ) -> (use,typ)) of
      Nothing            -> throwError $ UnboundVariable nam lvl
      Just ((_,typ),ctx) -> return (ctx, typ)
  RefH nam -> do
    --traceM ("RefH " ++ show nam)
    let mapMaybe = maybe (throwError $ UndefinedReference nam) pure
    def         <- mapMaybe (defs M.!? nam)
    let (_,typ) = (defToHoas nam def)
    return (toContext pre,typ)
  LamH name body -> throwError $ UntypedLambda
  AppH func argm -> do
    (funcCtx, funcTyp) <- infer defs pre use func
    case whnf defs funcTyp of
      AllH _ argmUse bind body -> do
        (argmCtx,_) <- check defs pre (argmUse *# use) argm bind
        return (addCtx funcCtx argmCtx, body argm)
      x -> throwError $ NonFunctionApplication funcCtx func funcTyp x
  UseH expr -> do
    (exprCtx, exprTyp) <- infer defs pre use expr
    case whnf defs exprTyp of
      SlfH _ body -> do
        return (exprCtx, body expr)
      x -> throwError $ NonSelfUse exprCtx expr exprTyp x
  AllH name bindUse bind body -> do
    let nameVar = VarH name $ Ctx.depth pre
    check defs pre None bind TypH
    check defs ((name,bind)<|pre) None (body nameVar) TypH
    return (toContext pre, TypH)
  SlfH name body -> do
    let selfVar = VarH name $ Ctx.depth pre
    check defs ((name,term)<|pre) None (body selfVar) TypH
    return (toContext pre, TypH)
  LetH name exprUse exprTyp expr body -> do
    (exprCtx,_)    <- check defs pre exprUse expr exprTyp
    let var = VarH name (Ctx.depth pre)
    (bodyCtx, typ) <- infer defs ((name,exprTyp) <| pre) Once (body var)
    case _ctx bodyCtx of
      Empty -> throwError EmptyContext
      ((name',(exprUse',exprTyp')) :<| bodyCtx') -> do
        unless (exprUse' ≤# exprUse) (do
          let original = (name,exprUse,exprTyp)
          let inferred = (name',exprUse',exprTyp')
          throwError (InferQuantityMismatch (Ctx bodyCtx') original inferred))
        return (mulCtx use (addCtx exprCtx (Ctx bodyCtx')), typ)
  -- Hole
  TypH           -> return (toContext pre, TypH)
  UnrH lvl val typ -> do
    case Ctx.adjust lvl (toContext pre) (\(_,typ) -> (use,typ)) of
      Nothing -> throwError $ EmptyContext
      Just ((_,typ),ctx) -> return (ctx, typ)
  AnnH val typ -> do
    check defs pre use val typ
  (HolH name) -> return (toContext pre, TypH)
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
    go :: (Name,(Uses,HOAS)) -> Text -> Text
    go (n,(u,t)) txt = T.concat ["- ", prettyCtxElem (n,u,t),"\n",txt]

prettyCtxElem :: (Name,Uses,HOAS) -> Text
prettyCtxElem ("",u,t) = T.concat [prettyUses u, " _: ", printHOAS t]
prettyCtxElem (n,u,t)  = T.concat [prettyUses u , " ", n, ": ", printHOAS t]

prettyPre :: PreContext -> Text
prettyPre (Ctx ctx) = foldr go "" ctx
  where
    go :: (Name,HOAS) -> Text -> Text
    go (n,t) txt = T.concat ["- ", prettyPreElem (n,t),"\n",txt]

prettyPreElem :: (Name,HOAS) -> Text
prettyPreElem ("",t) = T.concat [" _: ", printHOAS t]
prettyPreElem (n,t)  = T.concat [" ", n, ": ", printHOAS t]

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
      , "  Checked term: ", printHOAS trm,"\n"
      , "  Against type: ", printHOAS typ, "\n"
      , "  Reduced type: ",  printHOAS typ',"\n"
      , "With context:\n"
      , prettyPre ctx
      ]
    NewNonSelfType ctx trm typ typ' -> T.concat
      ["The type of data must be a self: \n"
      , "  Checked term: ", printHOAS trm,"\n"
      , "  Against type: ", printHOAS typ, "\n"
      , "  Reduced type: ",  printHOAS typ',"\n"
      , "With context:\n"
      , prettyPre ctx
      ]
    UnboundVariable nam idx -> T.concat
      ["Unbound free variable: ", T.pack $ show nam, " at level ", T.pack $ show idx]
    UntypedLambda -> "Can't infer the type of a lambda"
    FoundHole (name,term) -> T.concat
      ["Can't fill hole: ?", T.pack $ show name, " : ", printHOAS term]
    NonFunctionApplication ctx trm typ typ' -> T.concat
      ["Tried to apply something that wasn't a lambda: \n"
      , "  Checked term: ", printHOAS trm,"\n"
      , "  Against type: ", printHOAS typ, "\n"
      , "  Reduced type: ",  printHOAS typ',"\n"
      , "With context:\n"
      , prettyCtx ctx
      ]
    NonSelfUse ctx trm typ typ' -> T.concat
      ["Tried to case on something that wasn't data: \n"
      , "  Checked term: ", printHOAS trm,"\n"
      , "  Against type: ", printHOAS typ, "\n"
      , "  Reduced type: ",  printHOAS typ',"\n"
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

expandConstant :: Constant -> HOAS
expandConstant t = case t of
  TUni -> LitH TUni
  CUni -> LitH CUni
  --TStr -> termToHoas $


