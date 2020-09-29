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

--import           Debug.Trace
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.ST
import           Control.Monad.ST.UnsafePerform

import           Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.STRef

import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Language.Yatima.Print
import           Language.Yatima.Term
import           Language.Yatima.Ctx (Ctx(..), (<|))
import qualified Language.Yatima.Ctx as Ctx

-- | Higher-Order Abstract Syntax
data HOAS where
  VarH :: Name -> Int -> HOAS
  RefH :: Name -> HOAS
  LamH :: Name -> (HOAS -> HOAS) -> HOAS
  AppH :: HOAS -> HOAS -> HOAS
  LetH :: Name -> Uses -> HOAS -> HOAS -> (HOAS -> HOAS) -> HOAS
  AllH :: Name -> Name -> Uses -> HOAS -> (HOAS -> HOAS -> HOAS) -> HOAS
  FixH :: Name -> (HOAS -> HOAS) -> HOAS
  AnnH :: Int  -> HOAS -> HOAS -> HOAS
  AnyH :: HOAS
  HolH :: Name -> HOAS

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
  Any                     -> AnyH
  Hol nam                 -> HolH nam
  Var nam                 -> maybe (VarH nam 0) id (Ctx.find nam ctx)
  Ref nam                 -> RefH nam
  Lam nam bod             -> LamH nam (bind nam bod)
  App fun arg             -> AppH (go fun) (go arg)
  Let nam use typ exp bod -> LetH nam use (go typ) (rec nam exp) (bind nam bod)
  All slf nam use typ bod -> AllH slf nam use (go typ) (bind2 slf nam bod)
  where
    go      t   = termToHoas ctx t
    bind  n t   = (\x   -> termToHoas ((n,x)<|ctx) t)
    bind2 n m t = (\s x -> termToHoas ((m,x)<|(n,s)<|ctx) t)
    rec n t = FixH n (bind n t)

-- | Convert a GHC higher-order representation to a lower-order one
hoasToTerm :: PreContext -> HOAS -> Term
hoasToTerm ctx t = case t of
  AnyH                     -> Any
  HolH nam                 -> Hol nam
  RefH nam                 -> Ref nam
  VarH nam idx             -> Var nam
  LamH nam bod             -> Lam nam (bind nam bod)
  AppH fun arg             -> App (go fun) (go arg)
  LetH nam use typ exp bod -> Let nam use (go typ) (go exp) (bind nam bod)
  AllH slf nam use typ bod -> All slf nam use (go typ) (bind2 slf nam bod)
  FixH nam bod             -> bind nam bod
  AnnH _   trm _           -> go trm
  where
    dep          = Ctx.depth ctx
    go t         = hoasToTerm ctx t
    bind n b     = hoasToTerm ((n,AnyH)<|ctx) (b (VarH n dep))
    bind2 s n b  =
      hoasToTerm ((n,AnyH)<|(s,AnyH)<|ctx) (b (VarH s dep) (VarH n (dep+1)))

printHOAS :: HOAS -> Text
printHOAS = prettyTerm . (hoasToTerm Ctx.empty)

instance Show HOAS where
 show t = T.unpack $ printHOAS t

defToHOAS :: Name -> Def -> (HOAS,HOAS)
defToHOAS name def =
  ( FixH name (\s -> termToHoas (Ctx.singleton (name,s)) (_term def))
  , FixH name (\s -> termToHoas (Ctx.singleton (name,s)) (_type def))
  )

newtype Hash  = Hash { _unHash :: ByteString } deriving (Eq,Ord) via ByteString
newtype HashF = HashF { _runHashF :: HOAS -> Hash }

whnf :: HOAS -> Defs -> HOAS
whnf trm defs = case trm of
  RefH nam       -> case defs M.!? nam of
    Just d  -> go $ fst (defToHOAS nam d)
    Nothing -> error $ "Undefined Reference " ++ (T.unpack nam)
  FixH nam bod       -> go (bod trm)
  AppH fun arg       -> case go fun of
    LamH _ bod -> go (bod arg)
    x          -> AppH fun arg
  AnnH _ a _   -> go a
  LetH _ _ _ exp bod -> go (bod exp)
  x                  -> x
  where
    go x = whnf x defs

-- | Normalize a HOAS term
norm :: HOAS -> Defs -> HOAS
norm term defs = case whnf term defs of
  AllH slf nam use typ bod -> AllH slf nam use (go typ) (\s x -> go (bod s x))
  LamH nam bod             -> LamH nam (\x -> go (bod x))
  AppH fun arg             -> AppH (go fun) (go arg)
  FixH nam bod             -> go (bod (FixH nam bod))
  step                     -> step
  where
    go x = norm x defs

equal :: HashF -> Defs -> HOAS -> HOAS -> Int -> Either Hole Bool
equal hashF defs a b lvl = go a b lvl Set.empty
  where
    go :: HOAS -> HOAS -> Int -> Set (Hash,Hash) -> Either Hole Bool
    go a b lvl seen = do
      let aWhnf = whnf a defs
      let bWhnf = whnf b defs
      let aHash = _runHashF hashF aWhnf
      let bHash = _runHashF hashF bWhnf
      if | (aHash == bHash)                -> return True
         | (aHash,bHash) `Set.member` seen -> return True
         | (bHash,aHash) `Set.member` seen -> return True
         | otherwise -> do
             let seen' = Set.insert (aHash,bHash) seen
             next aWhnf bWhnf lvl seen'

    next :: HOAS -> HOAS -> Int -> Set (Hash, Hash) -> Either Hole Bool
    next a b lvl seen = case (a, b) of
      (AllH aSlf aNam aUse aTyp aBod, AllH bSlf bNam bUse bTyp bBod) -> do
        let aBod' = aBod (VarH aSlf lvl) (VarH aNam (lvl + 1))
        let bBod' = bBod (VarH bSlf lvl) (VarH bNam (lvl + 1))
        let useEq = aUse == bUse
        typEq <- go aTyp bTyp lvl seen
        bodEq <- go aBod' bBod' (lvl+2) seen
        return $ useEq && typEq && bodEq
      (LamH aNam aBod, LamH bNam bBod) -> do
        let aBod' = aBod (VarH aNam lvl)
        let bBod' = bBod (VarH bNam lvl)
        go aBod' bBod' (lvl+1) seen
      (AppH aFun aArg, AppH bFun bArg) -> do
        funEq <- go aFun bFun lvl seen
        argEq <- go aArg bArg lvl seen
        return $ funEq && argEq
      (HolH name, b) -> Left (name, b)
      (a, HolH name) -> Left (name, a)
      (AnyH, b) -> return True
      (a, AnyH) -> return True
      _         -> return False

data CheckErr e
  = CheckQuantityMismatch Context (Name,Uses,HOAS) (Name,Uses,HOAS)
  | InferQuantityMismatch Context (Name,Uses,HOAS) (Name,Uses,HOAS)
  | TypeMismatch PreContext HOAS HOAS
  | UnboundVariable Name Int
  | UnboundAnnotation Int HOAS HOAS
  | FoundHole Hole
  | EmptyContext
  | UntypedLambda
  | UndefinedReference Name
  | LambdaNonFunctionType PreContext HOAS HOAS HOAS
  | NonFunctionApplication Context HOAS HOAS HOAS
  | CheckEnvironmentError Name e
  | CustomErr PreContext Text

-- | Fills holes of a term until it is complete
synth :: HashF -> Defs -> HOAS -> HOAS -> Except (CheckErr e) (HOAS, HOAS)
synth hashF defs term typ_ = do
  case runExcept (check hashF defs Ctx.empty Once term typ_) of
    Left (FoundHole hole) -> synth hashF defs (fill hole term) (fill hole typ_)
    Left error            -> throwError error
    Right (_,typ_)        -> return (term, typ_)

fill :: Hole -> HOAS -> HOAS
fill hole term = case term of
  AnyH                     -> AnyH
  VarH nam idx             -> VarH nam idx
  LamH nam bod             -> LamH nam (\x -> go (bod x))
  AllH slf nam use typ bod -> AllH slf nam use (go typ) (\s x -> go (bod s x))
  AppH fun arg             -> AppH (go fun) (go arg)
  HolH nam                 -> if fst hole == nam then snd hole else HolH nam
  where
    go x = fill hole x

-- * Type System

check :: HashF -> Defs -> PreContext -> Uses -> HOAS -> HOAS
      -> Except (CheckErr e) (Context, HOAS)
check hashF defs pre use term typ = case term of
  LamH name body -> case whnf typ defs of
    AllH self bindName bindUse bind typeBody -> do
      let bodyType = typeBody term (VarH name (Ctx.depth pre))
      let bodyTerm = body (VarH name (Ctx.depth pre))
      (bodyCtx,_) <- go ((name,bind) <| pre) Once bodyTerm bodyType
      case _ctx bodyCtx of
        Empty -> throwError $ EmptyContext
        ((name',(bindUse',bind')) :<| bodyCtx') -> do
          unless (bindUse' ≤# bindUse) (do
            let original = (name,bindUse,bind)
            let checked  = (name',use,bind')
            throwError (CheckQuantityMismatch (Ctx bodyCtx') original checked))
          return (mulCtx use (Ctx bodyCtx'),typ)
    x -> throwError $ LambdaNonFunctionType pre term typ x
  LetH name exprUse exprTyp expr body -> do
    (exprCtx,_) <- go pre exprUse expr exprTyp
    let var = VarH name (Ctx.depth pre)
    (bodyCtx,_) <- go ((name,exprTyp) <| pre) Once (body var) typ
    case _ctx bodyCtx of
      Empty -> throwError $ EmptyContext
      ((name',(exprUse',exprTyp')) :<| bodyCtx') -> do
        unless (exprUse' ≤# exprUse) (do
          let original = (name,exprUse,exprTyp)
          let checked  = (name',exprUse',exprTyp')
          throwError (CheckQuantityMismatch (Ctx bodyCtx') original checked))
        return $ (mulCtx use (addCtx exprCtx (Ctx bodyCtx')), typ)
  FixH name body -> do
    let unroll = body (AnnH (Ctx.depth pre) (FixH name body) typ)
    (bodyCtx,_) <- go ((name,typ) <| pre) use unroll typ
    case _ctx bodyCtx of
     Empty -> throwError $ EmptyContext
     (_,(None,_)) :<| bodyCtx' -> return (Ctx bodyCtx', typ)
     (_,(use,_))  :<| bodyCtx' -> return (mulCtx Many (Ctx bodyCtx'),typ)
  _ -> do
    (ctx, termTyp) <- infer hashF defs pre use term
    case equal hashF defs typ termTyp lvl of
      Left hole   -> throwError (FoundHole hole)
      Right False -> throwError (TypeMismatch pre typ termTyp)
      Right True  -> return (ctx, typ)
  where
    go = check hashF defs
    typWhnf = whnf typ defs
    lvl = Ctx.depth pre

-- | Infers the type of a term
infer :: HashF -> Defs -> PreContext -> Uses -> HOAS
      -> Except (CheckErr e) (Context, HOAS)
infer hashF defs pre use term = case term of
  VarH nam lvl -> case Ctx.adjust lvl (toContext pre) (\(_,typ) -> (use,typ)) of
    Nothing            -> throwError $ UnboundVariable nam lvl
    Just ((_,typ),ctx) -> return (ctx, typ)
  RefH n -> do
    let mapMaybe = maybe (throwError $ UndefinedReference n) pure
    def         <- mapMaybe (defs M.!? n)
    let (_,typ) = (defToHOAS n def)
    return (toContext pre,typ)
  LamH name body -> throwError $ UntypedLambda
  AppH func argm -> do
    (funcCtx, funcTyp) <- go pre use func
    case whnf funcTyp defs of
      AllH _ _ argmUse bind body -> do
        (argmCtx,_) <- check' pre (argmUse *# use) argm bind
        return (addCtx funcCtx argmCtx, body func argm)
      x -> throwError $ NonFunctionApplication funcCtx func funcTyp x
  AllH self name bindUse bind body -> do
    let selfVar = VarH self $ Ctx.depth pre
    let nameVar = VarH name $ Ctx.depth pre + 1
    check' pre None bind AnyH
    check' ((name,bind) <| (self,term) <| pre) None (body selfVar nameVar) AnyH
    return (toContext pre, AnyH)
  LetH name exprUse exprTyp expr body -> do
    (exprCtx,_)    <- check' pre exprUse expr exprTyp
    let var = VarH name (Ctx.depth pre)
    (bodyCtx, typ) <- go ((name,exprTyp) <| pre) Once (body var)
    case _ctx bodyCtx of
      Empty -> throwError EmptyContext
      ((name',(exprUse',exprTyp')) :<| bodyCtx') -> do
        unless (exprUse' ≤# exprUse) (do
          let original = (name,exprUse,exprTyp)
          let inferred = (name',exprUse',exprTyp')
          throwError (InferQuantityMismatch (Ctx bodyCtx') original inferred))
        return (mulCtx use (addCtx exprCtx (Ctx bodyCtx')), typ)
  -- Hole
  AnyH           -> return (toContext pre, AnyH)
  AnnH lvl val typ -> do
    case Ctx.adjust lvl (toContext pre) (\(_,typ) -> (use,typ)) of
      Nothing -> throwError $ UnboundAnnotation lvl val typ
      Just ((_,typ),ctx) -> return (ctx, typ)
  (HolH name) -> return (toContext pre, AnyH)
  _ -> throwError $ CustomErr pre "can't infer type"
  where
    go     = infer hashF defs
    check' = check hashF defs

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

instance Show e => Show (CheckErr e) where
  show e = case e of
    CheckQuantityMismatch ctx a b -> concat
      ["Type checking quantity mismatch: \n"
      , "- Expected type:  ", T.unpack $ prettyCtxElem a, "\n"
      , "- Instead found: ", T.unpack $ prettyCtxElem b, "\n"
      , "With context:\n"
      , T.unpack $ prettyCtx ctx
      , "\n"
      ]
    InferQuantityMismatch ctx a b -> concat
      ["Type inference quantity mismatch: \n"
      , "- Expected type: ", T.unpack $ prettyCtxElem a, "\n"
      , "- But inferred:  ", T.unpack $ prettyCtxElem b, "\n"
      , "With context:\n"
      , T.unpack $ prettyCtx ctx
      , "\n"
      ]
    TypeMismatch ctx a b -> concat
      ["Type Mismatch: \n"
      , "- Expected type:  ", T.unpack $ prettyPreElem ("",a), "\n"
      , "- Instead, found: ", T.unpack $ prettyPreElem ("",b), "\n"
      , "With context:\n"
      , T.unpack $ prettyPre ctx
      , "\n"
      ]
    LambdaNonFunctionType ctx trm typ typ' -> concat
      ["The type of a lambda must be a forall: \n"
      , "  Checked term: ", T.unpack $ printHOAS trm,"\n"
      , "  Against type: ", T.unpack $ printHOAS typ, "\n"
      , "  Reduced type: ",  T.unpack $ printHOAS typ',"\n"
      , "With context:\n"
      , T.unpack $ prettyPre ctx
      , "\n"
      ]
    UnboundVariable nam idx -> concat
      ["Unbound free variable: ", show nam, " at level ", show idx]
    UnboundAnnotation lvl val typ -> concat
      ["Unbound annotation level: " , show lvl
      , " in annotation", T.unpack $ printHOAS val
      , " : ", T.unpack $ printHOAS typ
      ]
    UntypedLambda -> "Can't infer the type of a lambda"
    FoundHole (name,term) -> concat
      ["Can't fill hole: ?", show name, " : ", T.unpack $ printHOAS term]
    NonFunctionApplication ctx trm typ typ' -> concat
      ["Tried to apply something that wasn't a lambda: \n"
      , "  Checked term: ", T.unpack $ printHOAS trm,"\n"
      , "  Against type: ", T.unpack $ printHOAS typ, "\n"
      , "  Reduced type: ",  T.unpack $ printHOAS typ',"\n"
      , "With context:\n"
      , T.unpack $ prettyCtx ctx
      , "\n"
      ]
    EmptyContext -> "Empty Context"
    CheckEnvironmentError name e -> concat
      ["Environment error: \n"
      , "Name: ", show name, "\n"
      , show e, "\n"
      , "\n"
      ]
    UndefinedReference name -> concat
      ["UndefinedReference error: \n"
      , "Name: ", show name, "\n"
      ]
    CustomErr ctx txt -> concat
      ["Custom Error:\n"
      , T.unpack txt,"\n"
      , "With context:\n"
      , T.unpack $ prettyPre ctx
      , "\n"
      ]

