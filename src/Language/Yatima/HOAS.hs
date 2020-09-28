{-|
Module      : Language.Yatima.HOAS
Description : Evaluate and typecheck exprassions in the Yatima Language using
higher-order-abstract-syntax
Copyright   : (c) Sunshine Cybernetics, 2020
License     : GPL-3
Maintainer  : john@sunshinecybernetics.com
Stability   : experimental
-}
{-# LANGUAGE DerivingVia #-}
module Language.Yatima.HOAS
  ( HOAS(..)
  , Ctx
  , findCtx
  , atCtx
  , toHOAS
  , fromHOAS
  , defToHOAS
  , printHOAS
  , whnf
  , Hash(..)
  , HashF(..)
  , norm
  , equal
  , multiplyCtx
  , addCtx
  , prettyUses
  , prettyCtx
  , prettyCtxElem
  , CheckErr(..)
  , check
  , infer
  ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.ST
import           Control.Monad.ST.UnsafePerform
import           Debug.Trace

import           Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Sequence (Seq(..), ViewL(..), ViewR(..), viewr, viewl, (|>), (<|))
import qualified Data.Sequence as Seq
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.STRef

import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Language.Yatima.Print
import           Language.Yatima.Term

-- | Higher-Order Abstract Syntax
data HOAS where
  VarH :: Name -> Int -> HOAS
  RefH :: Name -> HOAS
  LamH :: Name -> (HOAS -> HOAS) -> HOAS
  AppH :: HOAS -> HOAS -> HOAS
  LetH :: Name -> Uses -> HOAS -> HOAS -> (HOAS -> HOAS) -> HOAS
  AllH :: Name -> Name -> Uses -> HOAS -> (HOAS -> HOAS -> HOAS) -> HOAS
  TypH :: HOAS
  FixH :: Name -> (HOAS -> HOAS) -> HOAS
  AnnH :: Int  -> HOAS -> HOAS -> HOAS

type Ctx  = Seq (Uses,Name,HOAS)

-- | Find a term in a context
findCtx :: Name -> Ctx -> Maybe HOAS
findCtx nam ((_,n,t) :<| cs)
  | n == nam   = Just t
  | otherwise  = findCtx nam cs
findCtx nam Empty = Nothing

atCtx :: Int -> Ctx -> Maybe (Uses,Name,HOAS)
atCtx lvl ctx = go (Seq.length ctx - lvl - 1) ctx
  where
    go :: Int -> Ctx -> Maybe (Uses,Name, HOAS)
    go 0 (x :<| xs) = Just x
    go i (x :<| xs) = go (i - 1) xs
    go i Empty      = Nothing

-- | Convert a lower-order `Term` to a GHC higher-order one
toHOAS :: Ctx -> Term -> HOAS
toHOAS ctx t = case t of
  Var nam                 -> maybe (VarH nam 0) id (findCtx nam ctx)
  Ref nam                 -> RefH nam
  Lam nam bod             -> LamH nam (bind nam bod)
  App fun arg             -> AppH (go fun) (go arg)
  Let nam use typ exp bod -> LetH nam use (go typ) (rec nam exp) (bind nam bod)
  All slf nam use typ bod -> AllH slf nam use (go typ) (bind2 slf nam bod)
  Typ                     -> TypH
  where
    go      t   = toHOAS ctx t
    bind  n t   = (\x   -> toHOAS ((Many,n,x):<|ctx) t)
    bind2 n m t = (\s x -> toHOAS ((Many,m,x):<|(Many,n,s):<|ctx) t)
    rec n t = FixH n (bind n t)

-- | Convert a GHC higher-order representation to a lower-order one
fromHOAS :: Ctx -> HOAS -> Term
fromHOAS ctx t = case t of
  VarH nam idx             -> Var nam
  LamH nam bod             -> Lam nam (bind nam bod)
  AppH fun arg             -> App (go fun) (go arg)
  RefH nam                 -> Ref nam
  LetH nam use typ exp bod -> Let nam use (go typ) (go exp) (bind nam bod)
  AllH slf nam use typ bod -> All slf nam use (go typ) (bind2 slf nam bod)
  TypH                     -> Typ
  FixH nam bod             -> bind nam bod
  AnnH _   trm _           -> go trm
  where
    dep          = Seq.length ctx
    go t         = fromHOAS ctx t
    f n          = (Many,n,TypH)
    bind n b     = fromHOAS (f n:<|ctx) (b (VarH n dep))
    bind2 s n b  = fromHOAS (f n:<|f s:<|ctx) (b (VarH s dep) (VarH n (dep+1)))


defToHOAS :: Name -> Def -> (HOAS,HOAS)
defToHOAS name def =
  ( FixH name (\s -> toHOAS (Seq.singleton (Many,name,s)) (_term def))
  , FixH name (\s -> toHOAS (Seq.singleton (Many,name,s)) (_type def))
  )

---- | Pretty-print a `HOAS`
printHOAS :: HOAS -> Text
printHOAS = prettyTerm . (fromHOAS Seq.empty)

instance Show HOAS where
 show t = T.unpack $ printHOAS t

---- * Evaluation
-- | Reduce a HOAS to weak-head-normal-form
whnf :: HOAS -> Defs -> HOAS
whnf trm defs = case trm of
  RefH nam       -> case defs M.!? nam of
    Just d  -> fst (defToHOAS nam d)
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


newtype Hash  = Hash { _unHash :: ByteString } deriving (Eq,Ord) via ByteString
newtype HashF = HashF { _runHashF :: HOAS -> Hash }

-- | Normalize a HOAS term
norm :: HashF -> HOAS -> Defs -> HOAS
norm hashF term defs = runST (top $ term)
  where
    top :: HOAS -> ST s HOAS
    top term = do
      seen <- newSTRef (Set.empty)
      go term seen

    go :: HOAS -> (STRef s (Set Hash)) -> ST s HOAS
    go term seen = do
      let step = whnf term defs
      let termHash = _runHashF hashF term
      let stepHash = _runHashF hashF step
      seenSet <- readSTRef seen
      if | termHash `Set.member` seenSet -> return step
         | stepHash `Set.member` seenSet -> return step
         | otherwise -> do
             modifySTRef' seen ((Set.insert termHash) . (Set.insert stepHash))
             next step seen

    next :: HOAS -> (STRef s (Set Hash)) -> ST s HOAS
    next step seen = case step of
      LamH nam bod         -> return $ LamH nam (\x -> unsafe $ go (bod x) seen)
      AllH slf nam use typ bod -> do
        typ' <- go typ seen
        return $ AllH slf nam use typ' (\s x -> unsafe $ go (bod s x) seen)
      AppH fun arg             -> AppH <$> (go fun seen) <*> (go arg seen)
      FixH n b                 -> go (b step) seen
      _                        -> return step
      where
        unsafe = unsafePerformST

-- * Type-checking

equal :: HashF -> HOAS -> HOAS -> Defs -> Int -> Bool
equal hashF a b defs dep = runST $ top a b dep
  where
    top :: HOAS -> HOAS -> Int -> ST s Bool
    top a b dep = do
--      traceM $ show a
--      traceM $ show b
      seen <- newSTRef (Set.empty)
      go a b dep seen

    go :: HOAS -> HOAS -> Int -> STRef s (Set (Hash,Hash)) -> ST s Bool
    go a b dep seen = do
--      traceM $ show a
--      traceM $ show b
      let a' = whnf a defs
      let b' = whnf b defs
      let aHash = _runHashF hashF a'
      let bHash = _runHashF hashF b'
      s' <- readSTRef seen
      if | (aHash == bHash)              -> return True
         | (aHash,bHash) `Set.member` s' -> return True
         | (bHash,aHash) `Set.member` s' -> return True
         | otherwise -> do
             modifySTRef' seen (Set.insert (aHash,bHash))
             modifySTRef' seen (Set.insert (bHash,aHash))
             next a' b' dep seen

    next :: HOAS -> HOAS -> Int -> STRef s (Set (Hash,Hash)) -> ST s Bool
    next a b dep seen = case (a,b) of
     (AllH aSlf aNam aUse aTyp aBod, AllH bSlf bNam bUse bTyp bBod) -> do
       let aBod' = aBod (VarH aSlf dep) (VarH aNam (dep + 1))
       let bBod' = bBod (VarH bSlf dep) (VarH bNam (dep + 1))
       let useEq = aUse == bUse
       typEq <- go aTyp bTyp dep seen
       bodEq <- go aBod' bBod' (dep+2) seen
       return $ useEq && typEq && bodEq
     (LamH aNam aBod, LamH bNam bBod) -> do
       let aBod' = aBod (VarH aNam dep)
       let bBod' = bBod (VarH bNam dep)
       go aBod' bBod' (dep+1) seen
     (AppH aFun aArg, AppH bFun bArg) -> do
       funEq <- go aFun bFun dep seen
       argEq <- go aArg bArg dep seen
       return $ funEq && argEq
     _ -> return False

prettyCtx :: Ctx -> Text
prettyCtx ctx = foldr go "" ctx 
  where
    go :: (Uses,Name,HOAS) -> Text -> Text
    go x txt = T.concat [txt, "\n", " - ", prettyCtxElem x]

prettyUses :: Uses -> Text
prettyUses None = "0"
prettyUses Affi = "&"
prettyUses Once = "1"
prettyUses Many = "ω"

prettyCtxElem :: (Uses, Name, HOAS) -> Text
prettyCtxElem (u,"",t) = T.concat [prettyUses u, " _: ", printHOAS t]
prettyCtxElem (u,n,t)  = T.concat [prettyUses u , " ", n, ": ", printHOAS t]

multiplyCtx :: Uses -> Ctx -> Ctx
multiplyCtx rho ctx = if rho == Once then ctx else fmap mul ctx
  where mul (pi, nam, typ) = (rho *# pi,nam,typ)

-- Assumes both context are compatible (different only by quantities)
addCtx :: Ctx -> Ctx -> Ctx
addCtx ctx ctx' = case viewl ctx of
  EmptyL          -> ctx'
  (π, nam,typ) :< ctx -> case viewl ctx' of
    EmptyL          -> (π,nam,typ) <| ctx
    (π', _,_) :< ctx' -> (π +# π',nam,typ) <| addCtx ctx ctx'

data CheckErr e
  = QuantityMismatch Ctx (Uses,Name,HOAS) (Uses,Name,HOAS)
  | TypeMismatch Ctx     (Uses,Name,HOAS) (Uses,Name,HOAS)
  | EmptyContext
  | UndefinedReference Ctx Name
  | LambdaNonFunctionType  Ctx HOAS HOAS HOAS
  | NonFunctionApplication Ctx HOAS HOAS HOAS
  | CustomErr Ctx Text
  | CheckEnvironmentError Ctx Name e

instance Show e => Show (CheckErr e) where
  show e = case e of
    QuantityMismatch ctx a b -> concat
      ["Quantity Mismatch: \n"
      , "- Expected type:  ", T.unpack $ prettyCtxElem a, "\n"
      , "- Instead, found: ", T.unpack $ prettyCtxElem b, "\n"
      , "With context:"
      , T.unpack $ prettyCtx ctx
      , "\n"
      ]
    TypeMismatch ctx a b -> concat
      ["Type Mismatch: \n"
      , "- Expected type:  ", T.unpack $ prettyCtxElem a, "\n"
      , "- Instead, found: ", T.unpack $ prettyCtxElem b, "\n"
      , "With context:"
      , T.unpack $ prettyCtx ctx
      , "\n"
      ]
    LambdaNonFunctionType ctx trm typ typ' -> concat
      ["The type of a lambda must be a forall: \n"
      , "  Checked term: ", T.unpack $ printHOAS trm,"\n"
      , "  Against type: ", T.unpack $ printHOAS typ, "\n"
      , "  Reduced type: ",  T.unpack $ printHOAS typ',"\n"
      , "With context:"
      , T.unpack $ prettyCtx ctx
      , "\n"
      ]
    NonFunctionApplication ctx trm typ typ' -> concat
      ["Tried to apply something that wasn't a lambda: \n"
      , "  Checked term: ", T.unpack $ printHOAS trm,"\n"
      , "  Against type: ", T.unpack $ printHOAS typ, "\n"
      , "  Reduced type: ",  T.unpack $ printHOAS typ',"\n"
      , "With context:"
      , T.unpack $ prettyCtx ctx
      , "\n"
      ]
    EmptyContext -> "Empty Context"
    CheckEnvironmentError ctx name e -> concat
      ["Environment error: \n"
      , "Name: ", show name, "\n"
      , show e, "\n"
      , "With context:"
      , T.unpack $ prettyCtx ctx
      , "\n"
      ]
    UndefinedReference ctx name -> concat
      ["UndefinedReference error: \n"
      , "Name: ", show name, "\n"
      , "With context:"
      , T.unpack $ prettyCtx ctx
      , "\n"
      ]
    CustomErr ctx txt -> concat
      ["Custom Error:\n"
      , T.unpack txt,"\n"
      , "With context:"
      , T.unpack $ prettyCtx ctx
      , "\n"
      ]

check :: HashF -> Ctx -> Uses -> HOAS -> HOAS -> Defs -> Except (CheckErr e) Ctx
check hashF pre ρ trm typ defs = case trm of
  LamH name termBody -> case whnf typ defs of
    AllH s n π bind typeBody -> do
      let var      = VarH name (length pre)
      let bodyType = typeBody trm var
      let bodyTerm = termBody var
      bodyCtx <- check hashF (pre |> (None,name,bind)) Once bodyTerm bodyType defs
      case viewr bodyCtx of
        EmptyR -> throwError $ EmptyContext
        bodyCtx' :> (π',n',b') -> do
          unless (π' ≤# π)
            (throwError (QuantityMismatch bodyCtx (π',n',b') (π,n,bind)))
          return $ multiplyCtx ρ bodyCtx'
    x -> throwError $ LambdaNonFunctionType pre trm typ x
  LetH name π exprType expr body -> do
    exprCtx <- check hashF pre π expr exprType defs
    let var = VarH name (length pre)
    bodyCtx <- check hashF (pre |> (None,name,exprType)) Once (body var) typ defs
    case viewr bodyCtx of
      EmptyR -> throwError $ EmptyContext
      bodyCtx' :> (π',n',b') -> do
        unless (π' ≤# π)
          (throwError (QuantityMismatch bodyCtx' (π',n',b') (π,name,exprType)))
        return $ multiplyCtx ρ (addCtx exprCtx bodyCtx')
  FixH name body -> do
    let idx    = length pre
    let var    = VarH name idx
    let unroll = body (AnnH idx (FixH name body) typ)
    bodyCtx <- check hashF (pre |> (None,name,typ)) ρ unroll typ defs
    case viewr bodyCtx of
      EmptyR -> throwError $ EmptyContext
      bodyCtx' :> (π,_,_) -> do
        if π == None
          then return bodyCtx'
          else return $ multiplyCtx Many bodyCtx'
  _ -> do
    (ctx, infr) <- infer hashF pre ρ trm defs
    if equal hashF typ infr defs (length pre)
      then return ctx
      else throwError (TypeMismatch ctx (ρ,"",typ) (Many,"",infr))

infer :: HashF -> Ctx -> Uses -> HOAS -> Defs -> Except (CheckErr e) (Ctx, HOAS)
infer hashF pre ρ term defs = case term of
  VarH n idx -> do
    let (_,_,typ) = Seq.index pre idx
    let ctx = Seq.update idx (ρ,n,typ) pre
    return (ctx, typ)
  RefH n -> do
    let mapMaybe = maybe (throwError $ UndefinedReference pre n) pure
    def         <- mapMaybe (defs M.!? n)
    let (_,typ) = (defToHOAS n def)
    return (pre,typ)
  AppH func argm -> do
    (funcCtx, funcType) <- infer hashF pre ρ func defs
    case whnf funcType defs of
      AllH _ _ π bind body -> do
        argmCtx <- check hashF pre (ρ *# π) argm bind defs
        return (addCtx funcCtx argmCtx, body func argm)
      x -> throwError $ NonFunctionApplication funcCtx func funcType x
  AllH self name π bind body -> do
    check hashF pre  None bind (TypH) defs
    let self_var = VarH self $ length pre
    let name_var = VarH name $ length pre + 1
    let pre'     = pre |> (None,self,term) |> (None,name,bind)
    check hashF pre' None (body self_var name_var) (TypH) defs
    return (pre, TypH)
  LetH name π exprType expr body -> do
    exprCtx <- check hashF pre π expr exprType defs
    let var = VarH name (length pre)
    let pre' = (pre |> (None, name,exprType))
    (bodyCtx, typ) <- infer hashF pre' Once (body var) defs
    case viewr bodyCtx of
      EmptyR                -> throwError EmptyContext
      bodyCtx' :> (π',n',b') -> do
        unless (π' ≤# π)
          (throwError (QuantityMismatch bodyCtx' (π',n',b') (π,name,exprType)))
        return (multiplyCtx ρ (addCtx exprCtx bodyCtx'), typ)
  TypH -> return (pre, TypH)
  AnnH idx val typ -> do
    let ctx = Seq.update idx (ρ, "", typ) pre
    return (ctx, typ)
  _ -> throwError $ CustomErr pre "can't infer type"

