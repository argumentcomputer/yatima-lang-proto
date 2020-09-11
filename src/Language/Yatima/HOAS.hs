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
  , checkFile
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

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.ST
import           Control.Monad.ST.UnsafePerform

import           Data.Map                   (Map)
import qualified Data.Map                   as M
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

deriving instance Show LOAS

toLOAS :: Term -> [Name] -> Defs -> Except DerefErr LOAS
toLOAS trm ctx defs = case trm of
  Var nam                 ->
    case find nam ctx of
      Just idx -> return $ VarL nam idx
      _        -> throwError $ FreeVariable nam ctx
  Ref nam                 -> RefL nam <$> anonymizeRef nam defs
  Lam nam ann bod         ->
    case ann of
      Just (use,typ) -> LamL nam <$> (Just . (use,) <$> go typ) <*> bind nam bod
      Nothing        -> LamL nam Nothing <$> bind nam bod
  App fun arg             -> AppL <$> go fun <*> go arg
  Let nam use typ exp bod ->
    LetL nam use <$> go typ <*> bind nam exp <*> bind nam bod
  Typ                     -> return TypL
  All slf nam use typ bod -> AllL slf nam use <$> go typ <*> bind2 slf nam bod
  where
    go t        = toLOAS t ctx defs
    bind    n t = toLOAS t (n:ctx) defs
    bind2 s n t = toLOAS t (n:s:ctx) defs

-- | Convert a GHC higher-order representation to a lower-order one
fromLOAS :: LOAS -> Term
fromLOAS t = case t of
  VarL nam _               -> Var nam
  RefL nam _               -> Ref nam
  LamL nam ann bod         -> Lam nam (fmap go <$> ann) (go bod)
  AppL fun arg             -> App (go fun) (go arg)
  LetL nam use typ exp bod -> Let nam use (go typ) (go exp) (go bod)
  AllL slf nam use typ bod -> All slf nam use (go typ) (go bod)
  TypL                     -> Typ
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
findCtx idx ctx = go ctx 0
  where
    go (c:cs) i
      | idx == i   = Just c
      | otherwise = go cs (i+1)
    go [] _      = Nothing

-- | Convert a lower-order `Term` to a GHC higher-order one
toHOAS :: LOAS -> [HOAS] -> Int -> HOAS
toHOAS t ctx dep = case t of
  VarL nam idx             -> maybe (VarH nam (dep-idx-1)) id (findCtx idx ctx)
  RefL nam cid             -> RefH nam cid
  LamL nam ann bod         -> LamH nam (fmap go <$> ann) (bind bod)
  AppL fun arg             -> AppH (go fun) (go arg)
  LetL nam use typ exp bod -> LetH nam use (go typ) (rec nam exp) (bind bod)
  AllL slf nam use typ bod -> AllH slf nam use (go typ) (bind2 bod)
  TypL                     -> TypH
  where
    go    t =          toHOAS t ctx       dep
    bind  t = (\x   -> toHOAS t (x:ctx)   (dep + 1))
    bind2 t = (\s x -> toHOAS t (x:s:ctx) (dep + 2))
    rec n t = FixH n (bind t)

-- | Convert a GHC higher-order representation to a lower-order one
fromHOAS :: HOAS -> Int -> LOAS
fromHOAS t dep = case t of
  VarH nam idx             -> VarL nam idx
  LamH nam ann bod         -> LamL nam (fmap go <$> ann) (unbind nam bod)
  AppH fun arg             -> AppL (go fun) (go arg)
  RefH nam cid             -> RefL nam cid
  LetH nam use typ exp bod -> LetL nam use (go typ) (go exp) (unbind nam bod)
  AllH slf nam use typ bod -> AllL slf nam use (go typ) (unbind2 slf nam bod)
  TypH                     -> TypL
  FixH nam bod             -> unbind nam bod
  where
    go          t = fromHOAS t                                 dep
    unbind    n b = fromHOAS (b (VarH n dep))                  (dep + 1)
    unbind2 s n b = fromHOAS (b (VarH s dep) (VarH n (dep+1))) (dep + 2)

termFromHOAS :: HOAS -> Term
termFromHOAS t = fromLOAS $ fromHOAS t 0

anonymizeHOAS:: HOAS -> Int -> Anon
anonymizeHOAS h dep = case h of
  VarH nam idx             -> VarA idx
  LamH nam ann bod         -> LamA (fmap go <$> ann) (unbind nam bod)
  AppH fun arg             -> AppA (go fun) (go arg)
  RefH nam cid             -> RefA cid
  LetH nam use typ exp bod -> LetA use (go typ) (go exp) (unbind nam bod)
  AllH slf nam use typ bod -> AllA use (go typ) (unbind2 slf nam bod)
  TypH                     -> TypA
  FixH nam bod             -> unbind nam bod
  where
    go          t = anonymizeHOAS t                                 dep
    unbind    n b = anonymizeHOAS (b (VarH n dep))                  (dep + 1)
    unbind2 s n b = anonymizeHOAS (b (VarH s dep) (VarH n (dep+1))) (dep + 2)

defToHOAS :: Name -> Def -> Defs -> Except DerefErr (HOAS,HOAS)
defToHOAS name def ds = do
  term <- toLOAS (_term def) [name] ds
  typ_ <- toLOAS (_type def) [] ds
  return (FixH name (\s -> toHOAS term [s] 1), toHOAS typ_ [] 0)

-- | Pretty-print a `HOAS`
printHOAS :: HOAS -> Text
printHOAS = prettyTerm . termFromHOAS

instance Show HOAS where
  show t = T.unpack $ printHOAS t

derefHOAS :: Name -> CID -> Defs -> Except DerefErr HOAS
derefHOAS name cid ds = do
  def  <- deref name cid ds
  loas <- toLOAS (_term def) [name] ds
  return $ FixH name (\s -> toHOAS loas [s] 1)

-- * Evaluation

-- | Reduce a HOAS to weak-head-normal-form
whnf :: HOAS -> Defs -> HOAS
whnf trm defs = case trm of
  FixH nam bod       -> go (bod trm)
  RefH nam cid       ->
    case runExcept (derefHOAS nam cid defs) of
      Right trm  -> go trm
      Left  err  -> error $ "BAD: Runtime DerefErr: " ++ show err
  AppH fun arg       -> case go fun of
    LamH _ _ bod -> go (bod arg)
    x            -> AppH fun arg
  LetH _ _ _ exp bod -> go (bod exp)
  x                  -> x
  where
    go x = whnf x defs

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
      LamH nam ann bod         -> do
        typ' <- case ann of
          Just (use,typ) -> Just . (use,) <$> go typ seen
          _              -> return Nothing
        return $ LamH nam typ' (\x -> unsafe $ go (bod x) seen)
      AllH slf nam use typ bod -> do
        typ' <- go typ seen
        return $ AllH slf nam use typ' (\s x -> unsafe $ go (bod s x) seen)
      AppH fun arg             -> AppH <$> (go fun seen) <*> (go arg seen)
      _                        -> return step
      where
        unsafe = unsafePerformST


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
      let a' = whnf a defs
      let b' = whnf b defs
      let aCID = makeCID $ anonymizeHOAS a' 0
      let bCID = makeCID $ anonymizeHOAS b' 0
      s' <- readSTRef seen
      if | (aCID == bCID)              -> return True
         | (aCID,bCID) `Set.member` s' -> return True
         | (bCID,aCID) `Set.member` s' -> return True
         | otherwise -> do
             modifySTRef' seen (Set.insert (aCID,bCID))
             modifySTRef' seen (Set.insert (bCID,aCID))
             next a' b' dep seen

    next :: HOAS -> HOAS -> Int -> STRef s (Set (CID,CID)) -> ST s Bool
    next a b dep seen = case (a,b) of
     (AllH aSlf aNam aUse aTyp aBod, AllH bSlf bNam bUse bTyp bBod) -> do
       let aBod' = aBod (VarH aSlf dep) (VarH aNam (dep + 1))
       let bBod' = bBod (VarH bSlf dep) (VarH bNam (dep + 1))
       let useEq = aUse == bUse
       typEq <- go aTyp bTyp dep seen
       bodEq <- go aBod' bBod' (dep+2) seen
       return $ useEq && typEq && bodEq
     (LamH aNam _ aBod, LamH bNam _ bBod) -> do
       let aBod' = aBod (VarH aNam dep)
       let bBod' = bBod (VarH bNam dep)
       go aBod' bBod' (dep+1) seen
     (AppH aFun aArg, AppH bFun bArg) -> do
       funEq <- go aFun bFun dep seen
       argEq <- go aArg bArg dep seen
       return $ funEq && argEq
     (LetH _ _ _ aExp aBod, LetH _ _ _ bExp bBod) -> do
       let aBod' = aBod aExp
       let bBod' = bBod bExp
       bodEq <- go aBod' bBod' (dep+1) seen
       return $ bodEq
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
  FixH name body -> do
    let var = VarH name (length ctx)
    let ctx' = (ctx |> (None,typ))
    ctx' <- check ctx' ρ (body var) typ defs
    case viewr ctx' of
      EmptyR -> throwError $ EmptyContext ctx
      ctx :> (π, _) -> do
        if π == None
          then return ctx
          else return $ multiplyCtx Many ctx
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
    def        <- mapE (deref n c defs)
    (_,typ)    <- mapE (defToHOAS n def defs)
    return (ctx,typ)
  AppH func argm -> do
    (ctx', funcType) <- infer ctx ρ func defs
    case whnf funcType defs of
      AllH _ _ π bind body -> do
        ctx'' <- check ctx (ρ *# π) argm bind defs
        return (addCtx ctx' ctx'', body func argm)
      x -> throwError $ NonFunctionApplication ctx func x
  AllH self name pi bind body -> do
    let self_var = VarH self $ length ctx
    let name_var = VarH name $ length ctx + 1
    let ctx'     = ctx |> (None, term) |> (None, bind)
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

checkRef :: Name -> CID -> Defs -> Except CheckErr ()
checkRef name cid defs = do
  let ctx = Seq.empty
  let mapE = mapExcept (either (\e -> throwError $ DerefError ctx name cid e) pure)
  def  <- mapE $ derefMetaDefCID name cid defs
  (trm,typ) <- mapE $ defToHOAS name def defs
  check ctx Once trm typ defs
  return ()

checkFile :: FilePath -> IO ()
checkFile file = do
  defs <- pFile file
  let func :: (Name, CID) -> IO ()
      func (name, cid) = do
        case runExcept $ checkRef name cid defs of
          Left  e -> putStrLn $ T.unpack $ T.concat [name, " ✗", " (", T.pack $ show e, ")"]
          Right _ -> putStrLn $ T.unpack $ T.concat [name, " ✓"]
  forM_ (M.toList $ _index defs) func
