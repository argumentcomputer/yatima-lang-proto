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
import           Data.Sequence (Seq, ViewL(..), ViewR(..), viewr, viewl, (|>), (<|))
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

toLOAS :: Term -> [Name] -> Index -> Cache -> Except DerefErr LOAS
toLOAS trm ctx index cache = case trm of
  Var nam                 ->
    case find nam ctx of
      Just idx -> return $ VarL nam idx
      _        -> throwError $ FreeVariable nam ctx
  Ref nam                 -> RefL nam <$> anonymizeRef nam index cache
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
    go t        = toLOAS t ctx index cache
    bind    n t = toLOAS t (n:ctx) index cache
    bind2 s n t = toLOAS t (n:s:ctx) index cache

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
  AnnH :: Int  -> HOAS -> HOAS -> HOAS

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
  AnnH _   trm _           -> go trm
  where
    go          t = fromHOAS t                                 dep
    unbind    n b = fromHOAS (b (VarH n dep))                  (dep + 1)
    unbind2 s n b = fromHOAS (b (VarH s dep) (VarH n (dep+1))) (dep + 2)

termFromHOAS :: HOAS -> Term
termFromHOAS t = fromLOAS $ fromHOAS t 0

anonymizeHOAS:: HOAS -> Int -> Anon
anonymizeHOAS hoas dep = case hoas of
  VarH nam idx             -> VarA idx
  LamH nam ann bod         -> LamA (fmap go <$> ann) (unbind nam bod)
  AppH fun arg             -> AppA (go fun) (go arg)
  RefH nam cid             -> RefA cid
  LetH nam use typ exp bod -> LetA use (go typ) (go exp) (unbind nam bod)
  AllH slf nam use typ bod -> AllA use (go typ) (unbind2 slf nam bod)
  TypH                     -> TypA
  FixH nam bod             -> unbind nam bod
  AnnH _   trm _           -> go trm
  where
    go          t = anonymizeHOAS t                                 dep
    unbind    n b = anonymizeHOAS (b (VarH n dep))                  (dep + 1)
    unbind2 s n b = anonymizeHOAS (b (VarH s dep) (VarH n (dep+1))) (dep + 2)

defToHOAS :: Name -> Def -> Index -> Cache -> Except DerefErr (HOAS,HOAS)
defToHOAS name def index cache = do
  term <- toLOAS (_term def) [name] index cache
  typ_ <- toLOAS (_type def) [name] index cache
  let term' = FixH name (\s -> toHOAS term [s] 1)
  let typ_' = FixH name (\s -> toHOAS typ_ [s] 1)
  return (term',typ_')

-- | Pretty-print a `HOAS`
printHOAS :: HOAS -> Text
printHOAS = prettyTerm . termFromHOAS


instance Show HOAS where
  --show t = show (anonymizeHOAS t 0)
 show t = T.unpack $ printHOAS t

derefHOAS :: Name -> CID -> Index -> Cache -> Except DerefErr HOAS
derefHOAS name cid index cache = do
  def  <- deref name cid index cache
  loas <- toLOAS (_term def) [name] index cache
  return $ FixH name (\s -> toHOAS loas [s] 1)

-- * Evaluation

-- | Reduce a HOAS to weak-head-normal-form
whnf :: HOAS -> Index -> Cache -> HOAS
whnf trm index cache = case trm of
  RefH nam cid       ->
    case runExcept (derefHOAS nam cid index cache) of
      Right trm  -> go trm
      Left  err  -> error $ "BAD: Runtime DerefErr: " ++ show err
  FixH nam bod       -> go (bod trm)
  AppH fun arg       -> case go fun of
    LamH _ _ bod -> go (bod arg)
    x            -> AppH fun arg
  AnnH _ a _   -> go a
  LetH _ _ _ exp bod -> go (bod exp)
  x                  -> x
  where
    go x = whnf x index cache

hash :: HOAS -> Int -> CID
hash term dep = makeCID $ anonymizeHOAS term dep

-- | Normalize a HOAS term
norm :: HOAS -> Index -> Cache -> HOAS
norm term index cache = runST (top $ term)
  where
    top :: HOAS -> ST s HOAS
    top term = do
      seen <- newSTRef (Set.empty)
      go term seen

    go :: HOAS -> (STRef s (Set CID)) -> ST s HOAS
    go term seen = do
      let step = whnf term index cache
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
      FixH n b                 -> go (b step) seen
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
  index <- _packInd . snd <$> pFile "" file
  cache <- readCache
  cid   <- catchDerefErr (indexLookup name index)
  def   <- catchDerefErr (derefHOAS name cid index cache)
  return $ def

normDef :: Name -> FilePath -> IO HOAS
normDef name file = do
  index <- _packInd . snd <$> pFile "" file
  cache <- readCache
  cid   <- catchDerefErr (indexLookup name index)
  def   <- catchDerefErr (derefHOAS name cid index cache)
  return $ norm def index cache

substFreeVar :: HOAS -> Int -> HOAS -> HOAS
substFreeVar a i v = case a of
    VarH n j              -> if i == j then v else a
    LamH n (Just (u,t)) b -> LamH n (Just (u,substFreeVar t i v)) (\x -> substFreeVar (b x) i v)
    LamH n Nothing b      -> LamH n Nothing (\x -> substFreeVar (b x) i v)
    AppH f a              -> AppH (substFreeVar f i v) (substFreeVar a i v)
    LetH n u t x b        -> LetH n u (substFreeVar t i v) (substFreeVar x i v) (\x -> substFreeVar (b x) i v)
    AllH s n u t b        -> AllH s n u (substFreeVar t i v) (\s x -> substFreeVar (b s x) i v)
    FixH n b              -> FixH n (\x -> substFreeVar (b x) i v)
    AnnH j b t            -> AnnH j (substFreeVar b i v) (substFreeVar t i v)
    _                     -> a

---- | Read, eval and print a `HOAS` from a file
----evalPrintFile :: FilePath -> IO ()
----evalPrintFile file = do
----  term <- evalFile file
----  putStrLn $ T.unpack $ printHOAS term

-- * Type-checking

equal :: HOAS -> HOAS -> Index -> Cache -> Int -> Bool
equal a b index cache dep = runST $ top a b dep
  where
    top :: HOAS -> HOAS -> Int -> ST s Bool
    top a b dep = do
      seen <- newSTRef (Set.empty)
      go a b dep seen

    go :: HOAS -> HOAS -> Int -> STRef s (Set (CID,CID)) -> ST s Bool
    go a b dep seen = do
      let a' = whnf a index cache
      let b' = whnf b index cache
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
     _ -> return False

type Ctx = Seq (Uses,Name,HOAS)

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

data CheckErr
  = QuantityMismatch Ctx (Uses,Name,HOAS) (Uses,Name,HOAS)
  | TypeMismatch Ctx     (Uses,Name,HOAS) (Uses,Name,HOAS)
  | EmptyContext
  | DerefError Ctx Name CID DerefErr
  | LambdaNonFunctionType  Ctx HOAS HOAS HOAS
  | NonFunctionApplication Ctx HOAS HOAS HOAS
  | CustomErr Ctx Text

instance Show CheckErr where
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
    DerefError ctx name cid derefErr -> concat
      ["Dereference error: \n"
      , "Name: ", show name, "\n"
      , "CID:  ", show cid, "\n"
      , "Error:", show derefErr
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

check :: Ctx -> Uses -> HOAS -> HOAS -> Index -> Cache -> Except CheckErr Ctx
check pre ρ trm typ index cache = case trm of
  LamH name ann termBody -> case whnf typ index cache of
    AllH s n π bind typeBody -> do
      maybe (pure ()) (\(φ,bind') -> do
        unless (π == φ)
          (throwError (QuantityMismatch pre (φ,name,bind') (π,n,bind)))
        unless (equal bind bind' index cache (length pre))
          (throwError (TypeMismatch pre (φ,name,bind') (π,n,bind)))
        pure ()) ann
      let var      = VarH name (length pre)
      let bodyType = typeBody trm var
      let bodyTerm = termBody var
      bodyCtx <- check (pre |> (None,name,bind)) Once bodyTerm bodyType index cache
      case viewr bodyCtx of
        EmptyR -> throwError $ EmptyContext
        bodyCtx' :> (π',n',b') -> do
          unless (π' ≤# π)
            (throwError (QuantityMismatch bodyCtx (π',n',b') (π,n,bind)))
          return $ multiplyCtx ρ bodyCtx'
    x -> throwError $ LambdaNonFunctionType pre trm typ x
  LetH name π exprType expr body -> do
    exprCtx <- check pre π expr exprType index cache
    let var = VarH name (length pre)
    bodyCtx <- check (pre |> (None,name,exprType)) Once (body var) typ index cache
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
    bodyCtx <- check (pre |> (None,name,typ)) ρ unroll typ index cache
    case viewr bodyCtx of
      EmptyR -> throwError $ EmptyContext
      bodyCtx' :> (π,_,_) -> do
        if π == None
          then return bodyCtx'
          else return $ multiplyCtx Many bodyCtx'
  _ -> do
    (ctx, infr) <- infer pre ρ trm index cache
    if equal typ infr index cache (length pre)
      then return ctx
      else throwError (TypeMismatch ctx (ρ,"",typ) (Many,"",infr))

infer :: Ctx -> Uses -> HOAS -> Index -> Cache -> Except CheckErr (Ctx, HOAS)
infer pre ρ term index cache = case term of
  VarH n idx -> do
    let (_,_,typ) = Seq.index pre idx
    let ctx = Seq.update idx (ρ,n,typ) pre
    return (ctx, typ)
  RefH n c -> do
    let mapE = mapExcept (either (\e -> throwError $ DerefError pre n c e) pure)
    def        <- mapE (deref n c index cache)
    (_,typ)    <- mapE (defToHOAS n def index cache)
    return (pre,typ)
  AppH func argm -> do
    (funcCtx, funcType) <- infer pre ρ func index cache
    case whnf funcType index cache of
      AllH _ _ π bind body -> do
        argmCtx <- check pre (ρ *# π) argm bind index cache
        return (addCtx funcCtx argmCtx, body func argm)
      x -> throwError $ NonFunctionApplication funcCtx func funcType x
  AllH self name π bind body -> do
    check pre  None bind (TypH) index cache
    let self_var = VarH self $ length pre
    let name_var = VarH name $ length pre + 1
    let pre'     = pre |> (None,self,term) |> (None,name,bind)
    check pre' None (body self_var name_var) (TypH) index cache
    return (pre, TypH)
  LamH name (Just(π, bind)) termBody -> do
    let var = VarH name (length pre)
    let pre' = (pre |> (None,name,bind))
    (ctx', typ) <- infer pre' Once (termBody var) index cache
    case viewr ctx' of
      EmptyR -> throwError $ EmptyContext
      ctx :> (π',n',b') -> do
        unless (π' ≤# π) 
          (throwError (QuantityMismatch pre' (π',n',b') (π,name,bind)))
        let typeBody _ x = substFreeVar typ (length pre) x
        return (multiplyCtx ρ ctx, AllH "" name π bind typeBody)
  LetH name π exprType expr body -> do
    exprCtx <- check pre π expr exprType index cache
    let var = VarH name (length pre)
    let pre' = (pre |> (None, name,exprType))
    (bodyCtx, typ) <- infer pre' Once (body var) index cache
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

checkRef :: Name -> CID -> Index -> Cache -> Except CheckErr HOAS
checkRef name cid index cache = do
  let ctx = Seq.empty
  let mapE = mapExcept (either (\e -> throwError $ DerefError ctx name cid e) pure)
  def  <- mapE $ derefMetaDefCID name cid index cache
  (trm,typ) <- mapE $ defToHOAS name def index cache
  check ctx Once trm typ index cache
  return typ

checkFile :: FilePath -> FilePath -> IO ()
checkFile root file = do
  (_,p) <- pFile root file
  let index = _packInd p
  cache <- readCache
  let func :: (Name, CID) -> IO ()
      func (name, cid) = do
        case runExcept $ checkRef name cid index cache of
          Left  e -> putStrLn $ T.unpack $ T.concat 
            ["\ESC[31m\STX✗\ESC[m\STX ", name, "\n", T.pack $ show e]
          Right t -> putStrLn $ T.unpack $ T.concat
            ["\ESC[32m\STX✓\ESC[m\STX ",name, ": ", printHOAS t]
  forM_ (M.toList $ index) func
