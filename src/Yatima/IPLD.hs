{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module Yatima.IPLD where

--import           Data.Set                   (Set)
--import qualified Data.Set                   as Set

import           Data.Text                  (Text)
import qualified Data.Text                  as T hiding (find)

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString            as BS
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IM

import           Codec.Serialise

import           Control.Monad.Except

import           Control.Monad.State.Strict

import           Path
import           Path.IO

import           Yatima.CID
import           Yatima.Term
import           Yatima.Package
import           Yatima.DagAST

newtype Cache = Cache { _data :: Map CID BS.ByteString }

data IPLDErr
  = NoDeserial DeserialiseFailure
  | NotInIndex Name
  | NotInCache CID
  | CIDMismatch Name CID CID
  | NameMismatch Name Name
  | FreeVariable Name [Name]
  | MergeFreeVariableAt Int [Name] Int
  | MergeMissingNameAt Int
  | MergeMissingCIDAt Int
  | UnexpectedCtor Name [AnonAST] [Name] Int
  | UnexpectedBind AnonAST [Name] Int
  deriving Eq

instance Show IPLDErr where
  show (NoDeserial e)      = "Deserialise Failure: " ++ show e
  show (NotInIndex n)      = "Not In Index: " ++ show n
  show (NotInCache c)      = "Not In Cache: " ++ (T.unpack $ printCIDBase32 c)
  show (CIDMismatch n x y) = concat
    ["CID Mismatch on ",show n,":\n"
    , T.unpack $ printCIDBase32 x, "\n"
    , T.unpack $ printCIDBase32 y
    ]
  show (NameMismatch x y)   = "Name Mismatch: " ++ show x ++ " " ++ show y
  show (FreeVariable n ctx) = concat
    ["Free Variable: " ,show n," with context ", show ctx]
  show (MergeFreeVariableAt i ctx j) = concat
    ["Free Variable while merging Tree and Meta: ", show i
    , " ", show ctx
    , " ", show j
    ]
  show (MergeMissingNameAt i)        = concat
    ["Missing Name entry while merging Tree and Meta: ", show i]
  show (UnexpectedCtor n ts ctx i) = concat
    ["UnexpectedCtor: ", show n, " with args ", show ts
    , "\ncontext: ", show ctx
    , "\nterm index: ", show i
    ]
  show (UnexpectedBind t ctx i)      = concat
    ["UnexpectedBind: ", show t
    , "\ncontext: ", show ctx
    , "\nterm index: ", show i
    ]

deriving instance Ord DeserialiseFailure
deriving instance Ord IPLDErr

-- | Find a name in the binding context and return its index
find :: Name -> [Name] -> Maybe Int
find n cs = go n cs 0
  where
    go n (c:cs) i
      | n == c    = Just i
      | otherwise = go n cs (i+1)
    go _ [] _     = Nothing

indexLookup :: Monad m => Name -> Index -> ExceptT IPLDErr m CID
indexLookup n d = maybe (throwError $ NotInIndex n) pure ((_byName d) M.!? n)

indexLookup' :: Monad m => CID -> Index -> ExceptT IPLDErr m Name
indexLookup' n d =
  maybe (throwError $ NotInIndex (printCIDBase32 n)) pure ((_byCID d) M.!? n)

cacheLookup :: Monad m => CID -> Cache -> ExceptT IPLDErr m BS.ByteString
cacheLookup c (Cache d) = maybe (throwError $ NotInCache c) pure (d M.!? c)

deserial :: (Serialise a, Monad m) => CID -> Cache -> ExceptT IPLDErr m a
deserial cid cache = do
  bs <- cacheLookup cid cache
  either (throwError . NoDeserial) pure (deserialiseOrFail $ BSL.fromStrict bs)

makeLink :: Monad m => Name -> Index -> Cache -> ExceptT IPLDErr m (CID,CID)
makeLink n index cache = do
  cid    <- indexLookup n index
  dagDef <- deserial @DagDef cid cache
  return (cid, _anonDef dagDef)

deref :: Monad m => Name -> Index -> Cache -> ExceptT IPLDErr m Def
deref name index cache = do
  mdCID   <- indexLookup name index
  metaDef <- deserial @DagDef mdCID cache
  def <- derefDagDef name index metaDef cache
  return def

derefDagDef :: Monad m => Name -> Index -> DagDef -> Cache
            -> ExceptT IPLDErr m Def
derefDagDef name index dagDef cache = do
  astDef  <- deserial @AnonDef (_anonDef dagDef) cache
  termAST <- deserial @AnonAST (_anonTerm astDef) cache
  term    <- astToTerm name index termAST (_termMeta dagDef)
  typeAST <- deserial @AnonAST (_anonType astDef) cache
  typ_    <- astToTerm name index typeAST (_typeMeta dagDef)
  return $ Def (_document dagDef) term typ_

derefDagDefCID :: Monad m => Name -> CID -> Index -> Cache 
               -> ExceptT IPLDErr m Def
derefDagDefCID name cid index cache = do
  mdCID   <- indexLookup name index
  dagDef  <- deserial @DagDef mdCID cache
  when (cid /= mdCID) (throwError $ CIDMismatch name cid mdCID)
  def <- derefDagDef name index dagDef cache
  --when (not $ name == _name def) (throwError $ NameMismatch name (_name def))
  return $ def

usesToAST :: Uses -> AnonAST
usesToAST None = Ctor "None" []
usesToAST Affi = Ctor "Affi" []
usesToAST Once = Ctor "Once" []
usesToAST Many = Ctor "Many" []

termToAST :: Monad m => Name -> Term -> Index -> Cache 
          -> ExceptT IPLDErr m (AnonAST, Meta)
termToAST n t index cache =
  case runState (runExceptT (go t [n])) meta  of
    (Left  err,_)   -> throwError err
    (Right a,(m,_)) -> return (a,m)
  where
    meta = (Meta IM.empty, 1)

    entry :: (Either Name CID) -> ExceptT IPLDErr (State (Meta,Int)) ()
    entry e = modify (\(Meta es,i) -> (Meta (IM.insert i e es), i))

    bind :: Name -> ExceptT IPLDErr (State (Meta,Int)) ()
    bind n = entry (Left n)

    bump :: ExceptT IPLDErr (State (Meta,Int)) ()
    bump = modify (\(m,i) -> (m, i+1))

    go :: Term -> [Name] -> ExceptT IPLDErr (State (Meta,Int)) AnonAST
    go t ctx = case t of
      Var n                 -> do
        bump
        case find n ctx of
          Just i -> return $ Vari i
          _      -> throwError $ FreeVariable n ctx
      Ref n                 -> do
        (metaCID, anonCID) <- liftEither . runExcept $ makeLink n index cache
        entry (Right metaCID)
        bump
        return (Link anonCID)
      Lam n b               -> do
        bind n
        bump
        b' <- go b (n:ctx)
        return $ Ctor "Lam" [Bind b']
      App f a               -> do
        bump
        f' <- go f ctx
        a' <- go a ctx
        return $ Ctor "App" [f', a']
      New e                 -> do
        bump
        e' <- go e ctx
        return $ Ctor "New" [e']
      Use e                 -> do
        bump
        e' <- go e ctx
        return $ Ctor "Use" [e']
      Ann v t               -> do
        bump
        v' <- go v ctx
        t' <- go t ctx
        return $ Ctor "Ann" [v', t']
      Let n u t x b         -> do
        bind n
        bump
        t' <- go t ctx
        x' <- go x (n:ctx)
        b' <- go b (n:ctx)
        return $ Ctor "Let" [usesToAST u, t', Bind x', Bind b']
      Typ                   -> bump >> return (Ctor "Typ" [])
      All n u t b         -> do
        bind n
        bump
        t' <- go t ctx
        b' <- go b (n:ctx)
        return $ Ctor "All" [usesToAST u, t', Bind (Bind b')]
      Slf n b               -> do
        bind n
        bump
        b' <- go b (n:ctx)
        return $ Ctor "Slf" [Bind b']
      Lit x                -> do
        bump
        return $ Data (BSL.toStrict $ serialise x)
      Opr x                -> do
        bump
        return $ Data (BSL.toStrict $ serialise x)

-- | Find a name in the binding context and return its index
lookupNameCtx :: Int -> [Name] -> Maybe Name
lookupNameCtx i []     = Nothing
lookupNameCtx i (x:xs)
  | i < 0     = Nothing
  | i == 0    = Just x
  | otherwise = lookupNameCtx (i - 1) xs

astToTerm :: Monad m => Name -> Index -> AnonAST -> Meta -> ExceptT IPLDErr m Term
astToTerm n index anon meta = do
  liftEither (evalState (runExceptT (go anon [n])) 1)
  where
    bump :: ExceptT IPLDErr (State Int) ()
    bump = modify (+1)

    name :: Int -> ExceptT IPLDErr (State Int) Name
    name i = do
     let entry = (_entries meta IM.!? i)
     let err = (throwError $ MergeMissingNameAt i)
     n <- maybe err pure entry
     either return (const err) n

    link :: Int -> ExceptT IPLDErr (State Int) Name
    link i = do
     let entry = (_entries meta IM.!? i)
     let err = (throwError $ MergeMissingCIDAt i)
     n   <- maybe err pure entry
     cid <- either (const err) return n
     indexLookup' cid index

    uses :: Name -> [Name] -> ExceptT IPLDErr (State Int) Uses
    uses n ctx = case n of
      "None" -> return None
      "Affi" -> return Affi
      "Once" -> return Once
      "Many" -> return Many
      n      -> get >>= \i -> throwError $ UnexpectedCtor n [] ctx i

    go :: AnonAST -> [Name] -> ExceptT IPLDErr (State Int) Term
    go t ctx = case t of
      Vari idx -> do
        i <- get
        bump
        case lookupNameCtx idx ctx of
          Nothing -> throwError $ MergeFreeVariableAt i ctx idx
          Just n  -> return $ Var n
      Link cid -> do
        n <- get >>= link
        bump
        return $ Ref n
      Bind b  -> get >>= \i -> throwError $ UnexpectedBind b ctx i
      Data bs -> do
        bump
        let bs' = BSL.fromStrict bs
        case (deserialiseOrFail bs') of
          Right x -> return $ Lit x
          Left e  -> case (deserialiseOrFail bs') of
            Right x -> return $ Opr x
            Left e  -> throwError $ NoDeserial e
      Ctor nam args -> case (nam,args) of
        ("Lam",[Bind b]) -> do
          n <- get >>= name
          bump
          Lam n <$> go b (n:ctx)
        ("App",[f,a]) -> bump >> App <$> go f ctx <*> go a ctx
        ("New",[e])   -> bump >> New <$> go e ctx
        ("Use",[e])   -> bump >> Use <$> go e ctx
        ("Ann",[v,t]) -> bump >> Ann <$> go v ctx <*> go t ctx
        ("Let",[Ctor u [],t,Bind x, Bind b]) -> do
          n <- get >>= name
          u <- uses u ctx
          bump
          Let n u <$> go t ctx <*> go x (n:ctx) <*> go b (n:ctx)
        ("Typ",[]) -> bump >> return Typ
        ("All",[Ctor u [], t, Bind (Bind b)]) -> do
          u <- uses u ctx
          n <- get >>= name
          bump
          All n u <$> go t ctx <*> go b (n:ctx)
        ("Slf",[Bind b]) -> do
          n <- get >>= name
          bump
          Slf n <$> go b (n:ctx)
        (c, b) -> get >>= \i -> throwError $ UnexpectedCtor c b ctx i

insertDef :: Monad m => Name -> Def -> Index -> Cache
          -> ExceptT IPLDErr m (CID,Cache)
insertDef name (Def doc term typ_) index c@(Cache cache) = do
  (termAnon, termMeta) <- termToAST name term index c
  (typeAnon, typeMeta) <- termToAST name typ_ index c
  let termAnonCID = makeCID termAnon :: CID
  let typeAnonCID = makeCID typeAnon :: CID
  let anonDef     = AnonDef termAnonCID typeAnonCID
  let anonDefCID  = makeCID anonDef :: CID
  let dagAST      = DagDef anonDefCID doc termMeta typeMeta
  let dagASTCID   = makeCID dagAST :: CID
  let cache'      = M.insert dagASTCID  (BSL.toStrict $ serialise dagAST)  $
                    M.insert anonDefCID  (BSL.toStrict $ serialise anonDef)  $
                    M.insert typeAnonCID (BSL.toStrict $ serialise typeAnon) $
                    M.insert termAnonCID (BSL.toStrict $ serialise termAnon) $
                    cache
  return $ (dagASTCID,Cache cache')

insertPackage :: Package -> Cache -> (CID,Cache)
insertPackage p (Cache c) = let pCID = makeCID p in
  (pCID, Cache $ M.insert pCID (BSL.toStrict $ serialise p) c)

insertSource :: Source -> Cache -> (CID,Cache)
insertSource p (Cache c) = let pCID = makeCID p in
  (pCID, Cache $ M.insert pCID (BSL.toStrict $ serialise p) c)

insertDefs :: Monad m => [(Name,Def)] -> Index -> Cache
           -> ExceptT IPLDErr m (Index,Cache)
insertDefs [] index cache = return (index,cache)
insertDefs ((n,d):ds) index cache = do
  (cid,cache') <- insertDef n d index cache
  let ns = M.insert n cid (_byName index)
  let cs = M.insert cid n (_byCID index)
  insertDefs ds (Index ns cs) cache'

-- | Checks that all Refs in a term correspond to valid DagDef cache entries
-- and that the term has no free variables
validateTerm :: Monad m => Term -> [Name] -> Index -> Cache
             -> ExceptT IPLDErr m Term
validateTerm trm ctx index cache = case trm of
  Var nam                 -> case find nam ctx of
    Just idx -> return $ Var nam
    _        -> throwError $ FreeVariable nam ctx
  Ref nam                 -> makeLink nam index cache >> return (Ref nam)
  Lam nam bod             -> Lam nam <$> bind nam bod
  App fun arg             -> App <$> go fun <*> go arg
  New exp                 -> New <$> go exp
  Use exp                 -> Use <$> go exp
  Let nam use typ exp bod ->
    Let nam use <$> go typ <*> bind nam exp <*> bind nam bod
  Typ                     -> return Typ
  All nam use typ bod     -> All nam use <$> go typ <*> bind nam bod
  Slf nam bod             -> Slf nam <$> bind nam bod
  Ann trm typ             -> Ann <$> go trm <*> go typ
  where
    go t        = validateTerm t ctx index cache
    bind    n t = validateTerm t (n:ctx) index cache

indexToDefs :: Monad m => Index -> Cache -> ExceptT IPLDErr m (Map Name Def)
indexToDefs index cache = M.traverseWithKey go (_byName index)
  where
    go n _ = do
     (Def doc term typ_) <- deref n index cache
     term' <- validateTerm term [n] index cache
     typ_' <- validateTerm typ_ [n] index cache
     return $ Def doc term' typ_'

readCache :: Path Abs Dir -> IO Cache
readCache root = do
  createDirIfMissing True dir
  (_,fs) <- listDir dir
  Cache . M.fromList <$> traverse go fs
  where
    dir = cacheDir root
    go :: Path Abs File -> IO (CID, BS.ByteString)
    go file = do
      bs <- BS.readFile (toFilePath file)
      let name = toFilePath $ filename file
      case cidFromText (T.pack $ name) of
        Left e  -> error $ "CORRUPT CACHE ENTRY: " ++ name ++ ", " ++ e
        Right c -> return (c,bs)

writeCache :: Path Abs Dir -> Cache -> IO ()
writeCache root (Cache c) = void $ M.traverseWithKey go c
  where
    dir = cacheDir root
    go :: CID -> BS.ByteString -> IO ()
    go c bs = do
      createDirIfMissing True dir
      name <- parseRelFile (T.unpack $ printCIDBase32 c)
      let file = (dir </> name)
      exists <- doesFileExist file
      unless exists (BS.writeFile (toFilePath file) bs)

cacheDir :: Path Abs Dir -> Path Abs Dir
cacheDir root = root </> [reldir|.yatima/cache|]

