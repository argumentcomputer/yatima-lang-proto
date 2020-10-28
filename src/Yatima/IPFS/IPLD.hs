{-
Module      : Yatima.IPFS.IPLD
Description : This module implements the IPLD embedding for Yatima terms and
definitions, as well as related utilities.
Copyright   : 2020 Yatima Inc.
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module Yatima.IPFS.IPLD where

--import           Data.Set                   (Set)
--import qualified Data.Set                   as Set
import           Control.Monad.Identity

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

import           Data.IPLD.CID
import           Data.IPLD.DagAST
import           Yatima.Term
import           Yatima.Package

newtype Cache = Cache { _data :: Map CID BS.ByteString }

data IPLDErr
  = NoDeserial [DeserialiseFailure]
  | NotInIndex Name
  | NotInCache CID
  | CIDMismatch Name CID CID
  | NameMismatch Name Name
  | MergeFreeVariableAt Int [Name] Int
  | MergeMissingNameAt Int
  | MergeMissingCIDAt Int
  | UnexpectedCtor Name [DagAST] [Name] Int
  | UnexpectedCIDEntry CID Int
  | UnexpectedBind DagAST [Name] Int
  deriving Eq

instance Show IPLDErr where
  show (NoDeserial e)      = "Deserialise Failure: " ++ show e
  show (NotInIndex n)      = "Not In Index: " ++ show n
  show (NotInCache c)      = "Not In Cache: " ++ (T.unpack $ cidToText c)
  show (CIDMismatch n x y) = concat
    ["CID Mismatch on ",show n,":\n"
    , T.unpack $ cidToText x, "\n"
    , T.unpack $ cidToText y
    ]
  show (NameMismatch x y)   = "Name Mismatch: " ++ show x ++ " " ++ show y
  show (MergeFreeVariableAt i ctx j) = concat
    ["Free Variable: ", show i, " during term reconstruction"
    , " ", show ctx
    , " ", show j
    ]
  show (MergeMissingNameAt i)        = concat
    ["Missing Name entry while merging Tree and Meta: ", show i]
  show (UnexpectedCtor n ts ctx i) = concat
    ["UnexpectedCtor: ", show n, " with args ", show ts, " during term reconstruction"
    , "\ncontext: ", show ctx
    , "\nterm index: ", show i
    ]
  show (UnexpectedCIDEntry cid i) = concat
    ["Unexpected Cid: " , show cid, " during term reconstruction"
    , "\nterm index: ", show i
    ]
  show (UnexpectedBind t ctx i)      = concat
    ["UnexpectedBind: ", show t, " during term reconstruction"
    , "\ncontext: ", show ctx
    , "\nterm index: ", show i
    ]

deriving instance Ord DeserialiseFailure
deriving instance Ord IPLDErr

indexLookup :: Monad m => Name -> Index -> ExceptT IPLDErr m (CID,CID)
indexLookup n i@(Index ns) = maybe (throwError $ NotInIndex n) pure (ns M.!? n)

cacheLookup :: Monad m => CID -> Cache -> ExceptT IPLDErr m BS.ByteString
cacheLookup c (Cache d) = maybe (throwError $ NotInCache c) pure (d M.!? c)

deserial :: (Serialise a, Monad m) => CID -> Cache -> ExceptT IPLDErr m a
deserial cid cache = do
  bs <- cacheLookup cid cache
  either (throwError . NoDeserial . pure) pure
    (deserialiseOrFail $ BSL.fromStrict bs)

deref :: Monad m => Name -> Index -> Cache -> ExceptT IPLDErr m Def
deref name index cache = do
  (defCid,trmCid)  <- indexLookup name index
  metaDef          <- deserial @DagDef defCid cache
  def              <- derefDagDef name index metaDef cache
  return def

derefDagDef :: Monad m => Name -> Index -> DagDef -> Cache
            -> ExceptT IPLDErr m Def
derefDagDef name index dagDef cache = do
  termAST <- deserial @DagAST (_anonTerm dagDef) cache
  typeAST <- deserial @DagAST (_anonType dagDef) cache
  term    <- astToTerm name index termAST (_termMeta dagDef)
  typ_    <- astToTerm name index typeAST (_typeMeta dagDef)
  return $ Def (_document dagDef) term typ_

derefDagDefCID :: Monad m => Name -> CID -> Index -> Cache 
               -> ExceptT IPLDErr m Def
derefDagDefCID name cid index cache = do
  (defCid,trmCid)   <- indexLookup name index
  dagDef            <- deserial @DagDef defCid cache
  def               <- derefDagDef name index dagDef cache
  return $ def

usesToAST :: Uses -> DagAST
usesToAST None = Ctor "None" []
usesToAST Affi = Ctor "Affi" []
usesToAST Once = Ctor "Once" []
usesToAST Many = Ctor "Many" []

boolToAST :: Bool -> DagAST
boolToAST True  = Ctor "True" []
boolToAST False = Ctor "False" []

termToAST :: Term -> DagAST
termToAST t = go t
  where
    rec r = boolToAST r
    use u = usesToAST u

    bind :: Term -> DagAST
    bind t = Bind (go t)

    go :: Term -> DagAST
    go t = case t of
      Var n i         -> (Vari i)
      Ref n _ cid     -> (Link cid)
      Lit x           -> (Data (BSL.toStrict $ serialise x))
      LTy x           -> (Data (BSL.toStrict $ serialise x))
      Opr x           -> (Data (BSL.toStrict $ serialise x))
      Lam n b         -> Ctor "Lam" [bind b]
      App f a         -> Ctor "App" [go f, go a]
      New e           -> Ctor "New" [go e]
      Use e           -> Ctor "Use" [go e]
      Ann v t         -> Ctor "Ann" [go v, go t]
      Let r n u t x b -> Ctor "Let" [rec r, use u, go t, bind x, bind b]
      Typ             -> Ctor "Typ" []
      All n u t b     -> Ctor "All" [use u, go t, bind b]
      Slf n b         -> Ctor "Slf" [bind b]

--test_defs :: (Index,Cache)
--test_defs =
--  let trm = Lam "A" (Lam "x" (Var "x" 0))
--      typ = All "A" Many Typ (All "x" Many (Var "A" 1) (Var "A" 2))
--      def = Def "" trm typ
--      (defCid, trmCid, cache) = insertDef "id" def emptyIndex (Cache M.empty)
--   in (Index (M.singleton "id" (defCid,trmCid)), cache)
--
--test_index = fst test_defs
--
--test_term = 
--  let Right d = cidFromText "bafy2bzaceb7tzcelrtfuo4zl375mtm7dqwmvv7a4amlpziwbm7k3hr4bp3lfc"
--      Right t = cidFromText "bafy2bzaceagf5dbfewoq632a5x5mjhhv3ojftx2sdh3lc73cneocoe7chzsks"
--   in Ref "id" d t


termToMeta :: Term -> Meta
termToMeta t = fst $ execState (go t) (Meta IM.empty, 1)
  where
    entry :: (Name, Maybe CID) -> State (Meta,Int) ()
    entry e = modify (\(Meta es,i) -> (Meta (IM.insert i e es), i))

    bind n   = entry (n,Nothing)
    link n c = entry (n,Just c)

    bump     = modify (\(m,i) -> (m, i+1))

    go :: Term -> State (Meta,Int) ()
    go t = case t of
      Ref n cid _     -> link n cid >> bump
      Lam n b         -> bind n >> bump >> go b
      App f a         -> bump >> go f >> go a
      New e           -> bump >> go e
      Use e           -> bump >> go e
      Ann v t         -> bump >> go v >> go t
      Let r n u t x b -> bind n >> bump >> go t >> go x >> go b
      All n u t b     -> bind n >> bump >> go t >> go b
      Slf n b         -> bind n >> bump >> go b
      _               -> bump

defToDag :: Def -> DagDef
defToDag (Def doc term typ_) = runIdentity $ do
  let anonTerm    = termToAST term
  let termMeta    = termToMeta term
  let anonType    = termToAST typ_
  let typeMeta    = termToMeta typ_
  let anonTermCid = makeCid anonTerm :: CID
  let anonTypeCid = makeCid anonType :: CID
  return $ DagDef anonTermCid anonTypeCid doc termMeta typeMeta

insertDef :: Name -> Def -> Index -> Cache -> (CID,CID,Cache)
insertDef name def@(Def doc term typ_) index c@(Cache cache) = runIdentity $ do
  let anonTerm = termToAST term
  let anonType = termToAST typ_
  let dagDef@(DagDef anonTermCid anonTypeCid _ _ _) = defToDag def
  let dagDefCid   = makeCid dagDef :: CID
  let cache'      = M.insert dagDefCid (BSL.toStrict $ serialise dagDef)  $
                    M.insert anonTypeCid (BSL.toStrict $ serialise anonTerm) $
                    M.insert anonTermCid (BSL.toStrict $ serialise anonType) $
                    cache
  return $ (dagDefCid, anonTermCid, Cache cache')

deserialiseData :: (Monad m) => BS.ByteString -> ExceptT IPLDErr m Term
deserialiseData bs = do
  let bs' = BSL.fromStrict bs
  case (deserialiseOrFail bs') of
    Right x -> return $ Lit x
    Left  e1 -> case (deserialiseOrFail bs') of
      Right x -> return $ LTy x
      Left  e2 -> case (deserialiseOrFail bs') of
        Right x -> return $ Opr x
        Left e3  -> throwError $ NoDeserial [e1,e2,e3]

astToTerm :: Monad m => Name -> Index -> DagAST -> Meta -> ExceptT IPLDErr m Term
astToTerm n index anon meta = do
  liftEither (evalState (runExceptT (go anon [n])) 1)
  where
    bump :: ExceptT IPLDErr (State Int) ()
    bump = modify (+1)

    name :: Int -> ExceptT IPLDErr (State Int) Name
    name i = do
     let entry = (_entries meta IM.!? i)
     let err = (throwError $ MergeMissingNameAt i)
     (nam, c) <- maybe err pure entry
     let err = (\c -> throwError $ UnexpectedCIDEntry c i)
     maybe (return ()) err c
     return nam


    link :: Int -> ExceptT IPLDErr (State Int) (Name,CID)
    link i = do
     let entry = (_entries meta IM.!? i)
     let err = (throwError $ MergeMissingNameAt i)
     (nam,c) <- maybe err pure entry
     let err = (throwError $ MergeMissingCIDAt i)
     cid <- maybe err pure c
     return (nam,cid)

    uses :: Name -> [Name] -> ExceptT IPLDErr (State Int) Uses
    uses n ctx = case n of
      "None" -> return None
      "Affi" -> return Affi
      "Once" -> return Once
      "Many" -> return Many
      n      -> get >>= \i -> throwError $ UnexpectedCtor n [] ctx i

    bool :: Name -> [Name] -> ExceptT IPLDErr (State Int) Bool
    bool n ctx = case n of
      "True"  -> return True
      "False" -> return False
      n      -> get >>= \i -> throwError $ UnexpectedCtor n [] ctx i

    go :: DagAST -> [Name] -> ExceptT IPLDErr (State Int) Term
    go t ctx = case t of
      Vari idx -> do
        i <- get
        bump
        case findByInt idx ctx of
          Nothing -> throwError $ MergeFreeVariableAt i ctx idx
          Just n  -> return $ Var n idx
      Link trmCid -> do
        (nam,defCid) <- get >>= link
        bump
        return $ Ref nam defCid trmCid
      Bind b  -> get >>= \i -> throwError $ UnexpectedBind b ctx i
      Data bs -> bump >> deserialiseData bs
      Ctor nam args -> case (nam,args) of
        ("Lam",[Bind b]) -> do
          n <- get >>= name
          bump
          Lam n <$> go b (n:ctx)
        ("App",[f,a]) -> bump >> App <$> go f ctx <*> go a ctx
        ("New",[e])   -> bump >> New <$> go e ctx
        ("Use",[e])   -> bump >> Use <$> go e ctx
        ("Ann",[v,t]) -> bump >> Ann <$> go v ctx <*> go t ctx
        ("Let",[Ctor r [], Ctor u [],t,Bind x, Bind b]) -> do
          n <- get >>= name
          r <- bool r ctx
          u <- uses u ctx
          bump
          Let r n u <$> go t ctx <*> go x (n:ctx) <*> go b (n:ctx)
        ("Typ",[]) -> bump >> return Typ
        ("All",[Ctor u [], t, (Bind b)]) -> do
          u <- uses u ctx
          n <- get >>= name
          bump
          All n u <$> go t ctx <*> go b (n:ctx)
        ("Slf",[Bind b]) -> do
          n <- get >>= name
          bump
          Slf n <$> go b (n:ctx)
        (c, b) -> get >>= \i -> throwError $ UnexpectedCtor c b ctx i

insertPackage :: Package -> Cache -> (CID,Cache)
insertPackage p (Cache c) = let pCID = makeCid p in
  (pCID, Cache $ M.insert pCID (BSL.toStrict $ serialise p) c)

insertSource :: Source -> Cache -> (CID,Cache)
insertSource p (Cache c) = let pCID = makeCid p in
  (pCID, Cache $ M.insert pCID (BSL.toStrict $ serialise p) c)

insertDefs :: [(Name,Def)] -> Index -> Cache -> (Index,Cache)
insertDefs [] index@(Index ns) cache         = (index,cache)
insertDefs ((n,d):ds) index@(Index ns) cache = runIdentity $ do
  let (defCid,trmCid,cache') = insertDef n d index cache
  return $ insertDefs ds (Index $ M.insert n (defCid,trmCid) ns) cache'

indexToDefs :: Monad m => Index -> Cache -> ExceptT IPLDErr m (Map Name Def)
indexToDefs i@(Index ns) c = M.traverseWithKey (\n _ -> deref n i c) ns

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
      name <- parseRelFile (T.unpack $ cidToText c)
      let file = (dir </> name)
      exists <- doesFileExist file
      unless exists (BS.writeFile (toFilePath file) bs)

cacheDir :: Path Abs Dir -> Path Abs Dir
cacheDir root = root </> [reldir|.yatima/cache|]

