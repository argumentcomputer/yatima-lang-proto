{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.Yatima.IPLD
  ( -- | IPFS content-identifiers
    module Language.Yatima.CID
  , Package(..)
  , Tree(..)
  , Meta(..)
  , TreeDef(..)
  , MetaDef(..)
  , DerefErr(..)
  , Index
  , Cache
  , makeLink
  , termToTree
  , treeToTerm
  , find
  , deserial
  , deref
  , derefMetaDef
  , derefMetaDefCID
  , indexLookup
  , cacheLookup
  , insertDef
  , insertPackage
  , readCache
  , writeCache
  ) where

import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IM
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.List                  (sortBy)
import           Data.Text                  (Text)
import qualified Data.Text                  as T hiding (find)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString            as BS

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

import           Control.Monad.State.Strict
import           Control.Monad.Except

import           System.Directory

import           Language.Yatima.CID
import           Language.Yatima.Term

-- | An anonymous Abstract Syntax Tree of a λ-like language
data Tree
  = Ctor Name Word [Tree]
  | Bind Tree
  | Vari Int
  | Link CID
  deriving (Eq,Show,Ord)

data Meta = Meta { _entries :: IntMap (Name, Maybe CID) } deriving (Show,Eq)

-- | An anonymized `Def`
data TreeDef = TreeDef
  { _termTree :: CID
  , _typeTree :: CID
  } deriving Show

-- | The metadata from a `Def`
data MetaDef = MetaDef
  { _anonDef   :: CID
  , _document  :: Text
  , _termMeta  :: Meta
  , _typeMeta  :: Meta
  } deriving Show

type Index = Map Name CID
type Cache = Map CID BS.ByteString

data Package = Package
  { _title   :: Name
  , _descrip :: Text
  , _imports :: Set CID -- links to packages
  , _index   :: Index   -- links to MetaDefs
  } deriving (Show, Eq)

encodeTree :: Tree -> Encoding
encodeTree term = case term of
  Ctor n i ts -> encodeListLen 3 <> encodeInt 0 <> encodeString n <>
    encodeListLen i <> foldr (\v r -> encodeTree v <> r) mempty ts
  Bind t    -> encodeListLen 2 <> encodeInt 1 <> encodeTree t
  Vari idx  -> encodeListLen 2 <> encodeInt 2 <> encodeInt idx
  Link cid  -> encodeListLen 2 <> encodeInt 3 <> encodeCID cid

decodeTree :: Decoder s Tree
decodeTree = do
  size <- decodeListLen
  tag  <- decodeInt
  case (size,tag) of
    (3,0) -> do
      ctor  <- decodeString
      arity <- decodeListLen
      args  <- replicateM arity decodeTree
      return $ Ctor ctor (fromIntegral arity) args
    (2,1) -> Bind <$> decodeTree
    (2,2) -> Vari <$> decodeInt
    (2,3) -> Link <$> decodeCID
    _     -> fail $ concat
      ["invalid Tree with size: ", show size, " and tag: ", show tag]

instance Serialise Tree where
  encode = encodeTree
  decode = decodeTree

encodeMeta :: Meta -> Encoding
encodeMeta (Meta m) = encodeMapLen (fromIntegral (IM.size m))
  <> IM.foldrWithKey go mempty m
  where
    go = (\k v r -> encodeString (T.pack $ show k) <> encodeEntry v <> r)
    encodeEntry (n,Nothing) = encodeListLen 1 <> encodeString n
    encodeEntry (n,Just c)  = encodeListLen 2 <> encodeString n <> encodeCID c

decodeMeta :: Decoder s Meta
decodeMeta = do
  size  <- decodeMapLen
  Meta . IM.fromList <$> replicateM size decodeEntry
  where
    decodeEntry = do
      keyString <- decodeString
      let  key = (read (T.unpack keyString) :: Int)
      len <- decodeListLen
      case len of
        1 -> do
          n <- decodeString
          return (key,(n,Nothing))
        2 -> do
          n <- decodeString
          c <- decodeCID
          return (key,(n,Just c))
        _ -> fail "invalid Meta map entry"

instance Serialise Meta where
  encode = encodeMeta
  decode = decodeMeta

encodeTreeDef :: TreeDef -> Encoding
encodeTreeDef (TreeDef term typ_) = encodeListLen 3
  <> (encodeString "TreeDef")
  <> (encodeCID term)
  <> (encodeCID typ_)

decodeTreeDef :: Decoder s TreeDef
decodeTreeDef = do
  size     <- decodeListLen
  when (size /= 3) (fail $ "invalid TreeDef list size: " ++ show size)
  tag <- decodeString
  when (tag /= "TreeDef") (fail $ "invalid TreeDef tag: " ++ show tag)
  TreeDef <$> decodeCID <*> decodeCID

instance Serialise TreeDef where
  encode = encodeTreeDef
  decode = decodeTreeDef

encodeMetaDef :: MetaDef -> Encoding
encodeMetaDef (MetaDef anonDef doc termMeta typeMeta ) = encodeListLen 5
  <> (encodeString "MetaDef")
  <> (encodeCID   anonDef)
  <> (encodeString doc)
  <> (encodeMeta  termMeta)
  <> (encodeMeta  typeMeta)

decodeMetaDef :: Decoder s MetaDef
decodeMetaDef = do
  size     <- decodeListLen
  when (size /= 5) (fail $ "invalid MetaDef list size: " ++ show size)
  tag <- decodeString
  when (tag /= "MetaDef") (fail $ "invalid MetaDef tag: " ++ show tag)
  MetaDef <$> decodeCID <*> decodeString <*> decodeMeta <*> decodeMeta

instance Serialise MetaDef where
  encode = encodeMetaDef
  decode = decodeMetaDef

encodeImports :: Set CID -> Encoding
encodeImports is = encodeListLen (fromIntegral $ Set.size is)
  <> foldr (\v r -> encodeCID v <> r) mempty is

decodeImports :: Decoder s (Set CID)
decodeImports = do
  size <- decodeListLen
  Set.fromList <$> replicateM size decodeCID

encodeIndex :: Index -> Encoding
encodeIndex i = encodeMapLen (fromIntegral $ M.size i) <> foldr go mempty i'
  where
    i' = sortBy cmp (M.toList i)
    go = (\(k,v) r -> encodeString k <> encodeCID v <> r)
    cmp  (k1,_) (k2,_) = cborCanonicalOrder (serialise k1) (serialise k2)

cborCanonicalOrder :: BSL.ByteString -> BSL.ByteString -> Ordering
cborCanonicalOrder x y
  | BSL.length x < BSL.length y = LT
  | BSL.length y > BSL.length x = GT
  | otherwise = compare x y

decodeIndex :: Decoder s Index
decodeIndex = do
  size  <- decodeMapLen
  M.fromList <$> replicateM size ((,) <$> decodeString <*> decodeCID)

encodePackage :: Package -> Encoding
encodePackage package = encodeListLen 5
  <> (encodeString  ("Package" :: Text))
  <> (encodeString  (_title   package))
  <> (encodeString  (_descrip package))
  <> (encodeImports (_imports package))
  <> (encodeIndex   (_index   package))

decodePackage :: Decoder s Package
decodePackage = do
  size     <- decodeListLen
  when (size /= 5) (fail $ "invalid Package list size: " ++ show size)
  tag <- decodeString
  when (tag /= "Package") (fail $ "invalid Package tag: " ++ show tag)
  Package <$> decodeString <*> decodeString <*> decodeImports <*> decodeIndex

instance Serialise Package where
  encode = encodePackage
  decode = decodePackage

-- * Anonymization
data DerefErr
  = NoDeserial DeserialiseFailure
  | NotInIndex Name
  | NotInCache CID
  | CIDMismatch Name CID CID
  | NameMismatch Name Name
  | FreeVariable Name [Name]
  | MergeFreeVariableAt Int [Name] Int
  | MergeMissingNameAt Int
  | UnexpectedCtor Name Word [Tree] [Name] Int
  | UnexpectedBind Tree [Name] Int
  deriving (Eq,Show)

deriving instance Ord DeserialiseFailure
deriving instance Ord DerefErr

-- | Find a name in the binding context and return its index
find :: Name -> [Name] -> Maybe Int
find n cs = go n cs 0
  where
    go n (c:cs) i
      | n == c    = Just i
      | otherwise = go n cs (i+1)
    go _ [] _     = Nothing

indexLookup :: Name -> Index -> Except DerefErr CID
indexLookup n d = maybe (throwError $ NotInIndex n) pure (d M.!? n)

cacheLookup :: CID -> Cache -> Except DerefErr BS.ByteString
cacheLookup c d = maybe (throwError $ NotInCache c) pure (d M.!? c)

deserial :: Serialise a => CID -> Cache -> Except DerefErr a
deserial cid cache = do
  bytes <- cacheLookup cid cache
  either (throwError . NoDeserial) return (deserialiseOrFail $ BSL.fromStrict bytes)

makeLink :: Name -> Index -> Cache -> Except DerefErr (CID,CID)
makeLink n index cache = do
  cid     <- indexLookup n index
  metaDef <- deserial @MetaDef cid cache
  return (cid, _anonDef metaDef)

deref :: Name -> CID -> Index -> Cache -> Except DerefErr Def
deref name cid index cache = do
  mdCID   <- indexLookup name index
  metaDef <- deserial @MetaDef mdCID cache
  let anonCID = _anonDef metaDef
  when (cid /= anonCID) (throwError $ CIDMismatch name cid anonCID)
  def <- derefMetaDef metaDef cache
  when (not $ name == _name def) (throwError $ NameMismatch name (_name def))
  return def

derefMetaDef :: MetaDef -> Cache -> Except DerefErr Def
derefMetaDef metaDef cache = do
  let termNames = fst <$> (_entries . _termMeta $ metaDef)
  termName <- maybe (throwError $ MergeMissingNameAt 0) pure (termNames IM.!? 0)
  let typeNames = fst <$> ( _entries . _typeMeta $ metaDef)
  typeName <- maybe (throwError $ MergeMissingNameAt 0) pure (typeNames IM.!? 0)
  when (not $ termName == typeName) (throwError $ NameMismatch termName typeName)
  anonDef  <- deserial @TreeDef (_anonDef metaDef) cache
  termTree <- deserial @Tree (_termTree anonDef) cache
  term     <- treeToTerm termTree (_termMeta metaDef)
  typeTree <- deserial @Tree (_typeTree anonDef) cache
  typ_     <- treeToTerm typeTree (_typeMeta metaDef)
  return $ Def termName (_document metaDef) term typ_

derefMetaDefCID :: Name -> CID -> Index -> Cache -> Except DerefErr Def
derefMetaDefCID name cid index cache = do
  mdCID   <- indexLookup name index
  metaDef <- deserial @MetaDef mdCID cache
  when (cid /= mdCID) (throwError $ CIDMismatch name cid mdCID)
  def <- derefMetaDef metaDef cache
  when (not $ name == _name def) (throwError $ NameMismatch name (_name def))
  return $ def

usesToTree :: Uses -> Tree
usesToTree None = Ctor "None" 0 []
usesToTree Affi = Ctor "Affi" 0 []
usesToTree Once = Ctor "Once" 0 []
usesToTree Many = Ctor "Many" 0 []

termToTree :: Name -> Term -> Index -> Cache -> Except DerefErr (Tree, Meta)
termToTree n t index cache =
  case runState (runExceptT (go t [n])) meta  of
    (Left  err,_)   -> throwError err
    (Right a,(m,_)) -> return (a,m)
  where
    meta = (Meta (IM.singleton 0 (n,Nothing)), 1)

    entry :: (Name,Maybe CID) -> ExceptT DerefErr (State (Meta,Int)) ()
    entry e = modify (\(Meta es,i) -> (Meta (IM.insert i e es), i))

    bind :: Name -> ExceptT DerefErr (State (Meta,Int)) ()
    bind n = entry (n,Nothing)

    bump :: ExceptT DerefErr (State (Meta,Int)) ()
    bump = modify (\(m,i) -> (m, i+1))

    go :: Term -> [Name] -> ExceptT DerefErr (State (Meta,Int)) Tree
    go t ctx = case t of
      Var n                 -> do
        bump
        case find n ctx of
          Just i -> return $ Vari i
          _      -> throwError $ FreeVariable n ctx
      Ref n                 -> do
        (metaCID, anonCID) <- liftEither . runExcept $ makeLink n index cache
        entry (n,Just metaCID)
        bump
        return (Link anonCID)
      Lam n b               -> do
        bind n
        bump
        b' <- go b (n:ctx)
        return $ Ctor "Lam" 1 [Bind b']
      App f a               -> do
        bump
        f' <- go f ctx 
        a' <- go a ctx
        return $ Ctor "App" 2 [f', a']
      Let n u t x b         -> do
        bind n
        bump
        t' <- go t ctx
        x' <- go x (n:ctx)
        b' <- go b (n:ctx)
        return $ Ctor "Let" 4 [usesToTree u, t', Bind x', Bind b']
      Typ                   -> bump >> return (Ctor "Typ" 0 [])
      All s n u t b         -> do
        bind s >> bump
        bind n >> bump
        t' <- go t ctx
        b' <- go b (n:s:ctx)
        return $ Ctor "All" 3 [usesToTree u, t', Bind (Bind b')]

-- | Find a name in the binding context and return its index
lookupNameCtx :: Int -> [Name] -> Maybe Name
lookupNameCtx i []     = Nothing
lookupNameCtx i (x:xs)
  | i < 0     = Nothing
  | i == 0    = Just x
  | otherwise = lookupNameCtx (i - 1) xs

lookupNameMeta :: Int -> Meta -> Except DerefErr Name
lookupNameMeta i meta = do
 let entry = (_entries meta IM.!? i)
 (n,_) <- maybe (throwError $ MergeMissingNameAt i) pure entry
 return n

treeToTerm :: Tree -> Meta -> Except DerefErr Term
treeToTerm anon meta = do
  defName <- lookupNameMeta 0 meta
  liftEither (evalState (runExceptT (go anon [defName])) 1)

  where
    bump :: ExceptT DerefErr (State Int) ()
    bump = modify (+1)

    name :: Int -> ExceptT DerefErr (State Int) Name
    name i = do
     let entry = (_entries meta IM.!? i)
     (n,_) <- maybe (throwError $ MergeMissingNameAt i) pure entry
     return n

    uses :: Name -> [Name] -> ExceptT DerefErr (State Int) Uses
    uses n ctx = case n of
      "None" -> return None
      "Affi" -> return Affi
      "Once" -> return Once
      "Many" -> return Many
      n      -> get >>= \i -> throwError $ UnexpectedCtor n 0 [] ctx i

    go :: Tree -> [Name] -> ExceptT DerefErr (State Int) Term
    go t ctx = case t of
      Vari idx -> do
        i <- get
        bump
        case lookupNameCtx idx ctx of
          Nothing -> throwError $ MergeFreeVariableAt i ctx idx
          Just n  -> return $ Var n
      Link cid -> do
        n <- get >>= name
        bump
        return $ Ref n
      Bind b    -> get >>= \i -> throwError $ UnexpectedBind b ctx i
      Ctor nam ari args -> case (nam,ari,args) of
        ("Lam",1,[Bind b]) -> do
          n <- get >>= name
          bump
          Lam n <$> go b (n:ctx)
        ("App",2,[f,a]) -> bump >> App <$> go f ctx <*> go a ctx
        ("Let",4,[Ctor u 0 [],t,Bind x, Bind b]) -> do
          n <- get >>= name
          u <- uses u ctx
          bump
          Let n u <$> go t ctx <*> go x (n:ctx) <*> go b (n:ctx)
        ("Typ",0,[]) -> bump >> return Typ
        ("All",3,[Ctor u 0 [], t, Bind (Bind b)]) -> do
          u <- uses u ctx
          s <- get >>= name
          bump
          n <- get >>= name
          bump
          All s n u <$> go t ctx <*> go b (n:s:ctx)
        (c, a, b) -> get >>= \i -> throwError $ UnexpectedCtor c a b ctx i

insertDef :: Def -> Index -> Cache -> Except DerefErr (CID,Cache)
insertDef (Def name doc term typ_) index cache = do
  (termAnon, termMeta) <- termToTree name term index cache
  (typeAnon, typeMeta) <- termToTree name typ_ index cache
  let termAnonCID = makeCID termAnon :: CID
  let typeAnonCID = makeCID typeAnon :: CID
  let anonDef     = TreeDef termAnonCID typeAnonCID
  let anonDefCID  = makeCID anonDef :: CID
  let metaDef     = MetaDef anonDefCID doc termMeta typeMeta
  let metaDefCID  = makeCID metaDef :: CID
  let cache'      = M.insert metaDefCID  (BSL.toStrict $ serialise metaDef)  $
                    M.insert anonDefCID  (BSL.toStrict $ serialise anonDef)  $
                    M.insert typeAnonCID (BSL.toStrict $ serialise typeAnon) $
                    M.insert termAnonCID (BSL.toStrict $ serialise termAnon) $
                    cache
  return $ (metaDefCID,cache')

insertPackage :: Package -> Cache -> (CID,Cache)
insertPackage p cache = let pCID = makeCID p in
  (pCID, M.insert pCID (BSL.toStrict $ serialise p) cache)

readCache :: IO Cache
readCache = do
  createDirectoryIfMissing True ".yatima/cache"
  ns <- listDirectory ".yatima/cache"
  M.fromList <$> traverse go ns
  where
    go :: FilePath -> IO (CID, BS.ByteString)
    go f = do
      bs <- BS.readFile (".yatima/cache/" ++ f)
      case cidFromText (T.pack f) of
        Left e  -> error $ "CORRUPT CACHE ENTRY: " ++ f ++ ", " ++ e
        Right c -> return (c,bs)

writeCache :: Cache -> IO ()
writeCache c = void $ M.traverseWithKey go c
  where
    go :: CID -> BS.ByteString -> IO ()
    go c bs = do
      createDirectoryIfMissing True ".yatima/cache"
      let file = (".yatima/cache/" ++ (T.unpack $ printCIDBase32 c))
      exists <- doesFileExist file
      unless exists (BS.writeFile file bs)

emptyPackage :: Name -> Package
emptyPackage n = Package n "" Set.empty M.empty

