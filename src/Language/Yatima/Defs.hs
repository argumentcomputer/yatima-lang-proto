{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.Yatima.Defs
  ( -- | IPFS content-identifiers
    module Language.Yatima.CID
  , Package(..)
  , Anon(..)
  , Meta(..)
  , AnonDef(..)
  , MetaDef(..)
  , DerefErr(..)
  , Index
  , Cache
  , anonymize
  , separateRef
  , separateMeta
  , mergeMeta
  , find
  , deserial
  , deref
  , derefMetaDef
  , derefMetaDefCID
  , indexLookup
  , cacheLookup
  , insertDef
  , insertPackage
  , decodeField
  , decodeCtor
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

-- * Serialisation datatypes

data Package = Package
  { _title   :: Name
  , _descrip :: Text
  , _imports :: Set CID -- links to packages
  , _packInd :: Index -- links to MetaDefs
  } deriving (Show, Eq)

type Index = Map Name CID
type Cache = Map CID BS.ByteString

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
      let file = (".yatima/cache/" ++ (T.unpack $ cidToText c))
      exists <- doesFileExist file
      unless exists (BS.writeFile file bs)

emptyPackage :: Name -> Package
emptyPackage n = Package n "" Set.empty M.empty

-- | An anonymized `Def`
data AnonDef = AnonDef
  { _termAnon :: CID
  , _typeAnon :: CID
  } deriving Show

-- | The metadata from a `Def`
data MetaDef = MetaDef
  { _anonDef   :: CID
  , _document  :: Text
  , _termMeta  :: Meta
  , _typeMeta  :: Meta
  } deriving Show

-- | An anonymized `Term`
data Anon where
  VarA :: Int  -> Anon
  LamA :: Maybe (Uses,Anon) -> Anon -> Anon
  AppA :: Anon -> Anon -> Anon
  RefA :: CID  -> Anon
  LetA :: Uses -> Anon -> Anon -> Anon -> Anon
  AllA :: Uses -> Anon -> Anon -> Anon
  TypA :: Anon

deriving instance Show Anon
deriving instance Eq Anon

-- | A `Term`'s metadata
data Meta = Meta { _entries :: IntMap (Name, Maybe CID) } deriving (Show,Eq)

-- * Serialisation utilities
-- | A helpful utility function like `when`, but with with pretty text errors
failM :: (Monad m, MonadFail m) => Bool -> [Text] -> m ()
failM c msg = if c then fail (T.unpack $ T.concat $ msg) else return ()

-- | A utility for erroring in a Decoder when we attempt to decode a field
decodeField :: Text -> Text -> Decoder s a -> Decoder s a
decodeField err nam decoder = do
  field <- decodeString
  failM (field /= nam)
    ["invalid ",err," field \"",field,"\"","expected \"",nam, "\"" ]
  decoder

-- | A utility for erroring in a Decoder when we attempt to decode a
-- constructor that matches one of a given set of names.
decodeCtor :: Text -> Text -> [Text] -> Decoder s Text
decodeCtor err field ns = do
  value <- decodeField err field decodeString
  failM (not $ value `elem` ns)
    ["invalid constructor tag at \"",field,"\""
    ,"expected one of: ",T.intercalate ", " ns
    ]
  return value


-- * Serialisation instance

-- | This encoding is designed to match the
-- [js-ipld-dag-cbor](https://github.com/ipld/js-ipld-dag-cbor) library's
-- encoding of the isomorphic JSON structure defined by:
--
-- > const Var = (idx)       => ({$0:"Var",$1:idx});
-- > const Ref = (cid)       => ({$0:"Ref",$1:cid});
-- > const Lam = (body)      => ({$0:"Lam",$1:body});
-- > const App = (func,argm) => ({$0:"App",$1:func,$2:argm});
encodeAnon :: Anon -> Encoding
encodeAnon term = case term of
  VarA idx             -> encodeMapLen 2
                          <> (encodeString "0$ctor" <> encodeString "Var")
                          <> (encodeString "1$indx" <> encodeInt     idx)
  RefA cid             -> encodeMapLen 2
                          <> (encodeString "0$ctor" <> encodeString "Ref")
                          <> (encodeString "1$link" <> encodeCID     cid)
  LamA typ bdy         -> encodeMapLen 3
                          <> (encodeString "0$ctor" <> encodeString "Lam")
                          <> (encodeString "1$annt" <> encodeMaybeTyp typ)
                          <> (encodeString "2$body" <> encodeAnon bdy)
  AppA fun arg         -> encodeMapLen 3
                          <> (encodeString "0$ctor" <> encodeString "App")
                          <> (encodeString "1$func" <> encodeAnon    fun)
                          <> (encodeString "2$argm" <> encodeAnon    arg)
  LetA use typ exp bdy -> encodeMapLen 5
                          <> (encodeString "0$ctor" <> encodeString "Let")
                          <> (encodeString "1$uses" <> encodeInt (fromEnum use))
                          <> (encodeString "2$type" <> encodeAnon typ)
                          <> (encodeString "3$expr" <> encodeAnon exp)
                          <> (encodeString "4$body" <> encodeAnon bdy)
  AllA use typ bdy     -> encodeMapLen 4
                          <> (encodeString "0$ctor" <> encodeString "All")
                          <> (encodeString "1$uses" <> encodeInt (fromEnum use))
                          <> (encodeString "2$type" <> encodeAnon typ)
                          <> (encodeString "3$body" <> encodeAnon bdy)
  TypA                  -> encodeMapLen 1
                          <> (encodeString "0$ctor" <> encodeString "Typ")

encodeMaybeTyp :: Maybe (Uses,Anon) -> Encoding
encodeMaybeTyp Nothing      = encodeListLen 0
encodeMaybeTyp (Just (u,t)) = encodeListLen 2
                              <> encodeInt (fromEnum u)
                              <> encodeAnon t

-- | Decode an `Encoding` generated by `encodeAnon`
decodeAnon :: Decoder s Anon
decodeAnon = do
  size <- decodeMapLen
  case size of
    1 -> do
      ctor <- decodeCtor "Anon" "0$ctor" ["Typ"]
      case ctor of
        "Typ" -> return TypA
        _ -> fail $ "invalid ctor: " ++ T.unpack ctor
    2 -> do
      ctor <- decodeCtor "Anon" "0$ctor" ["Var", "Ref"]
      case ctor of
        "Var" -> VarA <$> decodeField "Var" "1$indx" decodeInt
        "Ref" -> RefA <$> decodeField "Ref" "1$link" decodeCID
    3 -> do
      ctor <- decodeCtor "Anon" "0$ctor" ["App", "Lam"]
      case ctor of
        "App" -> AppA <$> decodeField "App" "1$func" decodeAnon
                      <*> decodeField "App" "2$argm" decodeAnon
        "Lam" -> LamA <$> decodeField "Lam" "1$annt" decodeMaybeTyp
                      <*> decodeField "Lam" "2$body" decodeAnon
    4 -> do
      ctor <- decodeCtor "Anon" "0$ctor" ["All"]
      case ctor of
        "All" -> AllA <$> decodeField "All" "1$uses" decodeUses
                      <*> decodeField "All" "2$type" decodeAnon
                      <*> decodeField "All" "3$body" decodeAnon
    5 -> do
      ctor <- decodeCtor "Anon" "0$ctor" ["Let"]
      case ctor of
        "Let" -> LetA <$> decodeField "Let" "1$uses" decodeUses
                      <*> decodeField "Let" "2$type" decodeAnon
                      <*> decodeField "Let" "3$expr" decodeAnon
                      <*> decodeField "Let" "4$body" decodeAnon
    _ -> fail "invalid map size"

-- | `Uses` is an `Enum` instance, but we define a separate decoding function
-- for convenience.
decodeUses :: Decoder s Uses
decodeUses = toEnum <$> decodeInt

decodeMaybeTyp :: Decoder s (Maybe (Uses, Anon))
decodeMaybeTyp = do
  size <- decodeListLen
  case size of
    0 -> return Nothing
    2 -> do
      u <- decodeUses
      t <- decodeAnon
      return (Just (u,t))
    _ -> fail "invalid lambda annotation"

instance Serialise Anon where
  encode = encodeAnon
  decode = decodeAnon

encodeMetaMap :: IntMap (Name,Maybe CID) -> Encoding
encodeMetaMap m = encodeMapLen (fromIntegral (IM.size m))
  <> IM.foldrWithKey go mempty m
  where
    go = (\k v r -> encodeString (T.pack $ show k) <> encodeEntry v <> r)
    encodeEntry (n,Nothing) = encodeListLen 1 <> encodeString n
    encodeEntry (n,Just c)  = encodeListLen 2 <> encodeString n <> encodeCID c

decodeMetaMap :: Decoder s (IntMap (Name,Maybe CID))
decodeMetaMap = do
  size  <- decodeMapLen
  IM.fromList <$> replicateM size decodeEntry
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

-- | This encoding is designed to match the
-- [js-ipld-dag-cbor](https://github.com/ipld/js-ipld-dag-cbor) library's
-- encoding of the isomorphic JSON structure defined by:
--
-- > const Meta = (nams, cids) => ({$0:"Meta", $1:nams, $2:cids})
--
-- where nams and locs are maps of integers to strings and `Loc`, using the
-- standard cborg map encoding.
encodeMeta :: Meta -> Encoding
encodeMeta (Meta ns) = encodeMapLen 2
    <> (encodeString "0$ctor" <> encodeString "Meta")
    <> (encodeString "1$data" <> encodeMetaMap ns)

-- | Decode an `Encoding` generate by `encodeMeta`
decodeMeta :: Decoder s Meta
decodeMeta = do
  size  <- decodeMapLen
  failM (size /= 2) ["invalid map size: ", T.pack $ show size]
  decodeCtor "Meta" "0$ctor" ["Meta"]
  ns <- decodeField "Meta" "1$data" decodeMetaMap
  return $ Meta ns

instance Serialise Meta where
  encode = encodeMeta
  decode = decodeMeta

encodeAnonDef :: AnonDef -> Encoding
encodeAnonDef (AnonDef termAnon typeAnon ) = encodeMapLen 3
  <> (encodeString "0$ctor" <> encodeString "AnonDef")
  <> (encodeString "1$term" <> encodeCID  termAnon)
  <> (encodeString "2$type" <> encodeCID  typeAnon)

decodeAnonDef :: Decoder s AnonDef
decodeAnonDef = do
  size     <- decodeMapLen
  failM (size /= 3) ["invalid map size: ", T.pack $ show size]

  decodeCtor "AnonDef" "0$ctor" ["AnonDef"]

  termAnon <- decodeField "AnonDef" "1$term" decodeCID
  typeAnon <- decodeField "AnonDef" "2$type" decodeCID

  return $ AnonDef termAnon typeAnon

instance Serialise AnonDef where
  encode = encodeAnonDef
  decode = decodeAnonDef

encodeMetaDef :: MetaDef -> Encoding
encodeMetaDef (MetaDef anonDef doc termMeta typeMeta ) = encodeMapLen 5
  <> (encodeString "0$ctor" <> encodeString "MetaDef")
  <> (encodeString "1$anon" <> encodeCID   anonDef)
  <> (encodeString "2$docu" <> encodeString doc)
  <> (encodeString "3$term" <> encodeMeta  termMeta)
  <> (encodeString "4$type" <> encodeMeta  typeMeta)

decodeMetaDef :: Decoder s MetaDef
decodeMetaDef = do
  size     <- decodeMapLen
  failM (size /= 5) ["invalid map size: ", T.pack $ show size]

  decodeCtor "MetaDef" "0$ctor" ["MetaDef"]

  anonDef  <- decodeField "MetaDef" "1$anon" decodeCID
  doc      <- decodeField "MetaDef" "2$docu" decodeString
  termMeta <- decodeField "MetaDef" "3$term" decodeMeta
  typeMeta <- decodeField "MetaDef" "4$type" decodeMeta

  return $ MetaDef anonDef doc termMeta typeMeta

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
encodePackage package = encodeMapLen 5
  <> (encodeString "0$ctor" <> encodeString  ("Pack" :: Text))
  <> (encodeString "1$name" <> encodeString  (_title   package))
  <> (encodeString "2$desc" <> encodeString  (_descrip package))
  <> (encodeString "3$impt" <> encodeImports (_imports package))
  <> (encodeString "4$defs" <> encodeIndex   (_packInd package))

decodePackage :: Decoder s Package
decodePackage = do
  size     <- decodeMapLen
  failM (size /= 5) ["invalid map size: ", T.pack $ show size]

  decodeCtor "Pack" "0$ctor" ["Pack"]

  title    <- decodeField "Pack" "1$name" decodeString
  descrip  <- decodeField "Pack" "2$desc" decodeString
  imports  <- decodeField "Pack" "3$impt" decodeImports
  index    <- decodeField "Pack" "4$defs" decodeIndex

  return $ Package title descrip imports index

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

separateRef :: Name -> Index -> Cache -> Except DerefErr (CID,CID)
separateRef n index cache = do
  cid     <- indexLookup n index
  metaDef <- deserial @MetaDef cid cache
  return (cid, _anonDef metaDef)

-- | Anonymize a term
anonymize :: Name -> Term -> Index -> Cache -> Except DerefErr Anon
anonymize n t index cache = go t [n]
  where
    go :: Term -> [Name] -> Except DerefErr Anon
    go t ctx = case t of
      Var n                 -> case find n ctx of
        Just i -> return $ VarA i
        _      -> throwError $ FreeVariable n ctx
      Ref n                 -> do
        (_,c) <- separateRef n index cache
        return $ RefA c
      Lam n (Just (u,t)) b  -> do
        t' <- go t ctx
        b' <- go b (n:ctx)
        return $ LamA (Just (u, t')) b'
      Lam n _ b             -> LamA Nothing <$> go b (n:ctx)
      App f a               -> AppA <$> go f ctx <*> go a ctx
      Let n u t x b         ->
        LetA u <$> go t ctx <*> go x (n:ctx) <*> go b (n:ctx)
      Typ                   -> return TypA
      All s n u t b         -> AllA u <$> go t ctx <*> go b (n:s:ctx)

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
  anonDef  <- deserial @AnonDef (_anonDef metaDef) cache
  termAnon <- deserial @Anon (_termAnon anonDef) cache
  term     <- mergeMeta termAnon (_termMeta metaDef)
  typeAnon <- deserial @Anon (_typeAnon anonDef) cache
  typ_     <- mergeMeta typeAnon (_typeMeta metaDef)
  return $ Def termName (_document metaDef) term typ_

derefMetaDefCID :: Name -> CID -> Index -> Cache -> Except DerefErr Def
derefMetaDefCID name cid index cache = do
  mdCID   <- indexLookup name index
  metaDef <- deserial @MetaDef mdCID cache
  when (cid /= mdCID) (throwError $ CIDMismatch name cid mdCID)
  def <- derefMetaDef metaDef cache
  when (not $ name == _name def) (throwError $ NameMismatch name (_name def))
  return $ def

separateMeta :: Name -> Term -> Index -> Cache -> Except DerefErr (Anon, Meta)
separateMeta n t index cache = do
  case runState (runExceptT (go t [n])) (Meta (IM.singleton 0 (n,Nothing)), 1) of
    (Left  err,_)   -> throwError err
    (Right a,(m,_)) -> return (a,m)

  where
    entry :: (Name,Maybe CID) -> ExceptT DerefErr (State (Meta,Int)) ()
    entry e = modify (\(Meta es,i) -> (Meta (IM.insert i e es), i))

    bind :: Name -> ExceptT DerefErr (State (Meta,Int)) ()
    bind n = entry (n,Nothing)

    bump :: ExceptT DerefErr (State (Meta,Int)) ()
    bump = modify (\(m,i) -> (m, i+1))

    go :: Term -> [Name] -> ExceptT DerefErr (State (Meta,Int)) Anon
    go t ctx = case t of
      Var n       -> do
        bump
        case find n ctx of
          Just i -> return $ VarA i
          _      -> throwError $ FreeVariable n ctx
      Ref n     -> do
        (metaCID, anonCID) <- liftEither . runExcept $ separateRef n index cache
        entry (n,Just metaCID) >> bump >> return (RefA anonCID)
      Lam n (Just (u,t)) b  -> do
        bind n >> bump
        t' <- go t ctx
        LamA (Just (u, t')) <$> go b (n:ctx)
      Lam n _ b             -> bind n >> bump >> LamA Nothing <$> go b (n:ctx)
      App f a -> bump >> AppA <$> go f ctx <*> go a ctx
      Let n u t x b         -> do
        bind n >> bump
        LetA u <$> go t ctx <*> go x (n:ctx) <*> go b (n:ctx)
      Typ                   -> bump >> return TypA
      All s n u t b         -> do
        bind s >> bump
        bind n >> bump
        AllA u <$> go t ctx <*> go b (n:s:ctx)

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

mergeMeta :: Anon -> Meta -> Except DerefErr Term
mergeMeta anon meta = do
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

    go :: Anon -> [Name] -> ExceptT DerefErr (State Int) Term
    go t ctx = case t of
      VarA idx -> do
        i <- get
        bump
        case lookupNameCtx idx ctx of
          Nothing -> throwError $ MergeFreeVariableAt i ctx idx
          Just n  -> return $ Var n
      RefA cid -> do
        n <- get >>= name
        bump
        return $ Ref n
      LamA (Just (u,t)) b  -> do
        n <- get >>= name
        bump
        t' <- go t ctx
        b' <- go b (n:ctx)
        return $ Lam n (Just (u,t')) b'
      LamA Nothing b  -> do
        n <- get >>= name
        bump
        Lam n Nothing <$> go b (n:ctx)
      AppA f a   -> bump >> App <$> go f ctx <*> go a ctx
      LetA u t x b -> do
        n <- get >>= name
        bump
        Let n u <$> go t ctx <*> go x (n:ctx) <*> go b (n:ctx)
      TypA         -> bump >> return Typ
      AllA u t b   -> do
        s <- get >>= name
        bump
        n <- get >>= name
        bump
        All s n u <$> go t ctx <*> go b (n:s:ctx)

insertDef :: Def -> Index -> Cache -> Except DerefErr (CID,Cache)
insertDef (Def name doc term typ_) index cache = do
  (termAnon, termMeta) <- separateMeta name term index cache
  (typeAnon, typeMeta) <- separateMeta name typ_ index cache
  let termAnonCID = makeCID termAnon :: CID
  let typeAnonCID = makeCID typeAnon :: CID
  let anonDef     = AnonDef termAnonCID typeAnonCID
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

