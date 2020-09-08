module Language.Yatima.Defs
  ( -- | IPFS content-identifiers
    module Language.Yatima.CID
  , Defs(..)
  , emptyDefs
  , Anon(..)
  , Meta(..)
  , AnonDef(..)
  , MetaDef(..)
  , DerefErr(..)
  , anonymize
  , separateMeta
  , mergeMeta
  , find
  , anonymizeRef
  , deref
  , derefDef
  , indexLookup
  , cacheLookup
  ) where

import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IM
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T hiding (find)
import qualified Data.ByteString.Lazy       as BSL

import           Codec.Serialise
import           Codec.Serialise.Decoding
import           Codec.Serialise.Encoding

import           Control.Monad.State.Strict
import           Control.Monad.Except

import           Language.Yatima.CID
import           Language.Yatima.Term

-- * Serialisation datatypes

-- | A content-addressed environment with an index of local names
data Defs = Defs { _index :: Map Name CID
                 , _cache :: Map CID BSL.ByteString
                 } deriving (Show, Eq)

emptyDefs :: Defs
emptyDefs = Defs M.empty M.empty

-- | An anonymized `Def`
data AnonDef = AnonDef
  { _termAnon :: CID
  , _typeAnon :: CID
  } deriving Show

-- | The metadata from a `Def`
data MetaDef = MetaDef
  { _anonDef   :: CID
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

-- | A `Term`'s metadata
data Meta = Meta
  { _nams :: IntMap Name
  , _cids :: IntMap CID
  } deriving (Show,Eq)

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
                          <> (encodeString "$0" <> encodeString "Var")
                          <> (encodeString "$1" <> encodeInt      idx)
  RefA cid             -> encodeMapLen 2
                          <> (encodeString "$0" <> encodeString "Ref")
                          <> (encodeString "$1" <> encodeCID     cid)
  LamA typ bdy         -> encodeMapLen 3
                          <> (encodeString "$0" <> encodeString "Lam")
                          <> (encodeString "$1" <> encodeMaybeTyp typ)
                          <> (encodeString "$2" <> encodeAnon bdy)
  AppA fun arg         -> encodeMapLen 3
                          <> (encodeString "$0" <> encodeString "App")
                          <> (encodeString "$1" <> encodeAnon    fun)
                          <> (encodeString "$2" <> encodeAnon    arg)
  LetA use typ exp bdy -> encodeMapLen 5
                          <> (encodeString "$0" <> encodeString "Let")
                          <> (encodeString "$1" <> encodeInt (fromEnum use))
                          <> (encodeString "$2" <> encodeAnon typ)
                          <> (encodeString "$3" <> encodeAnon exp)
                          <> (encodeString "$4" <> encodeAnon bdy)
  AllA use typ bdy     -> encodeMapLen 4
                          <> (encodeString "$0" <> encodeString "All")
                          <> (encodeString "$1" <> encodeInt (fromEnum use))
                          <> (encodeString "$2" <> encodeAnon typ)
                          <> (encodeString "$3" <> encodeAnon bdy)
  TypA                  -> encodeMapLen 1
                          <> (encodeString "$0" <> encodeString "Typ")

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
      ctor <- decodeCtor "Anon" "$0" ["Typ"]
      case ctor of
        "Typ" -> return TypA
        _ -> fail $ "invalid ctor: " ++ T.unpack ctor
    2 -> do
      ctor <- decodeCtor "Anon" "$0" ["Var", "Ref"]
      case ctor of
        "Var" -> VarA <$> decodeField "Var" "$1" decodeInt
        "Ref" -> RefA <$> decodeField "Ref" "$1" decodeCID
    3 -> do
      ctor <- decodeCtor "Anon" "$0" ["App", "Lam"]
      case ctor of
        "App" -> AppA <$> decodeField "App" "$1" decodeAnon
                      <*> decodeField "App" "$2" decodeAnon
        "Lam" -> LamA <$> decodeField "Lam" "$1" decodeMaybeTyp
                      <*> decodeField "Lam" "$2" decodeAnon
    4 -> do
      ctor <- decodeCtor "Anon" "$0" ["All", "Op2", "Ite"]
      case ctor of
        "All" -> AllA <$> decodeField "All" "$1" decodeUses
                      <*> decodeField "All" "$2" decodeAnon
                      <*> decodeField "All" "$3" decodeAnon
    5 -> do
      ctor <- decodeCtor "Anon" "$0" ["Let"]
      case ctor of
        "Let" -> LetA <$> decodeField "Let" "$1" decodeUses
                      <*> decodeField "Let" "$2" decodeAnon
                      <*> decodeField "Let" "$3" decodeAnon
                      <*> decodeField "Let" "$4" decodeAnon
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
    _ -> fail "invalid list size"

instance Serialise Anon where
  encode = encodeAnon
  decode = decodeAnon

-- | This encoding is designed to match the
-- [js-ipld-dag-cbor](https://github.com/ipld/js-ipld-dag-cbor) library's
-- encoding of the isomorphic JSON structure defined by:
--
-- > const Meta = (nams, cids) => ({$0:"Meta", $1:nams, $2:cids})
--
-- where nams and locs are maps of integers to strings and `Loc`, using the
-- standard cborg map encoding.
encodeMeta :: Meta -> Encoding
encodeMeta (Meta ns ls) = encodeMapLen 3
    <> (encodeString "$0" <> encodeString "Meta")
    <> (encodeString "$1" <> encode ns)
    <> (encodeString "$2" <> encode ls)

-- | Decode an `Encoding` generate by `encodeMeta`
decodeMeta :: Decoder s Meta
decodeMeta = do
  size  <- decodeMapLen
  failM (size /= 3) ["invalid map size: ", T.pack $ show size]
  decodeCtor "Meta" "$0" ["Meta"]
  ns <- decodeField "Meta" "$1" decode
  ls <- decodeField "Meta" "$2" decode
  return $ Meta ns ls

instance Serialise Meta where
  encode = encodeMeta
  decode = decodeMeta

encodeAnonDef :: AnonDef -> Encoding
encodeAnonDef (AnonDef termAnon typeAnon ) = encodeMapLen 3
  <> (encodeString "$0" <> encodeString "AnonDef")
  <> (encodeString "$1" <> encodeCID  termAnon)
  <> (encodeString "$2" <> encodeCID  typeAnon)

decodeAnonDef :: Decoder s AnonDef
decodeAnonDef = do
  size     <- decodeMapLen
  failM (size /= 3) ["invalid map size: ", T.pack $ show size]

  decodeCtor "AnonDef" "$0" ["AnonDef"]

  termAnon <- decodeField "AnonDef" "$1" decodeCID
  typeAnon <- decodeField "AnonDef" "$2" decodeCID

  return $ AnonDef termAnon typeAnon

instance Serialise AnonDef where
  encode = encodeAnonDef
  decode = decodeAnonDef

encodeMetaDef :: MetaDef -> Encoding
encodeMetaDef (MetaDef anonDef termMeta typeMeta ) = encodeMapLen 4
  <> (encodeString "$0" <> encodeString "MetaDef")
  <> (encodeString "$1" <> encodeCID   anonDef)
  <> (encodeString "$2" <> encodeMeta  termMeta)
  <> (encodeString "$3" <> encodeMeta  typeMeta)

decodeMetaDef :: Decoder s MetaDef
decodeMetaDef = do
  size     <- decodeMapLen
  failM (size /= 4) ["invalid map size: ", T.pack $ show size]

  decodeCtor "MetaDef" "$0" ["MetaDef"]

  anonDef  <- decodeField "MetaDef" "$1" decodeCID
  termMeta <- decodeField "MetaDef" "$2" decodeMeta
  typeMeta <- decodeField "MetaDef" "$3" decodeMeta

  return $ MetaDef anonDef termMeta typeMeta

instance Serialise MetaDef where
  encode = encodeMetaDef
  decode = decodeMetaDef

-- * Anonymization

data DerefErr
  = NoDeserial DeserialiseFailure
  | NotInIndex Name
  | NotInCache CID
  | CIDMismatch Name CID CID
  | NameMismatch Name Name Name
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

-- | Anonymize a term
anonymize :: Name -> Term -> Defs -> Except DerefErr Anon
anonymize n t ds = go t [n]
  where
    go :: Term -> [Name] -> Except DerefErr Anon
    go t ctx = case t of
      Var n                 -> case find n ctx of
        Just i -> return $ VarA i
        _      -> throwError $ FreeVariable n ctx
      Ref n                 -> RefA <$> anonymizeRef n ds
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

anonymizeRef :: Name -> Defs -> Except DerefErr CID
anonymizeRef n ds = do
  mdCID   <- indexLookup n ds
  mdBytes <- cacheLookup mdCID ds
  metaDef <- either (throwError . NoDeserial) return (deserialiseOrFail mdBytes)
  return (_anonDef metaDef)

indexLookup :: Name -> Defs -> Except DerefErr CID
indexLookup n d = maybe (throwError $ NotInIndex n) pure ((_index d) M.!? n)

cacheLookup :: CID -> Defs -> Except DerefErr BSL.ByteString
cacheLookup c d = maybe (throwError $ NotInCache c) pure ((_cache d) M.!? c)

derefDef :: Name -> CID -> Defs -> Except DerefErr Def
derefDef n cid ds = do
  mdCID   <- indexLookup n ds
  when (cid /= mdCID) (throwError $ CIDMismatch n cid mdCID)
  bytes    <- cacheLookup mdCID ds
  metaDef  <- either (throwError . NoDeserial) return (deserialiseOrFail bytes)
  let termNames = _nams . _termMeta $ metaDef
  termName <- maybe (throwError $ MergeMissingNameAt 0) pure (termNames IM.!? 0)
  let typeNames = _nams . _typeMeta $ metaDef
  typeName <- maybe (throwError $ MergeMissingNameAt 0) pure (termNames IM.!? 0)
  when (not $ n == termName && termName == typeName)
          (throwError $ NameMismatch n termName typeName)
  bytes    <- cacheLookup (_anonDef metaDef) ds
  anonDef  <- either (throwError . NoDeserial) return (deserialiseOrFail bytes)
  bytes    <- cacheLookup (_termAnon anonDef) ds
  termAnon <- either (throwError . NoDeserial) return (deserialiseOrFail bytes)
  term     <- mergeMeta termAnon (_termMeta metaDef)
  bytes    <- cacheLookup (_typeAnon anonDef) ds
  typeAnon <- either (throwError . NoDeserial) return (deserialiseOrFail bytes)
  typ_     <- mergeMeta typeAnon (_typeMeta metaDef)
  return $ Def n term typ_

deref :: Name -> CID -> Defs -> Except DerefErr Term
deref n cid ds = do
  mdCID   <- indexLookup n ds
  when (cid /= mdCID) (throwError $ CIDMismatch n cid mdCID)
  mBytes  <- cacheLookup mdCID ds
  metaDef <- either (throwError . NoDeserial) return (deserialiseOrFail mBytes)
  rBytes  <- cacheLookup (_anonDef metaDef) ds
  anonDef <- either (throwError . NoDeserial) return (deserialiseOrFail rBytes)
  aBytes  <- cacheLookup (_termAnon anonDef) ds
  anon    <- either (throwError . NoDeserial) return (deserialiseOrFail aBytes)
  mergeMeta anon (_termMeta metaDef)

separateMeta :: Name -> Term -> Defs -> Except DerefErr (Anon, Meta)
separateMeta n t ds = do
  case runState (runExceptT (go t [n])) (Meta (IM.singleton 0 n) IM.empty, 1) of
    (Left  err,_)   -> throwError err
    (Right a,(m,_)) -> return (a,m)

  where
    bind :: Name -> ExceptT DerefErr (State (Meta,Int)) ()
    bind n = modify (\(Meta ns cs,i) -> (Meta (IM.insert i n ns) cs, i))

    cids :: CID -> ExceptT DerefErr (State (Meta,Int)) ()
    cids c = modify (\(Meta ns cs,i) -> (Meta ns (IM.insert i c cs), i))

    bump :: ExceptT DerefErr (State (Meta,Int)) ()
    bump = modify (\(m,i) -> (m, i+1))

    go :: Term -> [Name] -> ExceptT DerefErr (State (Meta,Int)) Anon
    go t ctx = case t of
      Var n       -> case find n ctx of
        Just i -> return $ VarA i
        _      -> throwError $ FreeVariable n ctx
      Ref n     -> do
        c <- liftEither . runExcept $ anonymizeRef n ds
        bind n >> cids c >> return (RefA c)
      Lam n (Just (u,t)) b  -> do
        bind n >> bump
        t' <- go t ctx
        LamA (Just (u, t')) <$> go b (n:ctx)
      Lam n _ b             -> bind n >> bump >> LamA Nothing <$> go b (n:ctx)
      App f a -> bump >> AppA <$> go f ctx <*> go a ctx
      Let n u t x b         -> do
        bind n >> bump
        LetA u <$> go t ctx <*> go x (n:ctx) <*> go b (n:ctx)
      Typ                   -> return TypA
      All s n u t b         -> do
        bind s >> bump
        bind n >> bump
        AllA u <$> go t ctx <*> go b (n:s:ctx)

-- | Find a name in the binding context and return its index
lookupName :: Int -> [Name] -> Maybe Name
lookupName i []     = Nothing
lookupName i (x:xs)
  | i < 0     = Nothing
  | i == 0    = Just x
  | otherwise = lookupName (i - 1) xs

mergeMeta :: Anon -> Meta -> Except DerefErr Term
mergeMeta anon meta = do
  defName <- maybe (throwError $ MergeMissingNameAt 0) pure (_nams meta IM.!? 0)
  liftEither (evalState (runExceptT (go anon [defName])) 1)

  where
    bump :: ExceptT DerefErr (State Int) ()
    bump = modify (+1)

    go :: Anon -> [Name] -> ExceptT DerefErr (State Int) Term
    go t ctx = case t of
      VarA idx -> case lookupName idx ctx of
        Nothing -> get >>= (\i -> throwError $ MergeFreeVariableAt i ctx idx)
        Just n  -> return $ Var n
      RefA cid -> do
        i <- get
        n <- maybe (throwError $ MergeMissingNameAt i) pure (_nams meta IM.!? i)
        return $ Ref n
      LamA (Just (u,t)) b  -> do
        i <- get
        n <- maybe (throwError $ MergeMissingNameAt i) pure (_nams meta IM.!? i)
        bump
        t' <- go t ctx
        b' <- go b (n:ctx)
        return $ Lam n (Just (u,t')) b'
      LamA Nothing b  -> do
        i <- get
        n <- maybe (throwError $ MergeMissingNameAt i) pure (_nams meta IM.!? i)
        bump
        Lam n Nothing <$> go b (n:ctx)
      AppA f a   -> bump >> App <$> go f ctx <*> go a ctx
      LetA u t x b -> do
        i <- get
        n <- maybe (throwError $ MergeMissingNameAt i) pure (_nams meta IM.!? i)
        bump
        Let n u <$> go t ctx <*> go x (n:ctx) <*> go b (n:ctx)
      TypA         -> return Typ
      AllA u t b   -> do
        i <- get
        s <- maybe (throwError $ MergeMissingNameAt i) pure (_nams meta IM.!? i)
        bump
        j <- get
        n <- maybe (throwError $ MergeMissingNameAt i) pure (_nams meta IM.!? j)
        bump
        All s n u <$> go t ctx <*> go b (n:s:ctx)


