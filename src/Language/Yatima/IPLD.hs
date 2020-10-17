{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Language.Yatima.IPLD
  ( -- | IPFS content-identifiers
    module Language.Yatima.CID
  , Package(..)
  , AST(..)
  , Meta(..)
  , ASTDef(..)
  , MetaDef(..)
  , IPLDErr(..)
  , Index(..)
  , File(..)
  , Cache
  , Imports
  , makeLink
  , termToAST
  , astToTerm
  , find
  , deserial
  , deref
  , derefMetaDef
  , derefMetaDefCID
  , emptyPackage
  , emptyIndex
  , mergeIndex
  , indexLookup
  , cacheLookup
  , insertDef
  , insertDefs
  , insertPackage
  , indexToDefs
  , usesToAST
  , validateTerm
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
data AST
  = Ctor Name [AST]
  | Bind AST
  | Vari Int
  | Link CID
  deriving (Eq,Show,Ord)

data Meta = Meta { _entries :: IntMap (Either Name CID) } deriving (Show,Eq)

-- | An anonymized `Def`
data ASTDef = ASTDef
  { _termAST :: CID
  , _typeAST :: CID
  } deriving Show

-- | The metadata from a `Def`
data MetaDef = MetaDef
  { _astDef    :: CID
  , _document  :: Text
  , _termMeta  :: Meta
  , _typeMeta  :: Meta
  } deriving Show

data Index = Index
  { _byName :: Map Name CID
  , _byCID  :: Map CID Name
  } deriving (Eq,Show)

emptyIndex = Index M.empty M.empty

mergeIndex :: Index -> Index -> Index
mergeIndex (Index a b) (Index c d) = Index (M.union a c) (M.union b d)

type Cache   = Map CID BS.ByteString
type Imports = Map CID Text

data File = File
  { _filepath :: FilePath
  , _contents :: Text
  }

data Package = Package
  { _title   :: Name
  , _descrip :: Text
  , _source  :: CID     -- link to generating file
  , _imports :: Imports
  , _index   :: Index
  } deriving (Show, Eq)

encodeAST :: AST -> Encoding
encodeAST term = case term of
  Ctor n ts -> encodeListLen 3
    <> encodeInt 0
    <> encodeString n
    <> encodeListLen (fromIntegral $ length ts)
       <> foldr (\v r -> encodeAST v <> r) mempty ts
  Bind t    -> encodeListLen 2 <> encodeInt 1 <> encodeAST t
  Vari idx  -> encodeListLen 2 <> encodeInt 2 <> encodeInt idx
  Link cid  -> encodeListLen 2 <> encodeInt 3 <> encodeCID cid

decodeAST :: Decoder s AST
decodeAST = do
  size <- decodeListLen
  tag  <- decodeInt
  case (size,tag) of
    (3,0) -> do
      ctor  <- decodeString
      arity <- decodeListLen
      args  <- replicateM arity decodeAST
      return $ Ctor ctor args
    (2,1) -> Bind <$> decodeAST
    (2,2) -> Vari <$> decodeInt
    (2,3) -> Link <$> decodeCID
    _     -> fail $ concat
      ["invalid AST with size: ", show size, " and tag: ", show tag]

instance Serialise AST where
  encode = encodeAST
  decode = decodeAST

encodeMeta :: Meta -> Encoding
encodeMeta (Meta m) = encodeMapLen (fromIntegral (IM.size m))
  <> IM.foldrWithKey go mempty m
  where
    go = (\k v r -> encodeString (T.pack $ show k) <> encodeEntry v <> r)
    encodeEntry (Left n)  = encodeInt 0 <> encodeString n
    encodeEntry (Right c) = encodeInt 1 <> encodeCID c

decodeMeta :: Decoder s Meta
decodeMeta = do
  size  <- decodeMapLen
  Meta . IM.fromList <$> replicateM size decodeEntry
  where
    decodeEntry = do
      keyString <- decodeString
      let  key = (read (T.unpack keyString) :: Int)
      tag <- decodeInt
      case tag of
        0 -> do
          n <- decodeString
          return (key,Left n)
        1 -> do
          c <- decodeCID
          return (key,Right c)
        _ -> fail "invalid Meta map entry"

instance Serialise Meta where
  encode = encodeMeta
  decode = decodeMeta

encodeASTDef :: ASTDef -> Encoding
encodeASTDef (ASTDef term typ_) = encodeListLen 3
  <> (encodeString "ASTDef")
  <> (encodeCID term)
  <> (encodeCID typ_)

decodeASTDef :: Decoder s ASTDef
decodeASTDef = do
  size     <- decodeListLen
  when (size /= 3) (fail $ "invalid ASTDef list size: " ++ show size)
  tag <- decodeString
  when (tag /= "ASTDef") (fail $ "invalid ASTDef tag: " ++ show tag)
  ASTDef <$> decodeCID <*> decodeCID

instance Serialise ASTDef where
  encode = encodeASTDef
  decode = decodeASTDef

encodeMetaDef :: MetaDef -> Encoding
encodeMetaDef (MetaDef anonDef doc termMeta typeMeta) = encodeListLen 5
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

encodeImports :: Imports -> Encoding
encodeImports is = encodeMapLen (fromIntegral $ M.size is ) <> go is
  where
    f (k,v) r = encodeCID k <> encodeString v <> r
    go is = foldr f mempty (sortBy cmp (M.toList is))
    cmp (k1,_) (k2,_)   = cborCanonicalOrder (serialise k1) (serialise k2)

decodeImports :: Decoder s Imports
decodeImports = do
  n      <- decodeMapLen
  M.fromList <$> replicateM n ((,) <$> decodeCID <*> decodeString)

encodeIndex :: Index -> Encoding
encodeIndex (Index byName byCID) = encodeListLen 3
  <> (encodeString  ("Index" :: Text))
  <> (encodeMapLen (fromIntegral $ M.size byName) <> encodeByName byName)
  <> (encodeMapLen (fromIntegral $ M.size byCID ) <> encodeByCID  byCID)
  where
    f (k,v) r = encodeString k <> encodeCID v <> r
    g (k,v) r = encodeCID k <> encodeString v <> r
    encodeByName byName = foldr f mempty (sortBy cmp (M.toList byName))
    encodeByCID byCID   = foldr g mempty (sortBy cmp (M.toList byCID))
    cmp (k1,_) (k2,_)   = cborCanonicalOrder (serialise k1) (serialise k2)

cborCanonicalOrder :: BSL.ByteString -> BSL.ByteString -> Ordering
cborCanonicalOrder x y
  | BSL.length x < BSL.length y = LT
  | BSL.length y > BSL.length x = GT
  | otherwise = compare x y

decodeIndex :: Decoder s Index
decodeIndex = do
  size     <- decodeListLen
  when (size /= 3) (fail $ "invalid Index list size: " ++ show size)
  tag    <- decodeString
  when (tag /= "Index") (fail $ "invalid Index tag: " ++ show tag)
  n      <- decodeMapLen
  byName <- M.fromList <$> replicateM n ((,) <$> decodeString <*> decodeCID)
  c      <- decodeMapLen
  byCID  <- M.fromList <$> replicateM c ((,) <$> decodeCID <*> decodeString)
  return $ Index byName byCID

instance Serialise Index where
  encode = encodeIndex
  decode = decodeIndex

encodeFile :: File -> Encoding
encodeFile file = encodeListLen 3
  <> (encodeString  ("File" :: Text))
  <> (encodeString  (T.pack $ _filepath file))
  <> (encodeString  (_contents file))

decodeFile :: Decoder s File
decodeFile = do
  size <- decodeListLen
  when (size /= 3) (fail $ "invalid File list size: " ++ show size)
  tag <- decodeString
  when (tag /= "File") (fail $ "invalid File tag: " ++ show tag)
  path <- T.unpack <$> decodeString
  File path <$> decodeString

instance Serialise File where
  encode = encodeFile
  decode = decodeFile

encodePackage :: Package -> Encoding
encodePackage package = encodeListLen 6
  <> (encodeString  ("Package" :: Text))
  <> (encodeString  (_title   package))
  <> (encodeString  (_descrip package))
  <> (encodeCID     (_source package))
  <> (encodeImports (_imports package))
  <> (encodeIndex   (_index   package))

decodePackage :: Decoder s Package
decodePackage = do
  size     <- decodeListLen
  when (size /= 6) (fail $ "invalid Package list size: " ++ show size)
  tag <- decodeString
  when (tag /= "Package") (fail $ "invalid Package tag: " ++ show tag)
  Package <$> decodeString <*> decodeString <*> decodeCID
  <*> decodeImports <*> decodeIndex

instance Serialise Package where
  encode = encodePackage
  decode = decodePackage

-- * Anonymization
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
  | UnexpectedCtor Name [AST] [Name] Int
  | UnexpectedBind AST [Name] Int
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
cacheLookup c d = maybe (throwError $ NotInCache c) pure (d M.!? c)

deserial :: (Serialise a, Monad m) => CID -> Cache -> ExceptT IPLDErr m a
deserial cid cache = do
  bytes <- cacheLookup cid cache
  either (throwError . NoDeserial) return (deserialiseOrFail $ BSL.fromStrict bytes)

makeLink :: Monad m => Name -> Index -> Cache -> ExceptT IPLDErr m (CID,CID)
makeLink n index cache = do
  cid     <- indexLookup n index
  metaDef <- deserial @MetaDef cid cache
  return (cid, _astDef metaDef)

deref :: Monad m => Name -> Index -> Cache -> ExceptT IPLDErr m Def
deref name index cache = do
  mdCID   <- indexLookup name index
  metaDef <- deserial @MetaDef mdCID cache
  def <- derefMetaDef name index metaDef cache
  return def

derefMetaDef :: Monad m => Name -> Index -> MetaDef -> Cache
             -> ExceptT IPLDErr m Def
derefMetaDef name index metaDef cache = do
  astDef  <- deserial @ASTDef (_astDef metaDef) cache
  termAST <- deserial @AST (_termAST astDef) cache
  term    <- astToTerm name index termAST (_termMeta metaDef)
  typeAST <- deserial @AST (_typeAST astDef) cache
  typ_    <- astToTerm name index typeAST (_typeMeta metaDef)
  return $ Def (_document metaDef) term typ_

derefMetaDefCID :: Monad m => Name -> CID -> Index -> Cache
                -> ExceptT IPLDErr m Def
derefMetaDefCID name cid index cache = do
  mdCID   <- indexLookup name index
  metaDef <- deserial @MetaDef mdCID cache
  when (cid /= mdCID) (throwError $ CIDMismatch name cid mdCID)
  def <- derefMetaDef name index metaDef cache
  --when (not $ name == _name def) (throwError $ NameMismatch name (_name def))
  return $ def

usesToAST :: Uses -> AST
usesToAST None = Ctor "None" []
usesToAST Affi = Ctor "Affi" []
usesToAST Once = Ctor "Once" []
usesToAST Many = Ctor "Many" []

termToAST :: Monad m => Name -> Term -> Index -> Cache
          -> ExceptT IPLDErr m (AST, Meta)
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

    go :: Term -> [Name] -> ExceptT IPLDErr (State (Meta,Int)) AST
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

-- | Find a name in the binding context and return its index
lookupNameCtx :: Int -> [Name] -> Maybe Name
lookupNameCtx i []     = Nothing
lookupNameCtx i (x:xs)
  | i < 0     = Nothing
  | i == 0    = Just x
  | otherwise = lookupNameCtx (i - 1) xs

astToTerm :: Monad m => Name -> Index -> AST -> Meta -> ExceptT IPLDErr m Term
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

    go :: AST -> [Name] -> ExceptT IPLDErr (State Int) Term
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
      Bind b    -> get >>= \i -> throwError $ UnexpectedBind b ctx i
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
insertDef name (Def doc term typ_) index cache = do
  (termAnon, termMeta) <- termToAST name term index cache
  (typeAnon, typeMeta) <- termToAST name typ_ index cache
  let termAnonCID = makeCID termAnon :: CID
  let typeAnonCID = makeCID typeAnon :: CID
  let anonDef     = ASTDef termAnonCID typeAnonCID
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

insertDefs :: Monad m => [(Name,Def)] -> Index -> Cache
           -> ExceptT IPLDErr m (Index,Cache)
insertDefs [] index cache = return (index,cache)
insertDefs ((n,d):ds) index cache = do
  (cid,cache') <- insertDef n d index cache
  let ns = M.insert n cid (_byName index)
  let cs = M.insert cid n (_byCID index)
  insertDefs ds (Index ns cs) cache'

emptyPackage :: Name -> Package
emptyPackage n = Package n "" (makeCID BSL.empty) M.empty emptyIndex

-- | Checks that all Refs in a term correspond to valid MetaDef cache entries
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
