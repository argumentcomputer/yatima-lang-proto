{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Yatima.IPLD
-- Description : This module implements the IPLD embedding for Yatima terms and
-- definitions, as well as related utilities.
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
module Yatima.IPLD where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.Monad.Except
import Control.Monad.Identity
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IPLD.Cid
import Data.IPLD.DagAST
import Data.IPLD.DagPackage
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T hiding (find)
import Path
import Path.IO
import Yatima.Term

termToAST :: Term -> DagAST
termToAST t = go t
  where
    go :: Term -> DagAST
    go t = case t of
      Var _ _ i -> Ctor "Var" [Vari i]
      Ref _ _ _ cid -> Ctor "Ref" [Link cid]
      Lit _ x -> Ctor "Lit" [Data (BSL.toStrict $ serialise x)]
      LTy _ x -> Ctor "LTy" [Data (BSL.toStrict $ serialise x)]
      Opr _ x -> Ctor "Opr" [Data (BSL.toStrict $ serialise x)]
      Lam _ _ b -> Ctor "Lam" [bind b]
      App _ f a -> Ctor "App" [go f, go a]
      New _ e -> Ctor "New" [go e]
      Use _ e -> Ctor "Use" [go e]
      Ann _ v t -> Ctor "Ann" [go v, go t]
      Let _ r _ u t x b -> Ctor "Let" [rec r, use u, go t, if r then bind x else go x, bind b]
      Typ _ -> Ctor "Typ" []
      All _ _ u t b -> Ctor "All" [use u, go t, bind b]
      Slf _ _ b -> Ctor "Slf" [bind b]

    rec r = Data (BSL.toStrict $ serialise r)

    use u = Data (BSL.toStrict $ serialise u)
    bind t = Bind (go t)

termToMeta :: Term -> DagMeta
termToMeta t = go t
  where
    go :: Term -> DagMeta
    go t = case t of
      Ref _ n cid _ -> MCtor [MLink n cid]
      Lam _ n b -> MCtor [bind n b]
      App _ f a -> MCtor [go f, go a]
      New _ e -> MCtor [go e]
      Use _ e -> MCtor [go e]
      Ann _ v t -> MCtor [go v, go t]
      Let _ r n _ t x b -> MCtor [leaf, leaf, go t, if r then bind n x else go x, bind n b]
      All _ n _ t b -> MCtor [leaf, go t, bind n b]
      Slf _ n b -> MCtor [bind n b]
      Typ _ -> MCtor []
      _ -> MCtor [leaf]
    bind n t = MBind n (go t)
    leaf = MLeaf

defToDag :: Def -> (DagAST, DagMeta, DagAST, DagMeta)
defToDag (Def _ _ _ term typ_) = runIdentity $ do
  let (termAST, termMeta) = (termToAST term, termToMeta term)
  let (typeAST, typeMeta) = (termToAST typ_, termToMeta typ_)
  return (termAST, termMeta, typeAST, typeMeta)

defToDagDef :: Def -> DagDef
defToDagDef def@(Def _ title doc _ _) = runIdentity $ do
  let (termAST, termMeta, typeAST, typeMeta) = defToDag def
  let termASTCid = makeCid termAST :: Cid
  return $ DagDef title termASTCid typeAST doc termMeta typeMeta

data DagError
  = FreeVariable [Name] DagAST DagMeta Int
  | NoDeserial [Name] DagAST DagMeta DeserialiseFailure
  | MalformedLet [Name] DagAST DagMeta
  | UnexpectedASTMeta [Name] DagAST DagMeta DagAST DagMeta
  deriving (Eq)

deriving instance Ord DeserialiseFailure

deriving instance Ord DagError

instance Show DagError where
  show e = case e of
    FreeVariable ctx ast meta idx ->
      concat
        [ "DagError: Free Variable ",
          show idx,
          "\ncontext: ",
          show ctx,
          "\nDagAST: ",
          show ast,
          "\nDagMeta: ",
          show meta
        ]
    NoDeserial ctx ast meta err ->
      concat
        [ "DagError: Deserialisation failure ",
          show err,
          "\ncontext: ",
          show ctx,
          "\nDagAST: ",
          show ast,
          "\nDagMeta: ",
          show meta
        ]
    MalformedLet ctx ast meta ->
      concat
        [ "DagError: Malformed Let constructor ",
          "\ncontext: ",
          show ctx,
          "\nDagAST: ",
          show ast,
          "\nDagMeta: ",
          show meta
        ]
    UnexpectedASTMeta ctx ast meta ast' meta' ->
      concat
        [ "DagError: Unexpected DagAST and DagMeta",
          "\nunexpected DagAST: ",
          show ast',
          "\nunexpected DagMeta: ",
          show meta',
          "\ncontext: ",
          show ctx,
          "\nentire DagAST: ",
          show ast,
          "\nentire DagMeta: ",
          show meta
        ]

dagToTerm :: [Name] -> DagAST -> DagMeta -> Except DagError Term
dagToTerm ns ast meta = case (ast, meta) of
  (Ctor "Ref" [Link t], MCtor [MLink n d]) -> return $ Ref l n d t
  (Ctor "Lit" [Data bs], MCtor [MLeaf]) -> Lit l <$> deserial @Literal bs
  (Ctor "LTy" [Data bs], MCtor [MLeaf]) -> LTy l <$> deserial @LitType bs
  (Ctor "Opr" [Data bs], MCtor [MLeaf]) -> Opr l <$> deserial @PrimOp bs
  (Ctor "Typ" [], MCtor []) -> return $ Typ l
  (Ctor "App" [f, a], MCtor [fm, am]) -> App l <$> go ns f fm <*> go ns a am
  (Ctor "Ann" [v, t], MCtor [vm, tm]) -> Ann l <$> go ns v vm <*> go ns t tm
  (Ctor "New" [e], MCtor [em]) -> New l <$> go ns e em
  (Ctor "Use" [e], MCtor [em]) -> Use l <$> go ns e em
  (Ctor "Lam" [Bind b], MCtor [MBind n m]) -> Lam l n <$> go (n : ns) b m
  (Ctor "Slf" [Bind b], MCtor [MBind n m]) -> Slf l n <$> go (n : ns) b m
  (Ctor "All" [Data u, t, Bind b], MCtor [MLeaf, tm, MBind n bm]) -> do
    All l n <$> deserial @Uses u <*> go ns t tm <*> go (n : ns) b bm
  ( Ctor "Let" [Data r, Data u, t, exp, Bind b],
    MCtor [MLeaf, MLeaf, tm, mexp, MBind n' bm]
    ) -> do
      rec <- deserial @Bool r
      case (rec, exp, mexp) of
        (True, Bind x, MBind n xm) -> do
          when (n /= n') (throwError $ MalformedLet ns ast meta)
          Let l True n' <$> deserial @Uses u
            <*> go ns t tm
            <*> go (n' : ns) x xm
            <*> go (n' : ns) b bm
        (False, x, xm) ->
          Let l False n' <$> deserial @Uses u
            <*> go ns t tm
            <*> go ns x xm
            <*> go (n' : ns) b bm
        _ -> throwError $ MalformedLet ns ast meta
  (Ctor "Var" [Vari i], MCtor [MLeaf]) ->
    case findByInt i ns of
      Just n -> return $ Var l n i
      Nothing -> throwError $ FreeVariable ns ast meta i
  (ast', meta') -> throwError $ UnexpectedASTMeta ns ast meta ast' meta'
  where
    l = NoLoc
    go = dagToTerm
    deserial :: Serialise a => BS.ByteString -> Except DagError a
    deserial bs = do
      let err = (NoDeserial ns ast meta)
      withExceptT err (liftEither $ deserialiseOrFail (BSL.fromStrict bs))

dagToDef ::
  Text ->
  Text ->
  Name ->
  (DagAST, DagMeta) ->
  (DagAST, DagMeta) ->
  Except DagError Def
dagToDef title doc n (termAST, termMeta) (typeAST, typeMeta) = do
  trm <- dagToTerm [n] termAST termMeta
  typ <- dagToTerm [] typeAST typeMeta
  return $ Def NoLoc title doc trm typ

-- * Cache

globalDir :: Path Abs Dir -> Path Abs Dir
globalDir root = root </> [reldir|.yatima_global|]

cacheDir :: Path Abs Dir -> Path Abs Dir
cacheDir root = globalDir root </> [reldir|cache|]

getYatimaGlobalDir :: IO (Path Abs Dir)
getYatimaGlobalDir = do
  homeDir <- getHomeDir
  ensureDir (globalDir homeDir)
  return (globalDir homeDir)

getYatimaCacheDir :: IO (Path Abs Dir)
getYatimaCacheDir = do
  homeDir <- getHomeDir
  ensureDir (cacheDir homeDir)
  return (cacheDir homeDir)

cacheGet :: forall a. Serialise a => Cid -> IO a
cacheGet cid = do
  bs <- cacheGetBytes cid
  case (deserialiseOrFail @a bs) of
    Left e -> fail $ "Cannot deserialise cache file: " ++ show e
    Right a -> return a

cacheGetBytes :: Cid -> IO BSL.ByteString
cacheGetBytes cid = do
  file <- parseRelFile $ T.unpack $ cidToText cid
  cacheDir <- getYatimaCacheDir
  let path = cacheDir </> file
  bs <- BSL.readFile (toFilePath path)
  let cid' = makeCidFromBytes bs
  when
    (cid' /= cid)
    (fail $ "Cache file contents do not match given Cid: " ++ show cid)
  return bs

cachePut :: forall a. Serialise a => a -> IO Cid
cachePut x = cachePutBytes (serialise x)

cachePutBytes :: BSL.ByteString -> IO Cid
cachePutBytes bs = do
  let cid = makeCidFromBytes bs
  cacheDir <- getYatimaCacheDir
  file <- parseRelFile $ T.unpack $ cidToText cid
  let path = cacheDir </> file
  exists <- doesFileExist path
  unless exists (BSL.writeFile (toFilePath path) bs)
  return cid

cacheHas :: Cid -> IO Bool
cacheHas cid = do
  cacheDir <- getYatimaCacheDir
  file <- parseRelFile $ T.unpack $ cidToText cid
  let path = cacheDir </> file
  doesFileExist path

cachePutDef :: Def -> IO (Cid, Cid)
cachePutDef (Def _ title doc trm typ) = do
  termASTCid <- cachePut (termToAST trm)
  let termMeta = termToMeta trm
  let typeMeta = termToMeta typ
  let dagDef = DagDef title termASTCid (termToAST typ) doc termMeta typeMeta
  dagDefCid <- cachePut dagDef
  return (dagDefCid, termASTCid)

indexToDefs :: Index -> IO Defs
indexToDefs (Index ns) = do
  ds <- traverse go (M.toList ns)
  return $ M.fromList ds
  where
    go :: (Name, (Cid, Cid)) -> IO (Cid, Def)
    go (name, (defCid, trmCid)) = do
      dagDef <- cacheGet @DagDef defCid
      let (DagDef title termASTCid typeAST doc termMeta typeMeta) = dagDef
      when
        (trmCid /= termASTCid)
        (fail $ "indexToDefs failure: termAST CidS don't match")
      termAST <- cacheGet @DagAST termASTCid
      case runExcept (dagToDef title doc name (termAST, termMeta) (typeAST, typeMeta)) of
        Left e -> putStrLn (show e) >> fail ""
        Right x -> return (defCid, x)

catchErr :: Show e => Except e a -> IO a
catchErr x = do
  case runExcept x of
    Right x -> return x
    Left e -> putStrLn (show e) >> fail ""

--dagDefDepCids :: Cid -> IO (Set Cid)
--dagDefDepCids cid = do
--  (DagDef _ termASTCid typeASTCid _ termMeta typeMeta) <- cacheGet @DagDef cid
--  termAST <- cacheGet @DagAST termASTCid
--  typeAST <- cacheGet @DagAST typeASTCid
--  return $ Set.unions
--    [ Set.singleton cid
--    , Set.singleton termASTCid
--    , Set.singleton termASTCid
--    , dagASTCids termAST
--    , dagASTCids typeAST
--    , dagMetaCids termMeta
--    , dagMetaCids typeMeta
--    ]
--
--packageDepCids :: DagPackage -> IO (Set Cid)
--packageDepCids (DagPackage _ _ srcCid (Imports ms) (Index ns)) = do
--  let impCids = Set.fromList $ fst <$> ms
--  defCids <- traverse dagDefDepCids (fst <$> M.elems ns)
--  return $ Set.unions $ impCids:(Set.singleton srcCid):defCids

data DagYatima
  = YatimaPackage DagPackage
  | YatimaDef DagDef
  | YatimaAST DagAST
  | YatimaSource DagSource
  deriving (Eq, Show)

encodeDagYatima :: DagYatima -> Encoding
encodeDagYatima term = case term of
  YatimaPackage pack -> encodeDagPackage pack
  YatimaDef def -> encodeDagDef def
  YatimaAST ast -> encodeDagAST ast
  YatimaSource src -> encodeDagSource src

decodeDagYatima :: Decoder s DagYatima
decodeDagYatima = do
  len <- decodeListLen
  tag <- decodeString
  case (len, tag) of
    (3, "DagSource") ->
      YatimaSource <$> (DagSource <$> decode <*> decode)
    (6, "DagPackage") ->
      YatimaPackage <$> (DagPackage <$> decode <*> decode <*> decode <*> decode <*> decode)
    (7, "DagDef") ->
      YatimaDef <$> (DagDef <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode)
    (3, "Ctor") ->
      YatimaAST <$> do
        ctor <- decodeString
        arity <- decodeListLen
        args <- replicateM arity decodeDagAST
        return $ Ctor ctor args
    (2, "Bind") -> YatimaAST <$> (Bind <$> decode)
    (2, "Vari") -> YatimaAST <$> (Vari <$> decode)
    (2, "Link") -> YatimaAST <$> (Link <$> decode)
    (2, "Data") -> YatimaAST <$> (Data <$> decodeData)
    (x, y) -> fail $ concat ["invalid DagYatima with size ", show x, " and tag ", show y]

instance Serialise DagYatima where
  encode = encodeDagYatima
  decode = decodeDagYatima

dagYatimaDescription :: DagYatima -> Text
dagYatimaDescription d = case d of
  YatimaPackage p -> T.concat ["package:    ", _packageTitle p]
  YatimaDef d -> T.concat ["definition: ", _title d]
  YatimaAST _ -> T.concat ["DagAST"]
  YatimaSource s -> T.concat ["source:     ", _srcTitle s]
