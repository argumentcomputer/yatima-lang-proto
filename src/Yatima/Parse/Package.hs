{-
Module      : Yatima.Parse.Package
Description : This module implements the import stanza parser for Yatima packages
Copyright   : 2020 Yatima Inc.
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module Yatima.Parse.Package where

import           Control.Monad.Except
import           Control.Monad.Catch
import           Control.Monad.RWS.Lazy     hiding (All)

import           Codec.Serialise
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           System.Exit
import           Path
import           Path.IO

import qualified Data.Text.IO               as TIO
import           Data.IORef

import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString            as BS
import           Data.Char
import           Debug.Trace

import           Data.IPLD.CID
import           Data.IPLD.DagAST
import           Yatima.IPLD
import           Yatima.Package
import           Yatima.Term
import           Yatima.Parse.Parser     hiding (parseDefault)
import           Yatima.Parse.Term
import           Yatima.Print

import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

data PackageError
  = UnknownPackage Name
  | InvalidLocalImport Name
  | MisnamedPackageImport Name Name CID
  | LocalImportCycle (Path Rel File)
  | ConflictingImportNames Name CID (Name,CID) (Name,CID)
  | MisnamedCacheFile (Path Abs File) CID CID
  | CacheFileNoDeserial (Path Abs File) CID DeserialiseFailure
  deriving (Eq,Ord,Show)

instance ShowErrorComponent PackageError where
--  showErrorComponent (CorruptDefs e) =
--    "ERR: The defined environment is corrupt: " ++ show e
  showErrorComponent (InvalidLocalImport name) =
    "Invalid Local Import: " ++ show name -- ++ ", " ++ show e
  showErrorComponent (UnknownPackage nam) =
    "Unknown package: " ++ T.unpack nam
  showErrorComponent (ConflictingImportNames p pc (n1,c1) (n2,c2)) = concat
    [ "Imported package, ", show p, "#", show pc
    , " which generates the following name conflict: \n"
    , "  - ", show n1, "#", show c1, "\n"
    , "  - ", show n2, "#", show c2, "\n"
    , "Names must bijectively map to content-identifiers."
    ]
  showErrorComponent (MisnamedPackageImport a b c) = concat
    [ "Package was imported declaring name ", show a
    , " but the package titles itself " , show b 
    , " at CID ", show c
    ]
  showErrorComponent (LocalImportCycle imp) = concat
    [ "Package import creates a cycle: ", show imp
    ]

type ParserIO a = Parser PackageError IO a

customIOFailure :: PackageError -> ParserIO a
customIOFailure = customFailure . ParseEnvironmentError

-- | A utility for running a `Parser`, since the `RWST` monad wraps `ParsecT`
parseIO :: Show a => ParserIO a -> ParseEnv -> String -> Text -> IO a
parseIO p env file txt = do
  a <- parseM p env file txt
  case a of
    Left  e -> putStr (errorBundlePretty e) >> exitFailure
    Right m -> return m

-- | A top level parser with default env and state
parseDefault :: Show a => ParserIO a -> Text -> IO a
parseDefault p s = parseIO p defaultParseEnv "" s

pPackageName :: (Ord e, Monad m) => Parser e m Text
pPackageName = label "a package name" $ do
  n  <- letterChar
  ns <- many (alphaNumChar <|> oneOf ("-" :: [Char]))
  let nam = T.pack (n : ns)
  return nam

cacheDir :: Path Abs Dir -> Path Abs Dir
cacheDir root = root </> [reldir|.yatima/cache|]

findYatimaRoot :: Path a Dir -> IO (Path Abs Dir)
findYatimaRoot initialDir = makeAbsolute initialDir >>= go
  where
    go :: Path Abs Dir -> IO (Path Abs Dir)
    go dir = do
      (ds,_) <- listDir dir
      let yati = dir </> [reldir|.yatima|]
      if | elem yati ds       -> return dir
         | parent dir == dir  -> initYatimaRoot initialDir
         | otherwise          -> go (parent dir)

initYatimaRoot :: Path a Dir -> IO (Path Abs Dir)
initYatimaRoot dir = do
  ensureDir (dir </> [reldir|.yatima/cache|])
  makeAbsolute dir

pCacheGet :: forall a. Serialise a => Path Abs Dir -> CID -> ParserIO a
pCacheGet cacheDir cid = do
  file <- liftIO $ parseRelFile $ T.unpack $ cidToText cid
  let path = cacheDir </> file
  bs <- liftIO $ BSL.readFile (toFilePath path)
  let cid' = makeCidFromBytes bs
  when (cid' /= cid) (customIOFailure $ MisnamedCacheFile path cid cid')
  case (deserialiseOrFail @a bs) of
    Left  e -> customIOFailure $ CacheFileNoDeserial path cid e
    Right a -> return a

cacheGet :: forall a. Serialise a => Path Abs Dir -> CID -> IO a
cacheGet cacheDir cid = do
  file <- parseRelFile $ T.unpack $ cidToText cid
  let path = cacheDir </> file
  bs <- BSL.readFile (toFilePath path)
  let cid' = makeCidFromBytes bs
  when (cid' /= cid) 
    (fail $ "Cache file contents do not match given CID: " ++ show cid)
  case (deserialiseOrFail @a bs) of
    Left  e -> fail $ "Cannot deserialise cache file: " ++ show e
    Right a -> return a

cachePut :: forall a. Serialise a => Path Abs Dir -> a -> IO CID
cachePut cacheDir x = do
  let cid = makeCid x
  file <- parseRelFile $ T.unpack $ cidToText cid
  let path = cacheDir </> file
  exists <- doesFileExist path
  unless exists (BSL.writeFile (toFilePath path) (serialise x))
  return cid

cachePutDef :: Path Abs Dir -> (Name,Def) ->  IO (Name,(CID,CID))
cachePutDef cacheDir (n,d@(Def doc trm typ)) = do
  termASTCid <- cachePut cacheDir (termToAST trm)
  typeASTCid <- cachePut cacheDir (termToAST typ)
  let termMeta = termToMeta trm
  let typeMeta = termToMeta typ
  let dagDef = DagDef termASTCid typeASTCid doc termMeta typeMeta
  dagDefCid <- cachePut cacheDir dagDef
  return (n,(dagDefCid,termASTCid))

data PackageEnv = PackageEnv
 { _root      :: Path Abs Dir
 , _openFiles :: Set (Path Rel File)
 , _doneFiles :: Map (Path Rel File) CID
 }

data From
  = Local Name (Path Rel File)
  | IPFS CID
  deriving (Eq,Show)

pImportDefs :: ParserIO ImportDefs
pImportDefs = do
  symbol "("
  defs <- choice
    [ symbol "..." >> return Full
    , (Some . Set.fromList) <$> (sepBy1 (pName True) (space >> symbol ","))
    ]
  string ")"
  return defs

makePath :: Name -> ParserIO (Path Rel File)
makePath n = liftIO go >>= \e -> either customIOFailure pure e
  where
    go :: IO (Either PackageError (Path Rel File))
    go = do
      let filepath = (T.unpack $ (T.intercalate "/" $ T.splitOn "." n) <> ".ya")
      catch @IO @PathException (Right <$> parseRelFile filepath) $ \e ->
        pure (Left $ InvalidLocalImport n)

-- | with <package-name> [as <alias] [(<import1>,<import2>)] [from <cid>]
pImport :: IORef PackageEnv -> ParserIO (Name,Text,ImportDefs,CID,Package)
pImport env = label "an import" $ do
  name <- symbol "with" >> pPackageName <* space
  alia <- maybe "" id   <$> (optional $ symbol "as" >> (pName False) <* space)
  defs <- maybe Full id <$> (optional $ pImportDefs <* space)
  from <- choice 
    [ Local name <$> makePath name
    -- , symbol' "from" >> choice [Local <$> makePath (T.splitOn "/" name)]
    ]
    -- try $ (space >> symbol "from" >> IPFS nam ali <$> pCid)
  case from of
    Local name path -> do
      open  <- _openFiles <$> (liftIO $ readIORef env)
      when (Set.member path open) (customIOFailure $ LocalImportCycle path)
      done  <- _doneFiles <$> (liftIO $ readIORef env)
      (cid,p) <- case M.lookup path done of
        Nothing  -> do
          exists <- doesFileExist path
          unless exists (customIOFailure $ InvalidLocalImport name)
          liftIO $ pFile env path
        Just cid -> do
          root <- _root <$> (liftIO $ readIORef env)
          p <- pCacheGet @Package (cacheDir root) cid
          return (cid,p)
      unless (name == (_title p))
        (customIOFailure $ MisnamedPackageImport name (_title p) cid)
      return (name,alia,defs,cid,p)

    --IPFS nam ali cid -> do
    --  p <- pDeserial @Package cid
    --  unless (nam == (_title p))
    --    (customIOFailure $ MisnamedPackageImport nam (_title p) cid)
    --  let pre = maybe "" (\x -> T.append x "/") ali
    --  return (nam,cid, M.mapKeys (T.append pre) (_index p))

pImports :: IORef PackageEnv -> Imports -> Index -> ParserIO (Imports, Index)
pImports env imp@(Imports is) ind@(Index ns) = next <|> (return (imp,ind))
  where
    next = do
      (nam,ali,ds,cid,p) <- pImport env <* space
      let Index ns = filterIndex (_index p) ds
      let prefix x = if ali == "" then x else ali <> "." <> x
      let ns' = M.mapKeys prefix ns
      case mergeIndex ind (Index ns') of
        Left (n1,c1,c2) ->
          customIOFailure $ ConflictingImportNames nam cid (n1,c1) (n1,c2)
        Right index -> do
          pImports env (Imports $ M.insert cid ali is) index

pPackage :: IORef PackageEnv -> CID -> FilePath -> ParserIO (CID,Package)
pPackage env cid file = do
  space
  doc     <- maybe "" id <$> (optional $ pDoc)
  title   <- maybe "" id <$> (optional $ symbol "package" >> pPackageName)
  space
  (imports, index@(Index ns)) <- pImports env emptyImports emptyIndex
  when (title /= "") (void $ symbol "where")
  defs  <- local (\e -> e { _refs = ns }) pDefs
  root  <- _root <$> (liftIO $ readIORef env)
  ns' <- liftIO $ traverse (cachePutDef (cacheDir root)) defs
  let (Right index) = mergeIndex (Index ns) (Index $ M.fromList ns')
  let pack = Package title doc cid imports index
  packCid <- liftIO $ cachePut (cacheDir root) pack
  return $ (packCid, pack)

-- | Parse a file
pFile :: IORef PackageEnv -> Path Rel File -> IO (CID,Package)
pFile env relPath = do
  root       <- _root <$> (liftIO $ readIORef env)
  let absPath = root </> relPath
  let file    = toFilePath absPath
  modifyIORef' env (\e -> e { _openFiles = Set.insert relPath (_openFiles e)})
  txt        <- TIO.readFile file
  sourceCid  <- cachePut (cacheDir root) (Source txt)
  (cid,pack) <- parseIO (pPackage env sourceCid file) defaultParseEnv file txt
  modifyIORef' env (\e -> e { _openFiles = Set.delete relPath (_openFiles e)})
  modifyIORef' env (\e -> e { _doneFiles = M.insert relPath cid (_doneFiles e)})
  putStrLn $ concat
    [ "parsed: ", (T.unpack $ _title pack), " "
    , show cid
    ]
  return (cid,pack)

pDoc :: (Ord e, Monad m) => Parser e m Text
pDoc = do
  d <- optional (string "{|" >> T.pack <$> (manyTill anySingle (symbol "|}")))
  return $ maybe "" id d

-- | Parse a definition
pDef :: (Ord e, Monad m) => Parser e m (Name, Def)
pDef = label "a definition" $ do
  doc <- pDoc
  symbol "def"
  (nam,exp,typ) <- pDecl False
  return $ (nam, Def doc exp typ)

-- | Parse a sequence of definitions, e.g. in a file
pDefs :: (Ord e, Monad m) => Parser e m [(Name,Def)]
pDefs = (space >> next) <|> (space >> eof >> (return []))
  where
  next = do
    rs <- asks _refs
    (n,d) <- pDef
    let (cid,cid') = defCid n d
    ds    <- local (\e -> e { _refs = M.insert n (cid, cid') (_refs e) }) pDefs
    return $ (n,d):ds


indexToDefs:: Path Abs Dir -> Index -> IO Defs
indexToDefs cacheDir i@(Index ns) = do
  ds <- traverse go (M.toList ns)
  return $ M.fromList ds
  where
    go :: (Name,(CID,CID)) -> IO (CID,Def)
    go (name,(defCid,trmCid)) = do
      dagDef  <- cacheGet @DagDef cacheDir defCid
      let (DagDef termASTCid typeASTCid doc termMeta typeMeta) = dagDef
      when (trmCid /= termASTCid) 
        (fail $ "indexToDefs failure: termAST CIDS don't match")
      termAST <- cacheGet @DagAST cacheDir termASTCid
      typeAST <- cacheGet @DagAST cacheDir typeASTCid
      case runExcept (dagToDef doc name (termAST,termMeta) (typeAST,typeMeta)) of
        Left  e -> putStrLn (show e) >> fail ""
        Right x -> return (defCid,x)

catchErr:: Show e => Except e a -> IO a
catchErr x = do
  case runExcept x of
    Right x -> return x
    Left  e -> putStrLn (show e) >> fail ""
