{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Yatima.Parse.Package
-- Description : This module implements the import stanza parser for Yatima packages
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
module Yatima.Parse.Package where

import Codec.Serialise
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.RWS.Lazy hiding (All)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.IORef
import Data.IPLD.CID
import Data.IPLD.DagAST
import Data.IPLD.DagPackage
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import Path
import Path.IO
import System.Exit
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L
import Yatima.IPLD
import Yatima.Parse.Parser hiding (parseDefault)
import Yatima.Parse.Term
import Yatima.Print
import Yatima.Term

data PackageError
  = UnknownPackage Name
  | InvalidLocalImport Name
  | MisnamedPackageImport Name Name CID
  | LocalImportCycle (Path Rel File)
  | ConflictingImportNames Name CID (Name, CID) (Name, CID)
  | MisnamedCacheFile (Path Abs File) CID CID
  | CacheFileNoDeserial (Path Abs File) CID DeserialiseFailure
  deriving (Eq, Ord, Show)

instance ShowErrorComponent PackageError where
  --  showErrorComponent (CorruptDefs e) =
  --    "ERR: The defined environment is corrupt: " ++ show e
  showErrorComponent (InvalidLocalImport name) =
    "Invalid Local Import: " ++ show name -- ++ ", " ++ show e
  showErrorComponent (UnknownPackage nam) =
    "Unknown package: " ++ T.unpack nam
  showErrorComponent (ConflictingImportNames p pc (n1, c1) (n2, c2)) =
    concat
      [ "Imported package, ",
        show p,
        "#",
        show pc,
        " which generates the following name conflict: \n",
        "  - ",
        show n1,
        "#",
        show c1,
        "\n",
        "  - ",
        show n2,
        "#",
        show c2,
        "\n",
        "Names must bijectively map to content-identifiers."
      ]
  showErrorComponent (MisnamedPackageImport a b c) =
    concat
      [ "Package was imported declaring name ",
        show a,
        " but the package titles itself ",
        show b,
        " at CID ",
        show c
      ]
  showErrorComponent (LocalImportCycle imp) =
    concat
      [ "Package import creates a cycle: ",
        show imp
      ]

type ParserIO a = Parser PackageError IO a

customIOFailure :: PackageError -> ParserIO a
customIOFailure = customFailure . ParseEnvironmentError

-- | A utility for running a `Parser`, since the `RWST` monad wraps `ParsecT`
parseIO :: Show a => ParserIO a -> ParseEnv -> String -> Text -> IO a
parseIO p env file txt = do
  a <- parseM p env file txt
  case a of
    Left e -> putStr (errorBundlePretty e) >> exitFailure
    Right m -> return m

-- | A top level parser with default env and state
parseDefault :: Show a => ParserIO a -> Text -> IO a
parseDefault p s = parseIO p defaultParseEnv "" s

pPackageName :: (Ord e, Monad m) => Parser e m Text
pPackageName = label "a package name" $ do
  n <- letterChar
  ns <- many (alphaNumChar <|> oneOf ("-" :: [Char]))
  let nam = T.pack (n : ns)
  return nam

findYatimaProjectDir :: Path a Dir -> IO (Maybe (Path Abs Dir))
findYatimaProjectDir initialDir = makeAbsolute initialDir >>= go
  where
    go :: Path Abs Dir -> IO (Maybe (Path Abs Dir))
    go dir = do
      (ds, _) <- listDir dir
      let yati = dir </> [reldir|.yatima|]
      if
          | elem yati ds -> return (Just dir)
          | parent dir == dir -> return Nothing
          | otherwise -> go (parent dir)

initYatimaProject :: Path a Dir -> IO (Path Abs Dir)
initYatimaProject dir = do
  ensureDir (dir </> [reldir|.yatima|])
  makeAbsolute dir

pCacheGet :: forall a. Serialise a => CID -> ParserIO a
pCacheGet cid = do
  file <- liftIO $ parseRelFile $ T.unpack $ cidToText cid
  cacheDir <- liftIO $ getYatimaCacheDir
  let path = cacheDir </> file
  bs <- liftIO $ BSL.readFile (toFilePath path)
  let cid' = makeCidFromBytes bs
  when (cid' /= cid) (customIOFailure $ MisnamedCacheFile path cid cid')
  case (deserialiseOrFail @a bs) of
    Left e -> customIOFailure $ CacheFileNoDeserial path cid e
    Right a -> return a

data PackageEnv = PackageEnv
  { _root :: Path Abs Dir,
    _openFiles :: Set (Path Rel File),
    _doneFiles :: Map (Path Rel File) CID
  }

data From
  = Local Name (Path Rel File)
  | IPFS CID
  deriving (Eq, Show)

pImportDefs :: ParserIO ImportDefs
pImportDefs = do
  symbol "("
  defs <-
    choice
      [ symbol "..." >> return Full,
        (Some . Set.fromList) <$> (sepBy1 (pName True) (space >> symbol ","))
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
pImport :: IORef PackageEnv -> ParserIO (Name, Text, ImportDefs, CID, DagPackage)
pImport env = label "an import" $ do
  name <- symbol "with" >> pPackageName <* space
  alia <- maybe "" id <$> (optional $ symbol "as" >> (pName False) <* space)
  defs <- maybe Full id <$> (optional $ pImportDefs <* space)
  from <-
    choice
      [ Local name <$> makePath name
      -- , symbol' "from" >> choice [Local <$> makePath (T.splitOn "/" name)]
      ]
  -- try $ (space >> symbol "from" >> IPFS nam ali <$> pCid)
  case from of
    Local name path -> do
      open <- _openFiles <$> (liftIO $ readIORef env)
      when (Set.member path open) (customIOFailure $ LocalImportCycle path)
      done <- _doneFiles <$> (liftIO $ readIORef env)
      (cid, p) <- case M.lookup path done of
        Nothing -> do
          exists <- doesFileExist path
          unless exists (customIOFailure $ InvalidLocalImport name)
          liftIO $ pFile env path
        Just cid -> do
          root <- _root <$> (liftIO $ readIORef env)
          p <- pCacheGet @DagPackage cid
          return (cid, p)
      unless
        (name == (_packageTitle p))
        (customIOFailure $ MisnamedPackageImport name (_packageTitle p) cid)
      return (name, alia, defs, cid, p)

--IPFS nam ali cid -> do
--  p <- pDeserial @Package cid
--  unless (nam == (_title p))
--    (customIOFailure $ MisnamedPackageImport nam (_title p) cid)
--  let pre = maybe "" (\x -> T.append x "/") ali
--  return (nam,cid, M.mapKeys (T.append pre) (_index p))

pImports :: IORef PackageEnv -> Imports -> Index -> ParserIO (Imports, Index)
pImports env imp@(Imports is) ind@(Index ns) = next <|> (return (imp, ind))
  where
    next = do
      (nam, ali, ds, cid, p) <- pImport env <* space
      let Index ns = filterIndex (_index p) ds
      let prefix x = if ali == "" then x else ali <> "." <> x
      let ns' = M.mapKeys prefix ns
      case mergeIndex ind (Index ns') of
        Left (n1, c1, c2) ->
          customIOFailure $ ConflictingImportNames nam cid (n1, c1) (n1, c2)
        Right index -> do
          pImports env (Imports $ is ++ [(cid, ali)]) index

pPackage :: IORef PackageEnv -> Text -> FilePath -> ParserIO (CID, DagSource, DagPackage)
pPackage env src file = do
  space
  doc <- maybe "" id <$> (optional $ pDoc)
  title <- maybe "" id <$> (optional $ symbol "package" >> pPackageName)
  let source = DagSource title src
  space
  (imports, index@(Index ns)) <- pImports env emptyImports emptyIndex
  when (title /= "") (void $ symbol "where")
  defs <- local (\e -> e {_refs = ns}) pDefs
  root <- _root <$> (liftIO $ readIORef env)
  let (Right index) = mergeIndex (Index ns) (Index $ M.fromList defs)
  let pack = DagPackage title doc (makeCid source) imports index
  packCid <- liftIO $ cachePut pack
  return $ (packCid, source, pack)

-- | Parse a file
pFile :: IORef PackageEnv -> Path Rel File -> IO (CID, DagPackage)
pFile env relPath = do
  root <- _root <$> (liftIO $ readIORef env)
  let absPath = root </> relPath
  let file = toFilePath absPath
  modifyIORef' env (\e -> e {_openFiles = Set.insert relPath (_openFiles e)})
  txt <- TIO.readFile file
  (cid, src, pack) <- parseIO (pPackage env txt file) defaultParseEnv file txt
  cachePut src
  modifyIORef' env (\e -> e {_openFiles = Set.delete relPath (_openFiles e)})
  modifyIORef' env (\e -> e {_doneFiles = M.insert relPath cid (_doneFiles e)})
  putStrLn $
    concat
      [ "parsed: ",
        (T.unpack $ _packageTitle pack),
        " ",
        show cid
      ]
  return (cid, pack)

pDoc :: (Ord e, Monad m) => Parser e m Text
pDoc = do
  d <- optional (string "{|" >> T.pack <$> (manyTill anySingle (symbol "|}")))
  return $ maybe "" id d

-- | Parse a definition
pDef :: (Ord e, Monad m) => Parser e m (Name, Def)
pDef = label "a definition" $ do
  doc <- pDoc
  symbol "def"
  (nam, exp, typ) <- pDecl False
  return $ (nam, Def nam doc exp typ)

-- | Parse a sequence of definitions, e.g. in a file
pDefs :: ParserIO [(Name, (CID, CID))]
pDefs = (space >> next) <|> (space >> eof >> (return []))
  where
    next = do
      rs <- asks _refs
      (n, d) <- pDef
      cids <- liftIO $ cachePutDef d
      ds <- local (\e -> e {_refs = M.insert n cids (_refs e)}) pDefs
      return $ (n, cids) : ds
