{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Yatima.Import where

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

import           Yatima.CID
import           Yatima.Term
import           Yatima.Package
import           Yatima.IPLD
import           Yatima.Parse      hiding (parseDefault)
import           Yatima.Print

import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char       hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

data ImportErr
  = UnknownPackage Name
  | CorruptDefs IPLDErr
  | InvalidCID String Text
  | InvalidLocalImport Name
  | MisnamedPackageImport Name Name CID
  | LocalImportCycle (Path Rel File)
  | ConflictingImportNames Name CID (Name,CID) (Name,CID)
  deriving (Eq,Ord,Show)

instance ShowErrorComponent ImportErr where
  showErrorComponent (CorruptDefs e) =
    "ERR: The defined environment is corrupt: " ++ show e
  showErrorComponent (InvalidCID err txt) =
    "Invalid CID: " ++ show txt ++ ", " ++ err
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

type ParserIO a = Parser ImportErr IO a

customIOFailure :: ImportErr -> ParserIO a
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

pInsertDefs :: Path Abs Dir -> [(Name,Def)] -> Index -> ParserIO Index
pInsertDefs dir defs index = do
  cache <- liftIO $ readCache dir
  case runExcept (insertDefs defs index cache) of
    Left  e -> customIOFailure $ CorruptDefs e
    Right (index,cache) -> do
      liftIO $ writeCache dir cache
      return $ index

pDeserial :: forall a. Serialise a => Path Abs Dir -> CID -> ParserIO a
pDeserial dir cid = do
  cache <- liftIO $ readCache dir
  case runExcept (deserial @a cid cache) of
    Left  e -> customIOFailure $ CorruptDefs $ e
    Right a -> return a

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

pCID :: Monad m => Parser ImportErr m CID
pCID = do
  txt <- T.pack <$> (many alphaNumChar)
  case cidFromText txt of
    Left  err  -> customFailure $ ParseEnvironmentError $ InvalidCID err txt
    Right cid  -> return $ cid

makePath :: Name -> ParserIO (Path Rel File)
makePath n = liftIO go >>= \e -> either customIOFailure pure e
  where
    go :: IO (Either ImportErr (Path Rel File))
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
          bs <- liftIO $ BS.readFile (toFilePath $ root </> path)
          p  <- either (customIOFailure . CorruptDefs . NoDeserial . pure) pure
                  (deserialiseOrFail $ BSL.fromStrict bs)
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
    --Local nam ali -> do
    --  open  <- _open <$> (liftIO $ readIORef env)
    --  let file = (T.unpack nam ++ ".ya")
    --  when (Set.member file open) (customIOFailure $ LocalImportCycle file)
    --  (cid,p) <- liftIO $ pFile env (T.unpack nam ++ ".ya")
    --  unless (nam == (_title p))
    --    (customIOFailure $ MisnamedPackageImport nam (_title p) cid)
    --  return (nam,ali,cid,p)

pImports :: IORef PackageEnv -> Imports -> Index -> ParserIO (Imports, Index)
pImports env imp@(Imports is) ind@(Index ns cs) = next <|> (return (imp,ind))
  where
    next = do
      (nam,ali,ds,cid,p) <- pImport env <* space
      let Index ns' cs' = filterIndex (_index p) ds
      let prefix x = if ali == "" then x else ali <> "." <> x
      let ind' = Index (M.mapKeys prefix ns') (prefix <$> cs')
      let conflict = M.intersection ns (_byName ind')
      case mergeIndex ind ind' of
        Left (n1,c1,n2,c2) ->
          customIOFailure $ ConflictingImportNames nam cid (n1,c1) (n2,c2)
        Right index -> 
          pImports env (Imports $ M.insert cid ali is) index

pPackage :: IORef PackageEnv -> CID -> FilePath -> ParserIO (CID,Package)
pPackage env cid file = do
  space
  doc     <- maybe "" id <$> (optional $ pDoc)
  title   <- maybe "" id <$> (optional $ symbol "package" >> pPackageName)
  space
  (imports, index) <- pImports env emptyImports emptyIndex
  when (title /= "") (void $ symbol "where")
  defs  <- local (\e -> e { _refs = M.keysSet (_byName index) }) pDefs
  root <- _root <$> (liftIO $ readIORef env)
  cache <- liftIO $ readCache root
  (index, cache) <- case runExcept (insertDefs defs index cache) of
    Left e              -> customIOFailure $ CorruptDefs e
    Right (index,cache) -> return (index,cache)
  let pack = Package title doc cid imports index
  let (cid,cache')   = insertPackage pack cache
  liftIO $ writeCache root cache'
  return $ (cid, pack)

-- | Parse a file
pFile :: IORef PackageEnv -> Path Rel File -> IO (CID,Package)
pFile env relPath = do
  root <- _root <$> (liftIO $ readIORef env)
  let absPath = root </> relPath
  let file = toFilePath absPath
  modifyIORef' env (\e -> e { _openFiles = Set.insert relPath (_openFiles e)})
  txt   <- TIO.readFile file
  cache <- readCache root
  let (fileCID, cache') = insertSource (Source txt) cache
  writeCache root cache'
  (cid,pack)  <- parseIO (pPackage env fileCID file) defaultParseEnv file txt
  modifyIORef' env (\e -> e { _openFiles = Set.delete relPath (_openFiles e)})
  modifyIORef' env (\e -> e { _doneFiles = M.insert relPath cid (_doneFiles e)})
  putStrLn $ concat
    [ "parsed: ", (T.unpack $ _title pack), " "
    , (T.unpack $ printCIDBase32 cid)
    ]
  return (cid,pack)

--readPackages :: IO (Map Name CID)
--readPackages = do
--  createDirectoryIfMissing True ".yatima/packages"
--  ns <- listDirectory ".yatima/packages"
--  M.fromList <$> traverse go ns
--  where
--    go :: FilePath -> IO (Name,CID)
--    go f = do
--      txt <- TIO.readFile (".yatima/packages/" ++ f)
--      case cidFromText txt of
--        Left e  -> error $ "CORRUPT PACKAGE INDEX ENTRY: " ++ f ++ ", " ++ e
--        Right c -> return (T.pack f, c)
--
--writePackage :: Name -> CID -> IO ()
--writePackage title cid = do
--  createDirectoryIfMissing True ".yatima/packages"
--  let file = (".yatima/packages/" ++ (T.unpack $ title))
--  TIO.writeFile file (printCIDBase32 cid)

catchErr:: Show e => Except e a -> IO a
catchErr x = do
  case runExcept x of
    Right x -> return x
    Left  e -> putStrLn (show e) >> fail ""

