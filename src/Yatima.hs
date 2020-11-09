{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Yatima where

import           Codec.Serialise
import           Control.Monad.Catch
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Text
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import           Data.IORef
import           Data.IPLD.CID
import           Data.IPLD.DagJSON
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Sequence            (Seq (..))
import qualified Data.Sequence            as Seq
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Builder   as TL
import           Path
import           Path.IO

import           Debug.Trace

import qualified Yatima.Compiler.Scheme as Scheme
import qualified Yatima.Core            as Core
import           Yatima.Core.CheckError
import           Yatima.Core.Ctx        (Ctx, (<|))
import qualified Yatima.Core.Ctx        as Ctx
import           Yatima.Core.Hoas
import qualified Yatima.Core.IR         as IR
import           Yatima.IPFS.Client
import           Yatima.IPLD
import           Yatima.Package
import           Yatima.Parse           (parseTerm, unsafeParseTerm)
import qualified Yatima.Parse           as Parse
import           Yatima.Parse.Package
import qualified Yatima.Parse.Package   as Package
import           Yatima.Print           (prettyDef, prettyTerm)
import qualified Yatima.Print           as Print
import           Yatima.Term            (Def (..), Defs, Name, Term (..),
                                         Uses (..))
import qualified Yatima.Term            as Term

parseFilePath :: FilePath -> IO (Path Abs File)
parseFilePath file =
  catch @IO @PathException (parseAbsFile file) $ \e ->
  catch @IO @PathException (parseRelFile file >>= makeAbsolute) $ \ e ->
  fail ("Invalid File name: " ++ file)

loadFile :: FilePath -> IO (Path Abs Dir,CID,Package)
loadFile file = do
  path       <- parseFilePath file
  projectDir <- maybe (parent path) id <$> (findYatimaProjectDir (parent path))
  putStrLn $ concat ["Loading ", file, " from project ", toFilePath projectDir]
  env     <- newIORef (PackageEnv projectDir Set.empty M.empty)
  relPath <- makeRelative projectDir path
  (c,p)   <- withCurrentDir projectDir (pFile env relPath)
  return (projectDir,c,p)

loadCid :: CID -> IO Package
loadCid cid = do
  curDir  <- getCurrentDir
  putStrLn $ concat ["Loading ", show cid, " from cache"]
  cacheGet @Package cid

-- | Parse and pretty-print a file
prettyFile :: FilePath -> IO ()
prettyFile file = do
  (_,_,p) <- loadFile file
  let index@(Index ns) = _index p
  defs    <- indexToDefs index
  traverse (prettyIndexF defs) (M.toList ns)
  return ()

prettyIndexF :: Defs -> (Name,(CID,CID)) -> IO ()
prettyIndexF defs (nam,(cid,_)) = do
  putStrLn ""
  putStrLn $ T.unpack $ cidToText $ cid
  putStrLn $ T.unpack $ prettyDef nam (defs M.! cid)
  return ()

checkFile :: FilePath -> IO (CID,Package)
checkFile file = do
  (_,c,p) <- loadFile file
  path    <- parseFilePath file
  let index@(Index ns) = _index p
  defs    <- indexToDefs index
  traverse (checkRef defs) (M.toList ns)
  return (c,p)

checkCID :: CID -> IO (CID,Package)
checkCID cid = do
  p <- loadCid cid
  let index@(Index ns) = _index p
  defs    <- indexToDefs index
  traverse (checkRef defs) (M.toList ns)
  return (cid,p)

checkRef :: Defs -> (Name,(CID,CID)) -> IO ()
checkRef defs (name,(cid,_)) = do
  let (trm,typ) = defToHoas name (defs M.! cid)
  case runExcept $ Core.check defs Ctx.empty Once trm typ of
    Left  e -> putStrLn $ T.unpack $ T.concat 
        ["\ESC[31m\STX✗\ESC[m\STX ", name, "\n"
        , cidToText cid, "\n"
        , T.pack $ show e]
    Right (_,t,_) -> putStrLn $ T.unpack $ T.concat
        ["\ESC[32m\STX✓\ESC[m\STX ",name, ": ", printHoas t]

localPutCID :: CID -> IO ()
localPutCID cid = do
  resp <- runLocalDagPutCID cid
  case resp of
    Left e  -> putStrLn $ concat ["\ESC[31m\STX⚠ ",show cid," \ESC[m\STX ", show e]
    Right _ -> putStrLn $ concat ["\ESC[32m\STX📤 ", show cid, "\ESC[m\STX pinned"]

localPutPackageDeps :: CID -> IO ()
localPutPackageDeps cid = do
  pack <- cacheGet @Package cid
  putStrLn $ concat ["Pinning package ", T.unpack (_title pack), " ", show cid]
  cids <- Set.toList <$> packageDepCids pack
  traverse localPutCID cids
  localPutCID cid
  return ()

localGetCID :: CID -> IO ()
localGetCID cid = do
  hasCid <- cacheHas cid
  if hasCid 
  then do
    
    putStrLn $ concat ["\ESC[34m\STX📁 ", show cid, "\ESC[m\STX already in cache"]
    return ()
  else do
    bytes <- runLocalDagGetCID cid
    case eitherDecode' (BSL.fromStrict bytes) of
      Left e  -> putStrLn $ concat
        ["\ESC[31m\STX⚠ ",show cid,"\ESC[m\STX "
        , "JSON Parse Error: ", show e
        ]
      Right v -> do
        let bs   = serialise (v :: DagJSON)
        let cid' = makeCidFromBytes bs
        when (cid == cid') 
          (putStrLn $ concat ["\ESC[31m\STX⚠ CID",show cid," \ESC[m\STX "
          , "CID Mismatch with downloaded bytes: ", show cid'
          ])
        cachePutBytes bs
        putStrLn $ concat ["\ESC[32m\STX📥 ", show cid, "\ESC[m\STX downloaded from localhost"]

localGetPackageDeps :: CID -> IO ()
localGetPackageDeps cid = do
  localGetCID cid
  pack <- cacheGet @Package cid
  putStrLn $ concat ["Downloaded package ", T.unpack (_title pack)]
  cids <- Set.toList <$> packageDepCids pack
  traverse localGetCID cids
  putStrLn $ concat ["Downloaded dependencies for package ", T.unpack (_title pack)]
  return ()

showCIDJSON :: CID -> IO ()
showCIDJSON cid = do
  v <- cidDagJSON cid
  let txt = TL.toLazyText $ encodeToTextBuilder $ toAeson v
  T.putStrLn (TL.toStrict txt)

cidDagJSON :: CID -> IO DagJSON
cidDagJSON cid = do
  bs <- cacheGetBytes cid
  case (deserialiseOrFail @DagJSON bs) of
    Left e  -> fail $ concat
      ["\ESC[31m\STX⚠ ",show cid,"\ESC[m\STX "
      , "Deserialise Error: ", show e
      ]
    Right v -> return v

--infuraPutCID :: CID -> IO ()
--infuraPutCIDInfura = do
--  resp <- runInfuralDagPutCID cid
--  case resp of
--    Left e  -> putStrLn $ concat ["\ESC[31m\STX⚠ ",show cid," \ESC[m\STX ", show e]
--    Right _ -> putStrLn $ concat ["\ESC[32m\STX📤 ", show cid, "\ESC[m\STX "]
  

--compileFile :: FilePath -> IO ()
--compileFile file = do
--  (r,c,p) <- loadFile file
--  let index@(Index ns) = _index p
--  cache <- readCache r
--  codes <- forM (M.toList ns) (compileRef index cache)
--  putStrLn $ T.unpack $ T.concat codes
--
--compileRef ::  Index -> Cache -> (Name, (CID,CID)) -> IO Text
--compileRef index cache (name,(cid,_)) = do
--  def  <- liftIO $ catchErr $ derefDagDefCID name cid index cache
--  defs <- liftIO $ catchErr $ indexToDefs index cache
--  let (trm,typ) = defToHoas name def
--  case runExcept $ Core.check defs Ctx.empty Once trm typ of
--    Left  e -> ioError $ userError $ T.unpack $ T.concat
--        ["\ESC[31m\STX✗\ESC[m\STX ", name, "\n"
--        , cidToText cid, "\n"
--        , T.pack $ show e]
--    Right (_,_,c) -> return $ Scheme.defToCode name c
--
-- | Evaluate a `HOAS` from a file
normFile :: Name -> FilePath -> IO Hoas
normFile name file = do
  (r,c,p) <- loadFile file
  let index@(Index ns) = _index p
  defs    <- indexToDefs index
  case ns M.!? name of
    Nothing -> fail $ concat 
      ["undefined reference ", show name, " in package ", T.unpack (_title p)]
    Just (c,_)  -> return $ Core.norm defs (fst $ defToHoas name (defs M.! c))

normCID :: Name -> CID -> IO Hoas
normCID name cid = do
  p <- cacheGet @Package cid
  let index@(Index ns) = _index p
  defs    <- indexToDefs index
  case ns M.!? name of
    Nothing -> fail $ concat 
      ["undefined reference ", show name, " in package ", T.unpack (_title p)]
    Just (c,_)  -> return $ Core.norm defs (fst $ defToHoas name (defs M.! c))


-- | Evaluate a `HOAS` from a file
whnfDef :: Name -> FilePath -> IO Hoas
whnfDef name file = do
  (r,c,p) <- loadFile file
  let index@(Index ns) = _index p
  defs    <- indexToDefs index
  case ns M.!? name of
    Nothing -> fail $ concat 
      ["undefined reference ", show name, " in package ", T.unpack (_title p)]
    Just (c,_)  -> return $ Core.whnf defs (fst $ defToHoas name (defs M.! c))

whnf :: Defs -> Term -> Term
whnf defs = hoasToTerm 0 . Core.whnf defs . termToHoas []

norm :: Defs -> Term -> Term
norm defs = hoasToTerm 0 . Core.norm defs . termToHoas []

infer :: Defs -> Term -> Either CheckError Term
infer defs term =
  let hTerm = termToHoas [] term in
  case runExcept (Core.infer defs Ctx.empty Once hTerm) of
    Left err -> Left err
    Right (_,ty,_) -> Right (hoasToTerm 0 ty)

check :: Defs -> Term -> Term -> Either CheckError Term
check defs term typ_ =
  let hTerm = termToHoas [] term in
  let hType = termToHoas [] typ_ in
  case runExcept (Core.check defs Ctx.empty Once hTerm hType) of
    Left err     -> Left err
    Right (_,ty,_) -> Right (hoasToTerm 0 ty)

--prettyInfer :: Defs -> Term -> Text
--prettyInfer defs term = case infer defs term of
--  Left err -> prettyError err
--  Right ty -> prettyTerm ty
--
--prettyCheck :: Defs -> Term -> Term -> Text
--prettyCheck defs term typ_ = case check defs term typ_ of
--  Left err -> prettyError err
--  Right ty -> prettyTerm ty
