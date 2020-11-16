{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Yatima where

import Codec.Serialise
import Control.Exception
import Control.Monad.Except
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.Text
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Lazy as HM
import Data.IORef
import Data.IPLD.Cid
import Data.IPLD.DagAST
import Data.IPLD.DagJSON
import Data.IPLD.DagPackage
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types.Status (statusCode)
import Path
import Path.IO
import qualified Yatima.Compiler.Scheme as Scheme
import qualified Yatima.Core as Core
import Yatima.Core.CheckError
import qualified Yatima.Core.Ctx as Ctx
import Yatima.Core.Hoas
import qualified Yatima.Core.IR as IR
import Yatima.IPFS.Client
import Yatima.IPLD
import Yatima.Parse.Package
import Yatima.Print (prettyDef)
import Yatima.Term (Defs, Loc (..), Name, Term (..), Uses (..))

parseFilePath :: FilePath -> IO (Path Abs File)
parseFilePath file =
  catch @PathException (parseAbsFile file) $ \_ ->
    catch @PathException (parseRelFile file >>= makeAbsolute) $ \e ->
      throw e

parseDirPath :: FilePath -> IO (Path Abs Dir)
parseDirPath dir =
  catch @PathException (parseAbsDir dir) $ \_ ->
    catch @PathException (parseRelDir dir >>= makeAbsolute) $ \e ->
      throw e

loadFile :: FilePath -> IO (Path Abs Dir, Defs, Text, Cid, DagPackage)
loadFile file = do
  path <- parseFilePath file
  projectDir <- maybe (parent path) id <$> (findYatimaProjectDir (parent path))
  --putStrLn $ concat ["Loading ", file, " from project ", toFilePath projectDir]
  env <- newIORef (PackageEnv projectDir Set.empty M.empty)
  relPath <- makeRelative projectDir path
  (txt, defs, c, p) <- withCurrentDir projectDir (pFile env relPath)
  return (projectDir, defs, txt, c, p)

loadCid :: Cid -> IO DagPackage
loadCid cid = do
  --putStrLn $ concat ["Loading ", show cid, " from cache"]
  cacheGet @DagPackage cid

-- | Parse and pretty-print a file
prettyFile :: FilePath -> IO ()
prettyFile file = do
  (_, _, _, _, p) <- loadFile file
  let index@(Index ns) = _index p
  defs <- indexToDefs index
  traverse (prettyIndexF defs) (M.toList ns)
  return ()

prettyIndexF :: Defs -> (Name, (Cid, Cid)) -> IO ()
prettyIndexF defs (nam, (cid, _)) = do
  putStrLn ""
  putStrLn $ T.unpack $ cidToText $ cid
  putStrLn $ T.unpack $ prettyDef nam (defs M.! cid)
  return ()

checkFile :: FilePath -> IO (Cid, DagPackage)
checkFile file = do
  (_, defs, txt, c, p) <- loadFile file
  let Index ns = _index p
  traverse (checkRef (Just txt) defs) (M.toList ns)
  putStrLn $
    concat
      [ "checked: ",
        (T.unpack $ _packageTitle p),
        " ",
        show c
      ]
  return (c, p)

checkCid :: Cid -> IO (Cid, DagPackage)
checkCid cid = do
  p <- loadCid cid
  let index@(Index ns) = _index p
  defs <- indexToDefs index
  traverse (checkRef Nothing defs) (M.toList ns)
  putStrLn $
    concat
      [ "checked: ",
        (T.unpack $ _packageTitle p),
        " ",
        show cid
      ]
  return (cid, p)

checkRef :: Maybe Text -> Defs -> (Name, (Cid, Cid)) -> IO ()
checkRef txt defs (name, (cid, _)) = do
  let (trm, typ) = defToHoas name (defs M.! cid)
  case runExcept $ Core.check txt defs Ctx.empty Once typ (TypH NoLoc) of
    Left e ->
      putStrLn $
        T.unpack $
          T.concat
            [ "\ESC[31m\STX✗\ESC[m\STX ",
              name,
              "\n",
              cidToText cid,
              "\n",
              T.pack $ show e
            ]
    Right (_, _, _) ->
      case runExcept $ Core.check txt defs Ctx.empty Once trm typ of
        Left e ->
          putStrLn $
            T.unpack $
              T.concat
                [ "\ESC[31m\STX✗\ESC[m\STX ",
                  name,
                  "\n",
                  cidToText cid,
                  "\n",
                  T.pack $ show e
                ]
        Right (_, t, _) ->
          putStrLn $
            T.unpack $
              T.concat
                ["\ESC[32m\STX✓\ESC[m\STX ", name, ": ", printHoas t]

localPutCid :: Cid -> IO ()
localPutCid cid = do
  msg <- T.unpack . dagYatimaDescription <$> cacheGet @DagYatima cid
  resp <- runLocalDagPut cid
  case resp of
    Left e ->
      putStrLn $
        concat
          ["\ESC[31m\STX⚠ ", show cid, " \ESC[m\STX ", msg, ". ", show e]
    Right _ ->
      putStrLn $
        concat
          ["\ESC[32m\STX📤 ", show cid, "\ESC[m\STX ", msg, " pinned to local daemon"]

localPutPackage :: Cid -> IO ()
localPutPackage cid = do
  pack <- cacheGet @DagPackage cid
  localPutCid cid
  localPutCid (_sourceFile pack)
  traverse localPutCid (Set.toList $ packageIndexCids pack)
  traverse localPutPackage (Set.toList $ packageImportCids pack)
  putStrLn $ concat ["package ", T.unpack (_packageTitle pack), " pinned to localhost with Cid:  ", show cid]
  return ()

localGetCid :: Cid -> IO ()
localGetCid cid = do
  hasCid <- cacheHas cid
  let err x = putStrLn $ concat (["\ESC[31m\STX⚠ ", show cid, " \ESC[m\STX ", ". "] ++ x)
  if hasCid
    then do
      msg <- T.unpack . dagYatimaDescription <$> cacheGet @DagYatima cid
      putStrLn $ concat ["\ESC[34m\STX📁 ", show cid, "\ESC[m\STX already cached ", msg]
      return ()
    else do
      bytes <- runLocalDagGet cid
      case eitherDecode' @DagJSON (BSL.fromStrict bytes) of
        Left e -> err ["JSON Parse Error: ", show e]
        Right v -> do
          let value = deserialise @DagYatima (serialise v)
          let cid' = makeCid value
          when (cid /= cid') (err ["Cid Mismatch on download: ", show cid'])
          cachePut @DagYatima value
          let msg = T.unpack $ dagYatimaDescription value
          putStrLn $ concat ["\ESC[32m\STX📥 ", show cid, "\ESC[m\STX ", msg, " downloaded from local daemon"]

localGetPackage :: DagPackage -> IO ()
localGetPackage pack = do
  localGetCid (_sourceFile pack)
  traverse localGetCid (Set.toList $ packageIndexCids pack)
  traverse localGet (Set.toList $ packageImportCids pack)
  putStrLn $ concat ["Downloaded package ", T.unpack (_packageTitle pack), " from local daemon"]
  return ()

localGet :: Cid -> IO ()
localGet cid = do
  localGetCid cid
  yati <- cacheGet @DagYatima cid
  case yati of
    YatimaPackage p -> localGetPackage p
    YatimaDef d -> localGetCid (_termAST d)
    YatimaSource _ -> return ()
    YatimaAST _ -> return ()

showCidJSON :: Cid -> IO ()
showCidJSON cid = do
  v <- cidDagJSON cid
  let txt = TL.toLazyText $ encodeToTextBuilder $ toAeson v
  T.putStrLn (TL.toStrict txt)

cidDagJSON :: Cid -> IO DagJSON
cidDagJSON cid = do
  bs <- cacheGetBytes cid
  case (deserialiseOrFail @DagJSON bs) of
    Left e -> fail $ concat ["\ESC[31m\STX⚠ ", show cid, "\ESC[m\STX ", "Deserialise Error: ", show e]
    Right v -> return v

infuraPutCid :: Cid -> IO ()
infuraPutCid cid = do
  msg <- T.unpack . dagYatimaDescription <$> cacheGet @DagYatima cid
  let err x = putStrLn $ concat (["\ESC[31m\STX⚠ ", show cid, " \ESC[m\STX ", msg, ". "] ++ x)
  resp <- runInfuraDagPutCid cid
  unless (statusCode (responseStatus resp) == 200) (err [show resp])
  case Aeson.eitherDecode @DagJSON (responseBody resp) of
    Left e -> err [" received ", show resp, " but decoding errored with ", show e]
    Right (DagObject xs) -> do
      case HM.lookup "Cid" xs of
        Just (DagLink cid') -> do
          unless (cid == cid') (err ["Cid Mismatch: received ", show cid', " instead of ", show cid])
          putStrLn $ concat ["\ESC[32m\STX📤 ", show cid, "\ESC[m\STX pinned ", msg]
        v -> err [" received\n", show resp, ". Expected a Cid object, got ", show v]
    Right v -> err [" received\n", show resp, ". Expected a Cid object, got ", show v]

infuraPutPackage :: Cid -> IO ()
infuraPutPackage cid = do
  pack <- cacheGet @DagPackage cid
  infuraPutCid cid
  infuraPutCid (_sourceFile pack)
  traverse infuraPutCid (Set.toList $ packageIndexCids pack)
  traverse infuraPutPackage (Set.toList $ packageImportCids pack)
  putStrLn $ concat ["Pinned package ", T.unpack (_packageTitle pack), " ", show cid, "to infura.io"]
  return ()

eternumPutCid :: Cid -> IO ()
eternumPutCid cid = do
  msg <- T.unpack . dagYatimaDescription <$> cacheGet @DagYatima cid
  let err x = putStrLn $ concat (["\ESC[31m\STX⚠ ", show cid, " \ESC[m\STX ", msg, " "] ++ x)
  localPutCid cid
  dir <- getYatimaGlobalDir
  resp <- runEternumPinHash (dir </> [relfile|eternum|]) cid
  case (statusCode (responseStatus resp)) of
    400 -> err [show $ responseBody resp]
    201 -> do
      putStrLn $ concat ["\ESC[32m\STX📤 ", show cid, "\ESC[m\STX ", msg, " pinned to eternum.io"]
      return ()
    _ -> err [show resp]

eternumPutPackage :: Cid -> IO ()
eternumPutPackage cid = do
  pack <- cacheGet @DagPackage cid
  eternumPutCid cid
  eternumPutCid (_sourceFile pack)
  traverse eternumPutCid (Set.toList $ packageIndexCids pack)
  traverse eternumPutPackage (Set.toList $ packageImportCids pack)
  putStrLn $ concat ["Pinned package ", T.unpack (_packageTitle pack), " ", show cid, "to eternum.io"]
  return ()

clonePackage :: Path Abs Dir -> Cid -> IO ()
clonePackage dir cid = do
  localGetCid cid
  yati <- cacheGet @DagYatima cid
  case yati of
    YatimaPackage p -> do
      localGetCid (_sourceFile p)
      (DagSource title txt) <- cacheGet @DagSource (_sourceFile p)
      file <- parseRelFile (T.unpack title)
      path <- addExtension ".ya" (dir </> file)
      T.writeFile (toFilePath $ path) txt
      traverse (clonePackage dir) (Set.toList $ packageImportCids p)
      return ()
    _ -> putStrLn $ concat ["Cid ", show cid, " does not point to a package"]

--compileFile :: FilePath -> IO ()
--compileFile file = do
--  (r,c,p) <- loadFile file
--  let index@(Index ns) = _index p
--  cache <- readCache r
--  codes <- forM (M.toList ns) (compileRef index cache)
--  putStrLn $ T.unpack $ T.concat codes
--
--compileRef ::  Index -> Cache -> (Name, (Cid,Cid)) -> IO Text
--compileRef index cache (name,(cid,_)) = do
--  def  <- liftIO $ catchErr $ derefDagDefCid name cid index cache
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
  (_, _, _, _, p) <- loadFile file
  let index@(Index ns) = _index p
  defs <- indexToDefs index
  case ns M.!? name of
    Nothing ->
      fail $
        concat
          ["undefined reference ", show name, " in package ", T.unpack (_packageTitle p)]
    Just (c, _) -> return $ Core.norm defs (fst $ defToHoas name (defs M.! c))

normCid :: Name -> Cid -> IO Hoas
normCid name cid = do
  p <- cacheGet @DagPackage cid
  let index@(Index ns) = _index p
  defs <- indexToDefs index
  case ns M.!? name of
    Nothing ->
      fail $
        concat
          ["undefined reference ", show name, " in package ", T.unpack (_packageTitle p)]
    Just (c, _) -> return $ Core.norm defs (fst $ defToHoas name (defs M.! c))

-- | Evaluate a `HOAS` from a file
whnfDef :: Name -> FilePath -> IO Hoas
whnfDef name file = do
  (_, _, _, _, p) <- loadFile file
  let index@(Index ns) = _index p
  defs <- indexToDefs index
  case ns M.!? name of
    Nothing ->
      fail $
        concat
          ["undefined reference ", show name, " in package ", T.unpack (_packageTitle p)]
    Just (c, _) -> return $ Core.whnf defs (fst $ defToHoas name (defs M.! c))

--whnf :: Defs -> Term -> Term
--whnf defs = hoasToTerm 0 . Core.whnf defs . termToHoas []
--
--norm :: Defs -> Term -> Term
--norm defs = hoasToTerm 0 . Core.norm defs . termToHoas []
--
--infer :: Defs -> Term -> Either CheckError Term
--infer defs term =
--  let hTerm = termToHoas [] term
--   in case runExcept (Core.infer defs Ctx.empty Once hTerm) of
--        Left err -> Left err
--        Right (_, ty, _) -> Right (hoasToTerm 0 ty)
--
--check :: Maybe Text -> Defs -> Term -> Term -> Either CheckError Term
--check defs term typ_ =
--  let hTerm = termToHoas [] term
--   in let hType = termToHoas [] typ_
--       in case runExcept (Core.check defs Ctx.empty Once hTerm hType) of
--            Left err -> Left err
--            Right (_, ty, _) -> Right (hoasToTerm 0 ty)
