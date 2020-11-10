{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Yatima where

import Codec.Serialise
import Control.Monad.Catch
import Control.Monad.Except
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Aeson.Text
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Lazy as HM
import Data.IORef
import Data.IPLD.CID
import Data.IPLD.DagJSON
import Data.IPLD.DagPackage
import qualified Data.Map as M
import qualified Data.Set as Set
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
import Yatima.Term (Defs, Name, Term (..), Uses (..))

parseFilePath :: FilePath -> IO (Path Abs File)
parseFilePath file =
  catch @IO @PathException (parseAbsFile file) $ \_ ->
    catch @IO @PathException (parseRelFile file >>= makeAbsolute) $ \_ ->
      fail ("Invalid File name: " ++ file)

loadFile :: FilePath -> IO (Path Abs Dir, CID, DagPackage)
loadFile file = do
  path <- parseFilePath file
  projectDir <- maybe (parent path) id <$> (findYatimaProjectDir (parent path))
  putStrLn $ concat ["Loading ", file, " from project ", toFilePath projectDir]
  env <- newIORef (PackageEnv projectDir Set.empty M.empty)
  relPath <- makeRelative projectDir path
  (c, p) <- withCurrentDir projectDir (pFile env relPath)
  return (projectDir, c, p)

loadCid :: CID -> IO DagPackage
loadCid cid = do
  putStrLn $ concat ["Loading ", show cid, " from cache"]
  cacheGet @DagPackage cid

-- | Parse and pretty-print a file
prettyFile :: FilePath -> IO ()
prettyFile file = do
  (_, _, p) <- loadFile file
  let index@(Index ns) = _index p
  defs <- indexToDefs index
  traverse (prettyIndexF defs) (M.toList ns)
  return ()

prettyIndexF :: Defs -> (Name, (CID, CID)) -> IO ()
prettyIndexF defs (nam, (cid, _)) = do
  putStrLn ""
  putStrLn $ T.unpack $ cidToText $ cid
  putStrLn $ T.unpack $ prettyDef nam (defs M.! cid)
  return ()

checkFile :: FilePath -> IO (CID, DagPackage)
checkFile file = do
  (_, c, p) <- loadFile file
  let index@(Index ns) = _index p
  defs <- indexToDefs index
  traverse (checkRef defs) (M.toList ns)
  return (c, p)

checkCID :: CID -> IO (CID, DagPackage)
checkCID cid = do
  p <- loadCid cid
  let index@(Index ns) = _index p
  defs <- indexToDefs index
  traverse (checkRef defs) (M.toList ns)
  return (cid, p)

checkRef :: Defs -> (Name, (CID, CID)) -> IO ()
checkRef defs (name, (cid, _)) = do
  let (trm, typ) = defToHoas name (defs M.! cid)
  case runExcept $ Core.check defs Ctx.empty Once trm typ of
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

localPutCID :: CID -> IO ()
localPutCID cid = do
  msg <- T.unpack . dagYatimaDescription <$> cacheGet @DagYatima cid
  resp <- runLocalDagPutCID cid
  case resp of
    Left e ->
      putStrLn $
        concat
          ["\ESC[31m\STX⚠ ", show cid, " \ESC[m\STX ", msg, ". ", show e]
    Right _ ->
      putStrLn $
        concat
          ["\ESC[32m\STX📤 ", show cid, "\ESC[m\STX pinned ", msg]

localPutPackageDeps :: CID -> IO ()
localPutPackageDeps cid = do
  pack <- cacheGet @DagPackage cid
  localPutCID cid
  localPutCID (_sourceFile pack)
  traverse localPutCID (Set.toList $ packageIndexCids pack)
  traverse localPutPackageDeps (Set.toList $ packageImportCids pack)
  putStrLn $ concat ["Pinned package ", T.unpack (_packageTitle pack), " ", show cid, "to localhost"]
  return ()

localGetCID :: CID -> IO ()
localGetCID cid = do
  hasCid <- cacheHas cid
  if hasCid
    then do
      msg <- T.unpack . dagYatimaDescription <$> cacheGet @DagYatima cid
      putStrLn $
        concat
          ["\ESC[34m\STX📁 ", show cid, "\ESC[m\STX already cached ", msg]
      return ()
    else do
      bytes <- runLocalDagGetCID cid
      case eitherDecode' @DagJSON (BSL.fromStrict bytes) of
        Left e ->
          putStrLn $
            concat
              [ "\ESC[31m\STX⚠ ",
                show cid,
                "\ESC[m\STX ",
                "JSON Parse Error: ",
                show e
              ]
        Right v -> do
          let value = deserialise @DagYatima (serialise v)
          let cid' = makeCid value
          when
            (cid /= cid')
            ( putStrLn $
                concat
                  [ "\ESC[31m\STX⚠ CID",
                    show cid,
                    " \ESC[m\STX ",
                    "CID Mismatch with downloaded bytes: ",
                    show cid'
                  ]
            )
          cachePut @DagYatima value
          let msg = T.unpack $ dagYatimaDescription value
          putStrLn $
            concat
              [ "\ESC[32m\STX📥 ",
                show cid,
                "\ESC[m\STX downloaded ",
                msg
              ]

localGetPackageDeps :: CID -> IO ()
localGetPackageDeps cid = do
  localGetCID cid
  pack <- cacheGet @DagPackage cid
  localGetCID (_sourceFile pack)
  traverse localGetCID (Set.toList $ packageIndexCids pack)
  traverse localGetPackageDeps (Set.toList $ packageImportCids pack)
  putStrLn $
    concat
      ["Downloaded package ", T.unpack (_packageTitle pack), " from localhost"]
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
    Left e ->
      fail $
        concat
          [ "\ESC[31m\STX⚠ ",
            show cid,
            "\ESC[m\STX ",
            "Deserialise Error: ",
            show e
          ]
    Right v -> return v

infuraPutCID :: CID -> IO ()
infuraPutCID cid = do
  msg <- T.unpack . dagYatimaDescription <$> cacheGet @DagYatima cid
  let err x = putStrLn $ concat (["\ESC[31m\STX⚠ ", show cid, " \ESC[m\STX ", msg, ". "] ++ x)
  resp <- runInfuraDagPutCID cid
  unless (statusCode (responseStatus resp) == 200) (err [show resp])
  case Aeson.eitherDecode @DagJSON (responseBody resp) of
    Left e -> err [" received ", show resp, " but decoding errored with ", show e]
    Right (DagObject xs) -> do
      case HM.lookup "Cid" xs of
        Just (DagLink cid') -> do
          unless
            (cid == cid')
            (err ["CID Mismatch: received ", show cid', " instead of ", show cid])
          putStrLn $ concat ["\ESC[32m\STX📤 ", show cid, "\ESC[m\STX pinned ", msg]
        v -> err [" received\n", show resp, ". Expected a CID object, got ", show v]
    Right v -> err [" received\n", show resp, ". Expected a CID object, got ", show v]

infuraPutPackageDeps :: CID -> IO ()
infuraPutPackageDeps cid = do
  pack <- cacheGet @DagPackage cid
  infuraPutCID cid
  infuraPutCID (_sourceFile pack)
  traverse infuraPutCID (Set.toList $ packageIndexCids pack)
  traverse infuraPutPackageDeps (Set.toList $ packageImportCids pack)
  putStrLn $ concat ["Pinned package ", T.unpack (_packageTitle pack), " ", show cid, "to infura.io"]
  return ()

--infuraGetCID :: CID -> IO ()
--infuraGetCID cid = do
--  hasCid <- cacheHas cid
--  if hasCid
--  then do
--    msg  <- T.unpack . dagYatimaDescription <$> cacheGet @DagYatima cid
--    putStrLn $ concat
--      ["\ESC[34m\STX📁 ", show cid, "\ESC[m\STX already cached ", msg]
--    return ()
--  else do
--    resp <- runInfuraDagGetCID cid
--    let err x = putStrLn $ concat (["\ESC[31m\STX⚠ ",show cid," \ESC[m\STX "] ++ x)
--    unless (statusCode (responseStatus resp) == 200) (err [show resp])
--    case Aeson.eitherDecode @DagJSON (responseBody resp) of
--      Left e -> err [" received ", show resp , " but decoding errored with ", show e]
--      Right v@(DagObject xs) -> do
--        case HM.lookup "data" xs of
--          Just (DagText txt) -> do
--            let value = deserialise @DagYatima (serialise v)
--            let cid'    = makeCid value
--            when (cid /= cid')
--              (putStrLn $ concat ["\ESC[31m\STX⚠ CID",show cid," \ESC[m\STX "
--              , "CID Mismatch with downloaded bytes: ", show cid'
--              ])
--            putStrLn $ concat
--              ["\ESC[32m\STX📥 ", show cid, "\ESC[m\STX downloaded "
--              , msg
--              ]
--          v -> err [" received\n", show resp, ". Expected a CID object, got ", show v]
--      Right v -> err [" received\n", show resp , ". Expected a CID object, got ", show v]
--
--infuraGetPackageDeps :: CID -> IO ()
--infuraGetPackageDeps cid = do
--  infuraGetCID cid
--  pack <- cacheGet @DagPackage cid
--  infuraGetCID (_sourceFile pack)
--  putStrLn $ concat
--    ["Downloaded package ", T.unpack (_packageTitle pack), " from infurahost"]
--  traverse infuraGetCID (Set.toList $ packageIndexCids pack)
--  --traverse infuraGetPackageDeps (Set.toList $ packageImportCids pack)
--  putStrLn $ concat
--    ["Downloaded dependencies for package ", T.unpack (_packageTitle pack), " from infurahost"]
--  return ()

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
  (_, _, p) <- loadFile file
  let index@(Index ns) = _index p
  defs <- indexToDefs index
  case ns M.!? name of
    Nothing ->
      fail $
        concat
          ["undefined reference ", show name, " in package ", T.unpack (_packageTitle p)]
    Just (c, _) -> return $ Core.norm defs (fst $ defToHoas name (defs M.! c))

normCID :: Name -> CID -> IO Hoas
normCID name cid = do
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
  (_, _, p) <- loadFile file
  let index@(Index ns) = _index p
  defs <- indexToDefs index
  case ns M.!? name of
    Nothing ->
      fail $
        concat
          ["undefined reference ", show name, " in package ", T.unpack (_packageTitle p)]
    Just (c, _) -> return $ Core.whnf defs (fst $ defToHoas name (defs M.! c))

whnf :: Defs -> Term -> Term
whnf defs = hoasToTerm 0 . Core.whnf defs . termToHoas []

norm :: Defs -> Term -> Term
norm defs = hoasToTerm 0 . Core.norm defs . termToHoas []

infer :: Defs -> Term -> Either CheckError Term
infer defs term =
  let hTerm = termToHoas [] term
   in case runExcept (Core.infer defs Ctx.empty Once hTerm) of
        Left err -> Left err
        Right (_, ty, _) -> Right (hoasToTerm 0 ty)

check :: Defs -> Term -> Term -> Either CheckError Term
check defs term typ_ =
  let hTerm = termToHoas [] term
   in let hType = termToHoas [] typ_
       in case runExcept (Core.check defs Ctx.empty Once hTerm hType) of
            Left err -> Left err
            Right (_, ty, _) -> Right (hoasToTerm 0 ty)

--prettyInfer :: Defs -> Term -> Text
--prettyInfer defs term = case infer defs term of
--  Left err -> prettyError err
--  Right ty -> prettyTerm ty
--
--prettyCheck :: Defs -> Term -> Term -> Text
--prettyCheck defs term typ_ = case check defs term typ_ of
--  Left err -> prettyError err
--  Right ty -> prettyTerm ty
