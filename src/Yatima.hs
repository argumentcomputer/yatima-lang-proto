{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Yatima where

import qualified Yatima.Compiler.Scheme as Scheme
import qualified Yatima.Core            as Core
import           Yatima.Core.Ctx        (Ctx, (<|))
import qualified Yatima.Core.Ctx        as Ctx
import           Yatima.Core.Hoas
import           Yatima.Core.CheckError
import qualified Yatima.Core.IR         as IR
import           Yatima.Parse           (parseTerm, unsafeParseTerm)
import qualified Yatima.Parse           as Parse
import           Yatima.Print           (prettyTerm, prettyDef)
import qualified Yatima.Print           as Print
import           Yatima.Term            (Def (..), Defs, Name, Term (..),
                                         Uses (..))
import qualified Yatima.Term            as Term

import           Data.IPLD.CID
import           Yatima.IPLD
import           Yatima.Package
import qualified Yatima.Parse.Package as Package
import           Yatima.Parse.Package

import           Control.Monad.Catch
import           Control.Monad.Except

import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import           Data.IORef
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Sequence        (Seq (..))
import qualified Data.Sequence        as Seq
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Debug.Trace

import           Path
import           Path.IO

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
