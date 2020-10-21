{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Yatima where

import           Yatima.CID
import           Yatima.Package
import           Yatima.Import
--import           Language.Yatima.IPFS
import           Yatima.IPLD
import           Yatima.Uses
import           Yatima.Ctx    (Ctx, (<|))
import qualified Yatima.Ctx    as Ctx
import           Yatima.Core   (CheckErr,HOAS (..),PreContext,defToHoas)
import qualified Yatima.Core   as Core
import           Yatima.Print  (prettyTerm)
import qualified Yatima.Print  as Print
import           Yatima.Parse  (parseTerm, unsafeParseTerm)
import qualified Yatima.Parse  as Parse
import           Yatima.Term   (Def (..), Defs, Name, Term (..))
import qualified Yatima.Term   as Term

import           Control.Monad.Except
import           Control.Monad.Catch

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.IORef
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Sequence          (Seq (..))
import qualified Data.Sequence          as Seq
import           Data.Set               (Set)
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Debug.Trace

import           Path
import           Path.IO

findYatimaRoot :: Path a Dir -> IO (Path Abs Dir)
findYatimaRoot dir = makeAbsolute dir >>= go
  where
    go :: Path Abs Dir -> IO (Path Abs Dir)
    go dir = do
      (ds,_) <- listDir dir
      let yati = dir </> [reldir|.yatima|]
      if | elem yati ds      -> return dir
         | parent dir == dir -> fail $
             "Could not find .yatima project root for: " ++ (toFilePath dir)
         | otherwise          -> go (parent dir)

initYatimaRoot :: Path a Dir -> IO ()
initYatimaRoot dir = do
  ensureDir (dir </> [reldir|.yatima/cache|])
  return ()

parseFilePath :: FilePath -> IO (Path Abs File)
parseFilePath file =
  catch @IO @PathException (parseAbsFile file) $ \e ->
  catch @IO @PathException (parseRelFile file >>= makeAbsolute) $ \ e ->
  fail ("Invalid File name: " ++ file)

loadFile :: FilePath -> IO (Path Abs Dir,CID,Package)
loadFile file = do
  path    <- parseFilePath file
  root    <- findYatimaRoot (parent path)
  putStrLn $ concat ["Loading ", file, " from root ", (toFilePath root)]
  env     <- newIORef (PackageEnv root Set.empty M.empty)
  relPath <- makeRelative root path
  (c,p)   <- withCurrentDir root (pFile env relPath)
  return (root,c,p)

-- | Parse and pretty-print a file
prettyFile :: FilePath -> IO ()
prettyFile file = do
  (r,c,p) <- loadFile file
  let index = _index p
  cache <- readCache r
  defs  <- catchErr $ indexToDefs index cache
  M.traverseWithKey (go index) defs
  return ()
  where
    go :: Index -> Name -> Def -> IO ()
    go index nam def = do
      putStrLn ""
      putStrLn $ T.unpack $ printCIDBase32 $ (_byName index) M.! nam
      putStrLn $ T.unpack $ Print.prettyDef nam def
      return ()

checkFile :: FilePath -> IO (CID,Package)
checkFile file = do
  (r,c,p) <- loadFile file
  let index = _index p
  cache <- readCache r
  forM_ (M.toList $ (_byName index)) (checkRef index cache)
  return (c,p)

checkRef ::  Index -> Cache -> (Name, CID) -> IO ()
checkRef index cache (name,cid) = do
  def  <- liftIO $ catchErr $ derefDagDefCID name cid index cache
  defs <- liftIO $ catchErr $ indexToDefs index cache
  let (trm,typ) = defToHoas name def
  case runExcept $ Core.check defs Ctx.empty Once trm typ of
    Left  e -> putStrLn $ T.unpack $ T.concat 
        ["\ESC[31m\STX✗\ESC[m\STX ", name, "\n"
        , printCIDBase32 cid, "\n"
        , T.pack $ show e]
    Right (_,t) -> putStrLn $ T.unpack $ T.concat
        ["\ESC[32m\STX✓\ESC[m\STX ",name, ": ", Core.printHOAS t]

-- | Evaluate a `HOAS` from a file
normDef :: Name -> FilePath -> IO HOAS
normDef name file = do
  (r,c,p) <- loadFile file
  let index = _index p
  cache <- readCache r
  cid   <- catchErr (indexLookup name index)
  def   <- catchErr (derefDagDefCID name cid index cache)
  defs  <- catchErr (indexToDefs index cache)
  return $ Core.norm defs (fst $ defToHoas name def)

whnfDef :: Name -> FilePath -> IO HOAS
whnfDef name file = do
  (r,c,p) <- loadFile file
  let index = _index p
  cache <- readCache r
  cid   <- catchErr (indexLookup name index)
  def   <- catchErr (derefDagDefCID name cid index cache)
  defs  <- catchErr (indexToDefs index cache)
  return $ Core.whnf defs (fst $ defToHoas name def)

whnf :: Defs -> Term -> Term
whnf defs =
  Core.hoasToTerm Ctx.empty . Core.whnf defs . Core.termToHoas Ctx.empty

norm :: Defs -> Term -> Term
norm defs =
  Core.hoasToTerm Ctx.empty . Core.norm defs . Core.termToHoas Ctx.empty

infer :: Defs -> Term -> Either CheckErr Term
infer defs term =
  let hTerm = Core.termToHoas Ctx.empty term in
  case runExcept (Core.infer defs Ctx.empty Once hTerm) of
    Left err -> Left err
    Right (_,ty) -> Right (Core.hoasToTerm Ctx.empty ty)

check :: Defs -> Term -> Term -> Either CheckErr Term
check defs term typ_ =
  let hTerm = Core.termToHoas Ctx.empty term in
  let hType = Core.termToHoas Ctx.empty typ_ in
  case runExcept (Core.check defs Ctx.empty Once hTerm hType) of
    Left err     -> Left err
    Right (_,ty) -> Right (Core.hoasToTerm Ctx.empty ty)

synth :: Defs -> Term -> Term -> Either CheckErr (Term, Term)
synth defs term typ_ =
  let hTerm = Core.termToHoas Ctx.empty term in
  let hType = Core.termToHoas Ctx.empty typ_ in
  case runExcept (Core.synth defs hTerm hType) of
    Left err -> Left err
    Right tt -> Right $
      (Core.hoasToTerm Ctx.empty (fst tt), Core.hoasToTerm Ctx.empty (snd tt))

prettyInfer :: Defs -> Term -> Text
prettyInfer defs term = case infer defs term of
  Left err -> Core.prettyError err
  Right ty -> Print.prettyTerm ty

prettyCheck :: Defs -> Term -> Term -> Text
prettyCheck defs term typ_ = case check defs term typ_ of
  Left err -> Core.prettyError err
  Right ty -> Print.prettyTerm ty

prettySynth :: Defs -> Term -> Term -> Text
prettySynth defs term typ_ = case synth defs term typ_ of
  Left err -> Core.prettyError err
  Right (ty,tr) -> T.concat [Print.prettyTerm tr, " :: ", Print.prettyTerm ty]

testSynth :: Defs -> Text -> Text -> IO ()
testSynth defs termCode typeCode = do
  let term = unsafeParseTerm termCode
  let typ_ = unsafeParseTerm typeCode
  putStrLn ("input-term: " ++ T.unpack (prettyTerm term))
  putStrLn ("input-type: " ++ T.unpack (prettyTerm typ_))
  case synth defs term typ_ of
    Left err -> print (T.unpack (Core.prettyError err))
    Right (sTerm, sTipo) -> do
      putStrLn ("synth-term: " ++ T.unpack (prettyTerm sTerm))
      putStrLn ("synth-type: " ++ T.unpack (prettyTerm sTipo))
