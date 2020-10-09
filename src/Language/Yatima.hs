{-# LANGUAGE AllowAmbiguousTypes #-}
module Language.Yatima where

import Language.Yatima.Import
import Language.Yatima.IPFS
import Language.Yatima.IPLD
import Language.Yatima.Uses

import qualified Language.Yatima.Ctx as Ctx
import           Language.Yatima.Ctx (Ctx, (<|))

import qualified Language.Yatima.Core as Core
import Language.Yatima.Core (HOAS(..), PreContext, CheckErr, defToHoas)

import Language.Yatima.Print (prettyTerm)
import qualified Language.Yatima.Print as Print

import Language.Yatima.Parse (parseTerm, unsafeParseTerm)
import qualified Language.Yatima.Parse as Parse

import qualified Language.Yatima.Term as Term
import Language.Yatima.Term (Term(..), Name, Def(..), Defs)

import           Control.Monad.Except
import           Data.IORef
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

import           System.Directory

loadFile :: FilePath -> FilePath -> IO (CID,Package)
loadFile root file = do
  unless (root == "") (setCurrentDirectory root)
  cwd <- getCurrentDirectory
  putStrLn $ concat ["Loading ", file, " from ", cwd]
  createDirectoryIfMissing True ".yatima/cache"
  env   <- newIORef (PackageEnv root Set.empty Set.empty)
  pFile env file

-- | Parse and pretty-print a file
prettyFile :: FilePath -> FilePath -> IO ()
prettyFile root file = do
  index <- _index . snd <$> (loadFile root file)
  cache <- readCache
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

checkFile :: FilePath -> FilePath -> IO (CID,Package)
checkFile root file = do
  (cid,p) <- loadFile root file
  let index = _index p
  cache <- readCache
  forM_ (M.toList $ (_byName index)) (checkRef index cache)
  return (cid,p)

checkRef ::  Index -> Cache -> (Name, CID) -> IO ()
checkRef index cache (name,cid) = do
  def  <- liftIO $ catchErr $ derefMetaDefCID name cid index cache
  defs <- liftIO $ catchErr $ indexToDefs index cache
  let (trm,typ) = defToHoas name def
  case runExcept $ Core.check defs Ctx.empty Once trm typ of
    Left  e -> putStrLn $ T.unpack $ T.concat 
        ["\ESC[31m\STX✗\ESC[m\STX ", name, "\n"
        , printCIDBase32 cid, "\n"
        , T.pack $ show e]
    Right (_,t) -> putStrLn $ T.unpack $ T.concat
        ["\ESC[32m\STX✓\ESC[m\STX ",name, ": ", Core.printHOAS t]

catchErr:: Show e => Except e a -> IO a
catchErr x = do
  case runExcept x of
    Right x -> return x
    Left  e -> putStrLn (show e) >> fail ""

-- | Evaluate a `HOAS` from a file
normDef :: Name -> FilePath -> FilePath -> IO HOAS
normDef name root file = do
  (_,p) <- loadFile root file
  let index = _index p
  cache <- readCache
  cid   <- catchErr (indexLookup name index)
  def   <- catchErr (derefMetaDefCID name cid index cache)
  defs  <- catchErr (indexToDefs index cache)
  return $ Core.norm defs (fst $ defToHoas name def)

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
    Right tt -> Right (Core.hoasToTerm Ctx.empty (fst tt), Core.hoasToTerm Ctx.empty (snd tt))

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
