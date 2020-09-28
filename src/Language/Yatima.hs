module Language.Yatima where

import Language.Yatima.Term
import Language.Yatima.Parse
import Language.Yatima.Print
import Language.Yatima.Import
import Language.Yatima.IPFS
import Language.Yatima.IPLD
import Language.Yatima.HOAS

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
import           Data.Sequence (Seq(..), ViewL(..), ViewR(..), viewr, viewl, (|>), (<|))
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
  (_,pack)  <- loadFile root file
  cache <- readCache
  case prettyDefs (_index pack) cache of
    Left e  -> putStrLn $ show e
    Right t -> putStrLn $ T.unpack t

-- | simple MurmurHash implementation
--mix64 :: Word64 -> Word64
--mix64 h =
--  let h1     = xor h (shiftR h 33)
--      h2     = h1 * 0xff51afd7ed558ccd
--      h3     = xor h2 (shiftR h2 33)
--      h4     = h3 * 0xc4ceb9fe1a85ec53
--   in xor h4 (shiftR h4 33)

hash :: Index -> HashF
hash index = HashF $ \t ->
  Hash $ cidToBytes $ makeCID (treeFromHOAS Seq.empty index t)

treeFromHOAS :: Ctx -> Index -> HOAS -> Tree
treeFromHOAS ctx index t = case t of
  VarH nam idx             -> Vari idx
  LamH nam bod             -> Ctor "Lam" 1 [bind nam bod]
  AppH fun arg             -> Ctor "App" 2 [go fun, go arg]
  RefH nam                 -> case index M.!? nam of
    Nothing -> error $ "Undefined Reference " ++ (T.unpack nam)
    Just c  -> Link c
  LetH nam use typ exp bod ->
    Ctor "Let" 4 [uses use, go typ, go exp, bind nam bod]
  AllH slf nam use typ bod -> Ctor "All" 3 [uses use, go typ, bind2 slf nam bod]
  TypH                     -> Ctor "Typ" 0 []
  FixH nam bod             -> bind nam bod
  AnnH _   trm _           -> go trm
  where
    uses         = usesToTree
    dep          = Seq.length ctx
    go t         = treeFromHOAS ctx index t
    f n          = (Many,n,TypH)
    bind n b     = Bind $ treeFromHOAS (f n:<|ctx) index (b (VarH n dep))
    bind2 s n b  = Bind $ Bind $
      treeFromHOAS (f n:<|f s:<|ctx) index (b (VarH s dep) (VarH n (dep+1)))

checkRef :: Name -> CID -> Index -> Cache -> Except (CheckErr IPLDErr) HOAS
checkRef name cid index cache = do
  let ctx = Seq.empty
  let mapE = mapExcept (either (\e -> throwError $ CheckEnvironmentError ctx name e) pure)
  def  <- mapE $ derefMetaDefCID name cid index cache
  defs <- mapE $ indexToDefs index cache
  let (trm,typ) = defToHOAS name def
  check (hash index) ctx Once trm typ defs
  return typ

checkFile :: FilePath -> FilePath -> IO ()
checkFile root file = do
  (_,p) <- loadFile root file
  let index = _index p
  cache <- readCache
  let func :: (Name, CID) -> IO ()
      func (name, cid) = do
        case runExcept $ checkRef name cid index cache of
          Left  e -> putStrLn $ T.unpack $ T.concat 
            ["\ESC[31m\STX✗\ESC[m\STX ", name, "\n"
            , printCIDBase32 cid, "\n"
            , T.pack $ show e]
          Right t -> putStrLn $ T.unpack $ T.concat
            ["\ESC[32m\STX✓\ESC[m\STX ",name, ": ", printHOAS t]
  forM_ (M.toList $ index) func

catchIPLDErr :: Except IPLDErr a -> IO a
catchIPLDErr x = do
  case runExcept x of
    Right x -> return x
    Left  e -> error $ "Runtime DerefErr: " ++ show e

-- | Evaluate a `HOAS` from a file
normDef :: Name -> FilePath -> FilePath -> IO HOAS
normDef name root file = do
  (_,p) <- loadFile root file
  let index = _index p
  cache <- readCache
  cid   <- catchIPLDErr (indexLookup name index)
  def   <- catchIPLDErr (derefMetaDefCID name cid index cache)
  defs  <- catchIPLDErr (indexToDefs index cache)
  return $ norm (hash index) (fst $ defToHOAS name def) defs
