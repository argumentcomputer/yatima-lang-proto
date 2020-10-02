module Language.Yatima where

import Language.Yatima.Import
import Language.Yatima.IPFS
import Language.Yatima.IPLD
import Language.Yatima.Uses

import qualified Language.Yatima.Ctx as Ctx
import           Language.Yatima.Ctx (Ctx, (<|))

import qualified Language.Yatima.Core as Core
import Language.Yatima.Core (HOAS(..), HashF(..), Hash(..), PreContext, CheckErr, defToHOAS)

import Language.Yatima.Print (prettyTerm)
import qualified Language.Yatima.Print as Print

import Language.Yatima.Parse (parseTerm, unsafeParseTerm)
import qualified Language.Yatima.Parse as Parse

import qualified Language.Yatima.Term as Term
import Language.Yatima.Term (Term(..), Name, Def(..))

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
  defs  <- catchIPLDErr $ indexToDefs index cache
  forM_ defs (go index)
  return ()
  where
    go :: Index -> Def -> IO ()
    go index def = do
      putStrLn ""
      putStrLn $ T.unpack $ printCIDBase32 $ index M.! (_name def)
      putStrLn $ T.unpack $ Print.prettyDef def
      return ()

-- | simple MurmurHash implementation
--mix64 :: Word64 -> Word64
--mix64 h =
--  let h1     = xor h (shiftR h 33)
--      h2     = h1 * 0xff51afd7ed558ccd
--      h3     = xor h2 (shiftR h2 33)
--      h4     = h3 * 0xc4ceb9fe1a85ec53
--   in xor h4 (shiftR h4 33)

hash :: Index -> Cache -> HashF
hash index cache = HashF $ \t ->
  Hash $ cidToBytes $ makeCID (treeFromHOAS Ctx.empty index cache t)

treeFromHOAS :: PreContext -> Index -> Cache -> HOAS -> Tree
treeFromHOAS ctx index cache t = case t of
  VarH nam idx             -> Vari idx
  LamH nam bod             -> Ctor "Lam" 1 [bind nam bod]
  AppH fun arg             -> Ctor "App" 2 [go fun, go arg]
  RefH nam                 -> case (runExcept $ makeLink nam index cache) of
    Left e     -> error $ concat
      ["BAD HASHF: UndefinedReference:", (T.unpack nam), "\n"
      ,"IPLD Error: ", show e
      ]
    Right (_,c) -> Link c
  LetH nam use typ exp bod ->
    Ctor "Let" 4 [uses use, go typ, go exp, bind nam bod]
  AllH slf nam use typ bod -> Ctor "All" 3 [uses use, go typ, bind2 slf nam bod]
  AnyH                     -> Ctor "Any" 0 []
  FixH nam bod             -> bind nam bod
  AnnH _   trm _           -> go trm
  where
    uses         = usesToTree
    dep          = Ctx.depth ctx
    go t         = treeFromHOAS ctx index cache t
    bind n b     = Bind $ treeFromHOAS ((n,AnyH)<|ctx) index cache (b (VarH n dep))
    bind2 s n b  = Bind $ Bind $ treeFromHOAS
      ((n,AnyH)<|(s,AnyH)<|ctx) index cache (b (VarH s dep) (VarH n (dep+1)))

checkRef :: Name -> CID -> Index -> Cache -> Except (CheckErr IPLDErr) HOAS
checkRef name cid index cache = do
  let mapE = mapExcept (either (\e -> throwError $ Core.CheckEnvironmentError name e) pure)
  def  <- mapE $ derefMetaDefCID name cid index cache
  defs <- mapE $ indexToDefs index cache
  let (trm,typ) = defToHOAS name def
  Core.check (hash index cache) defs Ctx.empty Once trm typ
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
            ["\ESC[32m\STX✓\ESC[m\STX ",name, ": ", Core.printHOAS t]
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
  return $ Core.norm (fst $ defToHOAS name def) defs

--whnf :: Term -> Term
--whnf = Core.hoasToTerm Ctx.empty . Core.whnf . Core.termToHoas Ctx.empty
--
--norm :: Term -> Term
--norm = Core.hoasToTerm [] . Core.norm . Core.termToHoas []

--infer :: Term -> Either (CheckErr e) Term
--infer term =
--  let hTerm = Core.termToHoas [] term in
--  case Core.infer [] Once hTerm of
--    Left err -> Left err
--    Right (_,ty) -> Right (Core.hoasToTerm [] ty)
--
--check :: Defs -> Term -> Term -> Either (CheckErr e) Term
--check term tipo =
--  let hTerm = Core.termToHoas [] term in
--  let hType = Core.termToHoas [] tipo in
--  case runExcept (Core.check [] Once hTerm hType) of
--    Left err     -> Left err
--    Right (_,ty) -> Right (Core.hoasToTerm [] ty)
--
--synth :: Term -> Term -> Either (CheckErr e) (Term, Term)
--synth term typ_ =
--  let hTerm = Core.termToHoas [] term in
--  let hType = Core.termToHoas [] typ_ in
--  case runExcept (Core.synth hashF hTerm hType) of
--    Left err -> Left err
--    Right tt -> Right (Core.hoasToTerm Ctx.empty (fst tt), Core.hoasToTerm Ctx.empty (snd tt))
--    
--prettyInfer :: Term -> Text
--prettyInfer term = case infer term of
--  Left err -> HOAS.prettyError err
--  Right ty -> Print.prettyTerm ty
--
--prettyCheck :: Term -> Term -> Text
--prettyCheck term tipo = case check term tipo of
--  Left err -> HOAS.prettyError err
--  Right ty -> Print.prettyTerm ty
--
--prettySynth :: Term -> Term -> Text
--prettySynth term tipo = case synth term tipo of
--  Left err -> HOAS.prettyError err
--  Right (ty,tr) -> Text.concat [Print.prettyTerm tr, " :: ", Print.prettyTerm ty]
--
--testSynth :: Text -> Text -> IO ()
--testSynth termCode typeCode = do
--  let term = unsafeParseTerm termCode
--  let tipo = unsafeParseTerm typeCode
--  putStrLn ("input-term: " ++ Text.unpack (prettyTerm term))
--  putStrLn ("input-type: " ++ Text.unpack (prettyTerm tipo))
--  case synth term tipo of
--    Left err -> print (Text.unpack (HOAS.prettyError err))
--    Right (sTerm, sTipo) -> do
--      putStrLn ("synth-term: " ++ Text.unpack (prettyTerm sTerm))
--      putStrLn ("synth-type: " ++ Text.unpack (prettyTerm sTipo))
-- >>>>>>> e7e75e5dde6c8e5d0cad6a8398932c897d17a388
