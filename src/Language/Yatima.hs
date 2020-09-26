module Language.Yatima where

import Language.Yatima.HOAS
import Language.Yatima.Parse hiding (_index)
import Language.Yatima.IPFS
import Language.Yatima.IPLD

--checkRef :: Name -> CID -> Index -> Cache -> Except CheckErr HOAS
--checkRef name cid index cache = do
--  let ctx = Seq.empty
--  let mapE = mapExcept (either (\e -> throwError $ DerefError ctx name cid e) pure)
--  def  <- mapE $ derefMetaDefCID name cid index cache
--  (trm,typ) <- mapE $ defToHOAS name def index cache
--  check ctx Once trm typ index cache
--  return typ
--
--checkFile :: FilePath -> FilePath -> IO ()
--checkFile root file = do
--  (_,p) <- pFile root file
--  let index = _packInd p
--  cache <- readCache
--  let func :: (Name, CID) -> IO ()
--      func (name, cid) = do
--        case runExcept $ checkRef name cid index cache of
--          Left  e -> putStrLn $ T.unpack $ T.concat 
--            ["\ESC[31m\STX✗\ESC[m\STX ", name, "\n", T.pack $ show e]
--          Right t -> putStrLn $ T.unpack $ T.concat
--            ["\ESC[32m\STX✓\ESC[m\STX ",name, ": ", printHOAS t]
--  forM_ (M.toList $ index) func
--
--catchDerefErr :: Except DerefErr a -> IO a
--catchDerefErr x = do
--  case runExcept x of
--    Right x -> return x
--    Left  e -> error $ "Runtime DerefErr: " ++ show e
--
---- | Read and evaluate a `HOAS` from a file
--readDef :: Name -> FilePath -> IO HOAS
--readDef name file = do
--  index <- _packInd . snd <$> pFile "" file
--  cache <- readCache
--  cid   <- catchDerefErr (indexLookup name index)
--  def   <- catchDerefErr (derefHOAS name cid index cache)
--  return $ def
--
--normDef :: Name -> FilePath -> IO HOAS
--normDef name file = do
--  index <- _packInd . snd <$> pFile "" file
--  cache <- readCache
--  cid   <- catchDerefErr (indexLookup name index)
--  def   <- catchDerefErr (derefHOAS name cid index cache)
--  return $ norm def index cache


--
