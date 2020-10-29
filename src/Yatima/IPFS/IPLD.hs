{-
Module      : Yatima.IPFS.IPLD
Description : This module implements the IPLD embedding for Yatima terms and
definitions, as well as related utilities.
Copyright   : 2020 Yatima Inc.
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module Yatima.IPFS.IPLD where

--import           Data.Set                   (Set)
--import qualified Data.Set                   as Set
import           Control.Monad.Identity

import           Data.Text                  (Text)
import qualified Data.Text                  as T hiding (find)

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString            as BS
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IM

import           Debug.Trace

import           Codec.Serialise

import           Control.Monad.Except

import           Control.Monad.State.Strict

import           Path
import           Path.IO

import           Data.IPLD.CID
import           Data.IPLD.DagAST
import           Yatima.Term
import           Yatima.Package

--data IPLDErr
--  = NoDeserial [DeserialiseFailure]
--  | NotInIndex Name
--  | NotInCache CID
--  | CIDMismatch Name CID CID
--  | NameMismatch Name Name
--  deriving Eq

--insertPackage :: Package -> Cache -> (CID,Cache)
--insertPackage p (Cache c) = let pCID = makeCid p in
--  (pCID, Cache $ M.insert pCID (BSL.toStrict $ serialise p) c)
--
--insertSource :: Source -> Cache -> (CID,Cache)
--insertSource p (Cache c) = let pCID = makeCid p in
--  (pCID, Cache $ M.insert pCID (BSL.toStrict $ serialise p) c)
--
--insertDefs :: [(Name,Def)] -> Index -> Cache -> (Index,Cache)
--insertDefs [] index@(Index ns) cache         = (index,cache)
--insertDefs ((n,d):ds) index@(Index ns) cache = runIdentity $ do
--  let (defCid,trmCid,cache') = insertDef n d index cache
--  return $ insertDefs ds (Index $ M.insert n (defCid,trmCid) ns) cache'
--
--indexToDefs :: Monad m => Index -> Cache -> ExceptT IPLDErr m (Map Name Def)
--indexToDefs i@(Index ns) c = M.traverseWithKey (\n _ -> deref n i c) ns
--
--readCache :: Path Abs Dir -> IO Cache
--readCache root = do
--  createDirIfMissing True dir
--  (_,fs) <- listDir dir
--  Cache . M.fromList <$> traverse go fs
--  where
--    dir = cacheDir root
--    go :: Path Abs File -> IO (CID, BS.ByteString)
--    go file = do
--      bs <- BS.readFile (toFilePath file)
--      let name = toFilePath $ filename file
--      case cidFromText (T.pack $ name) of
--        Left e  -> error $ "CORRUPT CACHE ENTRY: " ++ name ++ ", " ++ e
--        Right c -> return (c,bs)
--
--writeCache :: Path Abs Dir -> Cache -> IO ()
--writeCache root (Cache c) = traceM "writeCache" >> void $ M.traverseWithKey go c
--  where
--    dir = cacheDir root
--    go :: CID -> BS.ByteString -> IO ()
--    go c bs = do
--      createDirIfMissing True dir
--      name <- parseRelFile (T.unpack $ cidToText c)
--      let file = (dir </> name)
--
--      exists <- doesFileExist file
--      unless exists ( traceM ("writing file: " ++ show file) >> BS.writeFile (toFilePath file) bs)
--
--cacheDir :: Path Abs Dir -> Path Abs Dir
--cacheDir root = root </> [reldir|.yatima/cache|]
--
--indexLookup :: Monad m => Name -> Index -> ExceptT IPLDErr m (CID,CID)
--indexLookup n i@(Index ns) = maybe (throwError $ NotInIndex n) pure (ns M.!? n)
--
--cacheLookup :: Monad m => CID -> Cache -> ExceptT IPLDErr m BS.ByteString
--cacheLookup c (Cache d) = maybe (throwError $ NotInCache c) pure (d M.!? c)
--
--deserial :: (Serialise a, Monad m) => CID -> Cache -> ExceptT IPLDErr m a
--deserial cid cache = do
--  bs <- cacheLookup cid cache
--  either (throwError . NoDeserial . pure) pure
--    (deserialiseOrFail $ BSL.fromStrict bs)
--
--deref :: Monad m => Name -> Index -> Cache -> ExceptT IPLDErr m Def
--deref name index cache = do
--  (defCid,trmCid)  <- indexLookup name index
--  dagDef          <- deserial @DagDef defCid cache
--  def             <- derefDagDef name index dagDef cache
--  return def
--
--derefDagDef :: Monad m => Name -> Index -> DagDef -> Cache -> ExceptT IPLDErr m Def
--derefDagDef name index dagDef cache = do
--  termAST <- deserial @DagAST (_anonTerm dagDef) cache
--  typeAST <- deserial @DagAST (_anonType dagDef) cache
--  term    <- astToTerm name index termAST (_termMeta dagDef)
--  typ_    <- astToTerm name index typeAST (_typeMeta dagDef)
--  return $ Def (_document dagDef) term typ_
--
--derefDagDefCID :: Monad m => Name -> CID -> Index -> Cache 
--               -> ExceptT IPLDErr m Def
--derefDagDefCID name cid index cache = do
--  (defCid,trmCid)   <- indexLookup name index
--  dagDef            <- deserial @DagDef defCid cache
--  def               <- derefDagDef name index dagDef cache
--  return $ def
--
--insertDef :: Name -> Def -> Index -> Cache -> (CID,CID,Cache)
--insertDef name def@(Def doc term typ_) index c@(Cache cache) = runIdentity $ do
--  let anonTerm = termToAST term
--  let anonType = termToAST typ_
--  let dagDef@(DagDef anonTermCid anonTypeCid _ _ _) = defToDag def
--  let dagDefCid   = makeCid dagDef :: CID
--  let cache'      = M.insert dagDefCid (BSL.toStrict $ serialise dagDef)  $
--                    M.insert anonTypeCid (BSL.toStrict $ serialise anonType) $
--                    M.insert anonTermCid (BSL.toStrict $ serialise anonTerm) $
--                    cache
--  return $ (dagDefCid, anonTermCid, Cache cache')
--
