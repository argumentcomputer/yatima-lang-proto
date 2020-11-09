{-
Module      : Yatima.IPFS.Client
Description : This module implements a Haskell Servant Client for the IPFS HTTP API
Copyright   : 2020 Yatima Inc.
License     : GPL-3
Maintainer  : john@yatima.io
Stability   : experimental
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
module Yatima.IPFS.Client where

import           Control.Monad
import           Control.Monad.Except

import           Data.Aeson

import           Data.Proxy
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.IO             as TIO

import           Network.HTTP.Client hiding (Proxy)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status (statusCode)
import           Network.IPFS.API

import           Servant.API
import           Servant.Client
import qualified Servant.Client.Streaming as S
import           Servant.Types.SourceT

import           Path
import           Path.IO

import           Codec.Serialise

import           Data.IPLD.CID
import           Data.IPLD.DagJSON
import           Data.IPLD.DagAST
import           Yatima.IPLD
import           Yatima.Package
import           Yatima.Parse.Package
import           Yatima.Term

apiCommands :: Proxy ApiV0Commands
apiCommands = Proxy

commands :: Maybe Bool -> ClientM Value
commands = client apiCommands

dagPut :: BSL.ByteString -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text
       -> ClientM Value
dagPut = client (Proxy :: Proxy ApiV0DagPut)

dagPutAST :: Maybe Bool -> DagAST -> ClientM Value
dagPutAST pin ast =
  dagPut (serialise ast) (Just "cbor") (Just "cbor") pin (Just "blake2b-256")

dagPutBytes :: Maybe Bool -> BSL.ByteString -> ClientM Value
dagPutBytes pin bs = dagPut bs (Just "cbor") (Just "cbor") pin (Just "blake2b-256")

dagGet :: Text -> S.ClientM (SourceIO BS.ByteString)
dagGet = S.client (Proxy :: Proxy ApiV0DagGet)

blockGet :: Text -> S.ClientM (SourceIO BS.ByteString)
blockGet = S.client (Proxy :: Proxy ApiV0BlockGet)

runDagPutAST :: DagAST -> IO ()
runDagPutAST ast = do
  manager' <- newManager defaultManagerSettings
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  res <- runClientM (dagPutAST (Just True) ast) env
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right val -> print val

runLocalDagGetCID :: CID -> IO BS.ByteString
runLocalDagGetCID cid = do
  manager' <- newManager defaultManagerSettings
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  S.withClientM (dagGet (cidToText cid)) env go
  where
    go :: Either ClientError (SourceIO BS.ByteString) -> IO BS.ByteString
    go e = case e of
      Left err -> putStrLn ("Error: " ++ show err) >> fail ""
      Right rs -> do
        resp <- runExceptT (runSourceT rs)
        case resp of
          Left err -> putStrLn ("Error: " ++ show err) >> fail ""
          Right bs -> return (BS.concat bs)

--runDagGetPackage :: Text -> IO Package
--runDagGetPackage cid_txt = do
--  bs <- runDagGet cid_txt
--  case eitherDecode' (BSL.fromStrict bs) of
--    Left err -> putStrLn ("JSON Parse Error: " ++ show err) >> fail ""
--    Right v  -> return $ (deserialise (serialise (v :: DagJSON)) :: Package)

runBlockGet :: Text -> IO ()
runBlockGet cid_txt = do
  manager' <- newManager defaultManagerSettings
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  S.withClientM (blockGet cid_txt) env $ \e -> case e of
    Left err -> putStrLn $ "Error: " ++ show err
    Right rs -> foreach fail BS.putStr rs

runDagPutFile :: Path Abs File -> IO (Either ClientError Value)
runDagPutFile file = do
  manager' <- newManager defaultManagerSettings
  bs <- BS.readFile (toFilePath file)
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  let client = (dagPutBytes (Just True) . BSL.fromStrict) bs
  runClientM client env

runLocalDagPutCID :: CID -> IO (Either ClientError Value)
runLocalDagPutCID cid = do
  manager' <- newManager defaultManagerSettings
  cache    <- getYatimaCacheDir
  file     <- (\x -> cache </> x) <$> (parseRelFile $ T.unpack $ cidToText cid)
  bytes    <- BS.readFile (toFilePath file)
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  let client = (dagPutBytes (Just True) . BSL.fromStrict) bytes
  runClientM client env

runInfuraDagPutCID :: CID -> IO (Network.HTTP.Client.Response BSL.ByteString)
runInfuraDagPutCID cid = do
  manager' <- newTlsManager
  cache    <- getYatimaCacheDir
  file     <- (\x -> cache </> x) <$> (parseRelFile $ T.unpack $ cidToText cid)
  bytes    <- BS.readFile (toFilePath file)
  initReq <- parseRequest "https://ipfs.infura.io:5001/api/v0/dag/put"
  let queryReq = setQueryString
        [ ("format", Just "cbor")
        , ("input-enc", Just "cbor")
        , ("hash", Just "blake2b-256")
        , ("pin", Just "true")
        ] initReq
  req <- formDataBody 
    [ partFileRequestBody (cidToText cid) (toFilePath file) 
        (RequestBodyLBS $ BSL.fromStrict bytes)
    ] queryReq
  httpLbs req manager'
  --resp <- httpLbs req manager'
  --putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus resp)
  --print $ Network.HTTP.Client.responseBody resp
  --return ()

--runDagPutCIDs :: [CID] -> IO [(CID, Either ClientError [Value])]
--runDagPutCIDs cids = do
--  manager' <- newManager defaultManagerSettings
--  cache    <- getYatimaCacheDir
--  relfiles <- mapM parseRelFile ((T.unpack . cidToText) <$> cids)
--  let absfiles = (\x -> cache </> x) <$> relfiles
--  bytes    <- mapM (\x -> BS.readFile (toFilePath x)) absfiles
--  let clientF (cid,bytes) 
--  let client = mapM (dagPutBytes (Just True) . BSL.fromStrict) bytes
--  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
--  resps    <- runClientM client env
--  return $ zip cids resps

runDagPutCache :: IO ()
runDagPutCache = do
  manager' <- newManager defaultManagerSettings
  cacheDir    <- getYatimaCacheDir
  (_,files)   <- listDir cacheDir
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  contents <- traverse (BS.readFile . toFilePath) files
  let client = traverse (dagPutBytes (Just True) . BSL.fromStrict) contents
  res <- runClientM client env
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right val -> print val

runDagPutCache' :: IO ()
runDagPutCache' = do
  cacheDir <- getYatimaCacheDir
  (_,ns)   <- listDir cacheDir
  traverse go ns
  return ()
  where
    go :: Path Abs File -> IO ()
    go f = do
      bs <- BS.readFile (toFilePath f)
      case cidFromText (T.pack $ toFilePath f) of
        Left e  -> error $ "CORRUPT CACHE ENTRY: " ++ (toFilePath f) ++ ", " ++ e
        Right c -> do
          putStrLn $ (T.unpack $ cidToText c)
          e <- runDagPutFile f
          case e of
            Left e  -> print e >> return ()
            Right v -> print v >> return ()

swarmConnect :: Text -> ClientM Value
swarmConnect = client (Proxy :: Proxy ApiV0SwarmConnect)

runSwarmConnect :: Text -> IO ()
runSwarmConnect text = do
  manager' <- newManager defaultManagerSettings
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  res <- runClientM (swarmConnect text) env
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right val -> print val

runConnectEternum :: IO ()
runConnectEternum = runSwarmConnect "/dns4/door.eternum.io/tcp/4001/ipfs/QmVBxJ5GekATHi89H8jbXjaU6CosCnteomjNR5xar2aH3q"

runEternumPinHash :: Path Abs File -> CID -> IO ()
runEternumPinHash token cid = do
  manager' <- newTlsManager
  cacheDir <- getYatimaCacheDir
  token  <- BS.readFile (toFilePath $ token)
  initReq <- parseRequest "https://www.eternum.io/api/pin/"
  let reqObj = object ["hash" .= (cidToText cid)]
  let req = initReq
        { method = "POST"
        , requestHeaders = 
          [ (hContentType, "application/json")
          , (hAuthorization, "Token " <> token)
          ]
        , requestBody = RequestBodyLBS $ Data.Aeson.encode reqObj
        }
  resp <- httpLbs req manager'
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus resp)
  print $ Network.HTTP.Client.responseBody resp
  return ()

--runInfuraDagPutCID :: CID -> IO ()
--runInfuraDagPutCID cid = do
--  manager' <- newTlsManager
--  cache    <- getYatimaCacheDir
--  file     <- (\x -> cache </> x) <$> (parseRelFile $ T.unpack $ cidToText cid)
--  bytes    <- BS.readFile (toFilePath file)
--  initReq <- parseRequest "https://ipfs.infura.io:5001/api/v0/dag/put"
--  let queryReq = setQueryString
--        [ ("format", Just "cbor")
--        , ("input-enc", Just "cbor")
--        , ("hash", Just "blake2b-256")
--        , ("pin", Just "true")
--        ] initReq
--  req <- formDataBody 
--    [ partFileRequestBody (cidToText cid) (toFilePath file) 
--        (RequestBodyLBS $ BSL.fromStrict bytes)
--    ] queryReq
--  resp <- httpLbs req manager'
--  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus resp)
--  print $ Network.HTTP.Client.responseBody resp
--  return ()
--
--runInfuraDagGetCID :: CID -> IO ()
--runInfuraDagGetCID cid = do
--  manager' <- newTlsManager
--  cache    <- getYatimaCacheDir
--  initReq  <- parseRequest "https://ipfs.infura.io:5001/api/v0/dag/get"
--  let req  =  setQueryString [("arg", Just $ T.encodeUtf8 $ cidToText cid)]
--                initReq { method = "POST" }
--  resp <- httpLbs req manager'
--  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus resp)
--  print $ Network.HTTP.Client.responseBody resp
--  return ()


----runPinataPinFile :: FilePath -> FilePath -> IO ()
----runPinataPinFile cacheDir file = do
----  manager' <- newTlsManager
----  cache    <- readCache cacheDir
----  pub  <- BS.readFile (cacheDir </> ".yatima/pinata_pub_key")
----  key  <- BS.readFile (cacheDir </> ".yatima/pinata_secret_key")
----  initReq <- parseRequest "https://api.pinata.cloud/pinning/pinByHash"
----  let opts   = object ["hostNodes" .= ["/dns4/door.eternum.io/tcp/4001/ipfs/QmVBxJ5GekATHi89H8jbXjaU6CosCnteomjNR5xar2aH3q" :: Text]]
----  let reqObj = object ["hashToPin" .= ((T.pack file) :: Text)
----                      ,"pinataOptions" .= opts
----                      ]
----  let req = initReq
----        { method = "POST"
----        , requestHeaders = 
----          [ (hContentType, "application/json")
----          , ("pinata_api_key",pub)
----          , ("pinata_secret_api_key",key)
----          ]
----        , requestBody = RequestBodyLBS $ Data.Aeson.encode reqObj
----        }
----  resp <- httpLbs req manager'
----  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus resp)
----  print $ Network.HTTP.Client.responseBody resp
----  return ()
--
--
----runEternumPinCache :: Path Abs Dir -> IO ()
----runEternumPinCache root = do
----  (_,ns) <- listDir (cacheDir root)
----  traverse go ns
----  return ()
----  where
----    go :: Path Abs File -> IO ()
----    go f = do
----      bs <- BS.readFile (toFilePath f)
----      case cidFromText (T.pack f) of
----        Left e  -> error $ "CORRUPT CACHE ENTRY: " ++ (toFilePath f) ++ ", " ++ e
----        Right c -> do
----          putStrLn $ (T.unpack $ cidToText c)
----          runEternumPinFile root f
----          return ()
--
