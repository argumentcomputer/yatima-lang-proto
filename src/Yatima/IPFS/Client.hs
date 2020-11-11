{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Yatima.IPFS.Client
-- Description : This module implements a Haskell Servant Client for the IPFS HTTP API
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
module Yatima.IPFS.Client where

import Codec.Serialise
import Control.Monad.Except
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IPLD.Cid
import Data.IPLD.DagAST
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Network.IPFS.API
import Path
import Servant.API
import Servant.Client hiding (Response)
import qualified Servant.Client.Streaming as S
import Servant.Types.SourceT
import Yatima.IPLD

apiCommands :: Proxy ApiV0Commands
apiCommands = Proxy

commands :: Maybe Bool -> ClientM Aeson.Value
commands = client apiCommands

dagPut ::
  BSL.ByteString ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Text ->
  ClientM Aeson.Value
dagPut = client (Proxy :: Proxy ApiV0DagPut)

dagPutAST :: Maybe Bool -> DagAST -> ClientM Aeson.Value
dagPutAST pin ast =
  dagPut (serialise ast) (Just "cbor") (Just "cbor") pin (Just "blake2b-256")

dagPutBytes :: Maybe Bool -> BSL.ByteString -> ClientM Aeson.Value
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

runLocalDagGetCid :: Cid -> IO BS.ByteString
runLocalDagGetCid cid = do
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

runDagPutFile :: Path Abs File -> IO (Either ClientError Aeson.Value)
runDagPutFile file = do
  manager' <- newManager defaultManagerSettings
  bs <- BS.readFile (toFilePath file)
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  let client = (dagPutBytes (Just True) . BSL.fromStrict) bs
  runClientM client env

runLocalDagPutCid :: Cid -> IO (Either ClientError Aeson.Value)
runLocalDagPutCid cid = do
  manager' <- newManager defaultManagerSettings
  cache <- getYatimaCacheDir
  file <- (\x -> cache </> x) <$> (parseRelFile $ T.unpack $ cidToText cid)
  bytes <- BS.readFile (toFilePath file)
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  let client = (dagPutBytes (Just True) . BSL.fromStrict) bytes
  runClientM client env

runInfuraDagPutCid :: Cid -> IO (Network.HTTP.Client.Response BSL.ByteString)
runInfuraDagPutCid cid = do
  manager' <- newTlsManager
  cache <- getYatimaCacheDir
  file <- (\x -> cache </> x) <$> (parseRelFile $ T.unpack $ cidToText cid)
  bytes <- BS.readFile (toFilePath file)
  initReq <- parseRequest "https://ipfs.infura.io:5001/api/v0/dag/put"
  let queryReq =
        setQueryString
          [ ("format", Just "cbor"),
            ("input-enc", Just "cbor"),
            ("hash", Just "blake2b-256"),
            ("pin", Just "true")
          ]
          initReq
  req <-
    formDataBody
      [ partFileRequestBody
          (cidToText cid)
          (toFilePath file)
          (RequestBodyLBS $ BSL.fromStrict bytes)
      ]
      queryReq
  httpLbs req manager'

swarmConnect :: Text -> ClientM Aeson.Value
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

runEternumPinHash :: Path Abs File -> Cid -> IO (Response BSL.ByteString)
runEternumPinHash token cid = do
  manager' <- newTlsManager
  token <- BS.readFile (toFilePath $ token)
  initReq <- parseRequest "https://www.eternum.io/api/pin/"
  let reqObj = object ["hash" .= (cidToText cid)]
  let req =
        initReq
          { method = "POST",
            requestHeaders =
              [ (hContentType, "application/json"),
                (hAuthorization, "Token " <> token)
              ],
            requestBody = RequestBodyLBS $ Aeson.encode reqObj
          }
  httpLbs req manager'

runEternumListPin ::
  Path Abs File ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Text ->
  IO (Response BSL.ByteString)
runEternumListPin token limit offset ordering = do
  manager' <- newTlsManager
  token <- BS.readFile (toFilePath $ token)
  initReq <- parseRequest "https://www.eternum.io/api/pin/"
  let queryReq =
        setQueryString
          [ ("limit", (BSL.toStrict . Aeson.encode) <$> limit),
            ("offset", (BSL.toStrict . Aeson.encode) <$> offset),
            ("ordering", (BSL.toStrict . Aeson.encode) <$> ordering)
          ]
          initReq
  let req =
        queryReq
          { method = "GET",
            requestHeaders =
              [ (hContentType, "application/json"),
                (hAuthorization, "Token " <> token)
              ]
          }
  httpLbs req manager'

runEternumUpdatePin ::
  Path Abs File ->
  Cid ->
  Maybe Text ->
  Maybe Text ->
  IO (Response BSL.ByteString)
runEternumUpdatePin token cid label tag = do
  manager' <- newTlsManager
  token <- BS.readFile (toFilePath $ token)
  initReq <-
    parseRequest
      ("https://www.eternum.io/api/pin/" <> (T.unpack $ cidToText cid))
  let reqObj =
        object
          [ "name" .= label,
            "tag" .= tag
          ]
  let req =
        initReq
          { method = "PUT",
            requestHeaders =
              [ (hContentType, "application/json"),
                (hAuthorization, "Token " <> token)
              ],
            requestBody = RequestBodyLBS $ Aeson.encode reqObj
          }
  httpLbs req manager'

runEternumRemovePin :: Path Abs File -> Cid -> IO (Response BSL.ByteString)
runEternumRemovePin token cid = do
  manager' <- newTlsManager
  token <- BS.readFile (toFilePath $ token)
  initReq <-
    parseRequest
      ("https://www.eternum.io/api/pin/" <> (T.unpack $ cidToText cid))
  let req =
        initReq
          { method = "DELETE",
            requestHeaders =
              [ (hContentType, "application/json"),
                (hAuthorization, "Token " <> token)
              ]
          }
  httpLbs req manager'

--runInfuraDagGetCid :: Cid -> IO ()
--runInfuraDagGetCid cid = do
--  manager' <- newTlsManager
--  cache    <- getYatimaCacheDir
--  initReq  <- parseRequest "https://ipfs.infura.io:5001/api/v0/dag/get"
--  let req  =  setQueryString [("arg", Just $ T.encodeUtf8 $ cidToText cid)]
--                initReq { method = "POST" }
--  resp <- httpLbs req manager'
--  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus resp)
--  print $ Network.HTTP.Client.responseBody resp
--  return ()
