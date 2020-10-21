{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
module Yatima.IPFS where

import           Control.Monad
import           Control.Monad.Except

import           Data.Aeson

import           Data.Proxy
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO

import           Network.HTTP.Client hiding (Proxy)
import           Network.HTTP.Client.TLS
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

import           Yatima.CID
import           Yatima.IPLD
import           Yatima.DagJSON
import           Yatima.DagAST
import           Yatima.Package
import           Yatima.Import
import           Yatima.Term


apiCommands :: Proxy ApiV0Commands
apiCommands = Proxy

commands :: Maybe Bool -> ClientM Value
commands = client apiCommands

dagPut :: BSL.ByteString -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text
       -> ClientM Value
dagPut = client (Proxy :: Proxy ApiV0DagPut)

dagPutAST :: Maybe Bool -> AnonAST -> ClientM Value
dagPutAST pin ast =
  dagPut (serialise ast) (Just "cbor") (Just "cbor") pin (Just "blake2b-256")

dagPutBytes :: Maybe Bool -> BSL.ByteString -> ClientM Value
dagPutBytes pin bs = dagPut bs (Just "cbor") (Just "cbor") pin (Just "blake2b-256")

dagGet :: Text -> S.ClientM (SourceIO BS.ByteString)
dagGet = S.client (Proxy :: Proxy ApiV0DagGet)

blockGet :: Text -> S.ClientM (SourceIO BS.ByteString)
blockGet = S.client (Proxy :: Proxy ApiV0BlockGet)

runDagPutAST :: AnonAST -> IO ()
runDagPutAST ast = do
  manager' <- newManager defaultManagerSettings
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  res <- runClientM (dagPutAST (Just True) ast) env
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right val -> print val

runDagGet :: Text -> IO BS.ByteString
runDagGet cid_txt = do
  manager' <- newManager defaultManagerSettings
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  S.withClientM (dagGet cid_txt) env go
  where
    go :: Either ClientError (SourceIO BS.ByteString) -> IO BS.ByteString
    go e = case e of
      Left err -> putStrLn ("Error: " ++ show err) >> fail ""
      Right rs -> do
        resp <- runExceptT (runSourceT rs)
        case resp of
          Left err -> putStrLn ("Error: " ++ show err) >> fail ""
          Right bs -> return (BS.concat bs)

runDagGetPackage :: Text -> IO Package
runDagGetPackage cid_txt = do
  bs <- runDagGet cid_txt
  case eitherDecode' (BSL.fromStrict bs) of
    Left err -> putStrLn ("JSON Parse Error: " ++ show err) >> fail ""
    Right v  -> return $ (deserialise (serialise (v :: DagJSON)) :: Package)

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

runDagPutCache :: Path Abs Dir -> IO ()
runDagPutCache root = do
  manager' <- newManager defaultManagerSettings
  Cache cache    <- readCache root
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  let client = traverse (dagPutBytes (Just True) . BSL.fromStrict) cache
  res <- runClientM client env
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right val -> print val

runDagPutCache' :: Path Abs Dir -> IO ()
runDagPutCache' root = do
  (_,ns) <- listDir (cacheDir root)
  traverse go ns
  return ()
  where
    go :: Path Abs File -> IO ()
    go f = do
      bs <- BS.readFile (toFilePath f)
      case cidFromText (T.pack $ toFilePath f) of
        Left e  -> error $ "CORRUPT CACHE ENTRY: " ++ (toFilePath f) ++ ", " ++ e
        Right c -> do
          putStrLn $ (T.unpack $ printCIDBase32 c)
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

runEternumPinHash :: Path Abs Dir -> Text -> IO ()
runEternumPinHash root hash = do
  manager' <- newTlsManager
  cache  <- readCache (cacheDir root)
  token  <- BS.readFile (toFilePath $ root </> [reldir|.yatima/eternum_token|])
  initReq <- parseRequest "https://www.eternum.io/api/pin/"
  let reqObj = object ["hash" .= hash]
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

--runPinataPinFile :: FilePath -> FilePath -> IO ()
--runPinataPinFile cacheDir file = do
--  manager' <- newTlsManager
--  cache    <- readCache cacheDir
--  pub  <- BS.readFile (cacheDir </> ".yatima/pinata_pub_key")
--  key  <- BS.readFile (cacheDir </> ".yatima/pinata_secret_key")
--  initReq <- parseRequest "https://api.pinata.cloud/pinning/pinByHash"
--  let opts   = object ["hostNodes" .= ["/dns4/door.eternum.io/tcp/4001/ipfs/QmVBxJ5GekATHi89H8jbXjaU6CosCnteomjNR5xar2aH3q" :: Text]]
--  let reqObj = object ["hashToPin" .= ((T.pack file) :: Text)
--                      ,"pinataOptions" .= opts
--                      ]
--  let req = initReq
--        { method = "POST"
--        , requestHeaders = 
--          [ (hContentType, "application/json")
--          , ("pinata_api_key",pub)
--          , ("pinata_secret_api_key",key)
--          ]
--        , requestBody = RequestBodyLBS $ Data.Aeson.encode reqObj
--        }
--  resp <- httpLbs req manager'
--  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus resp)
--  print $ Network.HTTP.Client.responseBody resp
--  return ()

--runInfuraPinFile :: FilePath -> FilePath -> IO ()
--runInfuraPinFile root file = do
--  unless (root == "") (setCurrentDirectory root)
--  manager' <- newTlsManager
--  cache  <- readCache
--  initReq <- parseRequest "https://ipfs.infura.io:5001/api/v0/dag/put?format=cbor&input-enc=cbor&hash=blake2b-256&pin=true"
--  let reqObj = object ["hashToPin" .= ((T.pack file) :: Text)
--                      ]
--  let req = initReq
--        { method = "POST"
--        , requestHeaders = 
--          [ (hContentType, "application/json")
--          , ("pinata_api_key",pub)
--          , ("pinata_secret_api_key",key)
--          ]
--        , requestBody = RequestBodyLBS $ Data.Aeson.encode reqObj
--        }
--  resp <- httpLbs req manager'
--  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus resp)
--  print $ Network.HTTP.Client.responseBody resp
--  return ()

--runEternumPinCache :: Path Abs Dir -> IO ()
--runEternumPinCache root = do
--  (_,ns) <- listDir (cacheDir root)
--  traverse go ns
--  return ()
--  where
--    go :: Path Abs File -> IO ()
--    go f = do
--      bs <- BS.readFile (toFilePath f)
--      case cidFromText (T.pack f) of
--        Left e  -> error $ "CORRUPT CACHE ENTRY: " ++ (toFilePath f) ++ ", " ++ e
--        Right c -> do
--          putStrLn $ (T.unpack $ printCIDBase32 c)
--          runEternumPinFile root f
--          return ()

