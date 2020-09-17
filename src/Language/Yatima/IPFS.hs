{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Language.Yatima.IPFS where

import           Data.Aeson
import           Data.Proxy
import           Network.HTTP.Client      (defaultManagerSettings, newManager)

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Servant.API
import           Servant.Client
import qualified Servant.Client.Streaming as S
import           Servant.Types.SourceT

import           System.Directory
import           Control.Monad

import           Codec.Serialise
import           Language.Yatima.Defs
import           Language.Yatima.Term

import           Network.IPFS.API

apiCommands :: Proxy ApiV0Commands
apiCommands = Proxy

commands :: Maybe Bool -> ClientM Value
commands = client apiCommands

dagPut :: BSL.ByteString -> Maybe Text -> Maybe Text -> Maybe Bool -> Maybe Text
       -> ClientM Value
dagPut = client (Proxy :: Proxy ApiV0DagPut)

dagPutAnon :: Maybe Bool -> Anon -> ClientM Value
dagPutAnon pin anon =
  dagPut (serialise anon) (Just "cbor") (Just "cbor") pin (Just "blake2b-256")

dagPutBytes :: Maybe Bool -> BSL.ByteString -> ClientM Value
dagPutBytes pin bytes =
  dagPut bytes (Just "cbor") (Just "cbor") pin (Just "blake2b-256")

dagGet :: Text -> S.ClientM (SourceIO BS.ByteString)
dagGet = S.client (Proxy :: Proxy ApiV0DagGet)

blockGet :: Text -> S.ClientM (SourceIO BS.ByteString)
blockGet = S.client (Proxy :: Proxy ApiV0BlockGet)

runDagPut :: Anon -> IO ()
runDagPut anon = do
  manager' <- newManager defaultManagerSettings
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  res <- runClientM (dagPutAnon (Just True) anon) env
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right val -> print val

runDagGet :: Text -> IO ()
runDagGet cid_txt = do
  manager' <- newManager defaultManagerSettings
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  S.withClientM (dagGet cid_txt) env $ \e -> case e of
    Left err -> putStrLn $ "Error: " ++ show err
    Right rs -> foreach fail print rs

runBlockGet :: Text -> IO ()
runBlockGet cid_txt = do
  manager' <- newManager defaultManagerSettings
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  S.withClientM (blockGet cid_txt) env $ \e -> case e of
    Left err -> putStrLn $ "Error: " ++ show err
    Right rs -> foreach fail print rs

runDagPutCache :: FilePath -> IO ()
runDagPutCache root = do
  unless (root == "") (setCurrentDirectory root)
  manager' <- newManager defaultManagerSettings
  cache <- readCache
  let env = mkClientEnv manager' (BaseUrl Http "localhost" 5001 "")
  let client = traverse (dagPutBytes (Just True) . BSL.fromStrict) cache
  res <- runClientM client env
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right val -> print val


