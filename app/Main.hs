{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Codec.Serialise
import           Control.Monad.State.Strict
import           Data.IPLD.CID
import           Data.IPLD.DagJSON
import           Data.IPLD.DagPackage
import           Data.List                  (isPrefixOf)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Path
import           Path.IO
import           Prelude                    hiding (FilePath)

import           Options.Applicative

import           Repl                       hiding (Command (..))

import           Yatima
import           Yatima.IPLD
import           Yatima.Parse.Package

data Command
  = Check Text
  | Run Text Text
  | Show Text
  | Init
  | Repl
  | Put Text IPFSNode
  | Get Text IPFSNode
  -- | Clone Text
  deriving Show

data IPFSNode = LocalDaemon | Infura deriving Show

main :: IO ()
main = do
  opts <- execParser pCommand
  run opts

pCommand :: ParserInfo Command
pCommand = 
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc "the Yatima command line interface" <>
      header "The Yatima Programming Language, version 0.0.1")
  where
    versionOption :: Parser (a -> a)
    versionOption = infoOption "0.0.1"
      (long "version" <> short 'v' <> help "Show version")
    programOptions :: Parser Command
    programOptions = hsubparser $ mconcat
      [ command "check" (info pCheck (progDesc "Typecheck a package")) 
      , command "run"   (info pRun (progDesc "Run a program"))
      , command "init"  (info pInit (progDesc "Initialize a project"))
      , command "repl"  (info pRepl (progDesc "Start the REPL"))
      , command "put"   (info pPut (progDesc "Put a package into the IPLD graph"))
      , command "get"   (info pGet (progDesc "Get a package from the IPLD graph"))
      , command "show"  (info pShow (progDesc "Show a Yatima IPLD object"))
      -- , command "remote" (info pRemote (progDesc "Pull a package from IPFS"))
      -- , command "clone" (info pClone (progDesc "Clone a package's source files"))
      ]

cacheCompleter :: Completer
cacheCompleter = listIOCompleter getCacheCids

getCacheCids :: IO [String]
getCacheCids = do
  cacheDir  <- getYatimaCacheDir
  (_,files) <- listDir cacheDir
  return $ fromRelFile . filename <$> files

pCheck :: Parser Command
pCheck = Check <$> argument str argPackage

argPackage :: Mod ArgumentFields Text
argPackage = (metavar "PACKAGE" <> completer cacheCompleter <> action "file")

argCID :: Mod ArgumentFields Text
argCID = (metavar "CID" <> completer cacheCompleter)

pRun :: Parser Command
pRun = Run <$> argument str argPackage
           <*> (argument str (metavar "DEFINITION") <|> pure "main")

pInit :: Parser Command
pInit = pure Init

pRepl :: Parser Command
pRepl = pure Repl

nodeFlag :: Parser IPFSNode
nodeFlag = flag LocalDaemon Infura 
  (long "infura" <> help "sets the IPFS node as the infura.io server")

pPut :: Parser Command
pPut = Put <$> argument str argPackage <*> nodeFlag

pGet :: Parser Command
pGet = Get <$> argument str argCID <*> nodeFlag

pShow :: Parser Command
pShow = Show <$> argument str argCID

readArgPackageID :: Text -> IO (Either CID (Path Abs File))
readArgPackageID txt = do
  dir <- getCurrentDir
  nam <- parseRelFile (T.unpack txt)
  let path = dir </> nam
  exists <- doesFileExist path
  if exists
  then return $ Right path
  else case cidFromText txt of
    Right c -> return $ Left c
    Left  e -> fail $ concat
      [ "Can't find package ", T.unpack txt
      , "\n Failed to read as filepath: ", (toFilePath path), "does not exist"
      , "\n Failed to read as CID: ", e
      ]

run :: Command -> IO ()
run c = case c of
  Check pack -> void $ do
    argPackageID <- readArgPackageID pack
    case argPackageID of
      Left  cid  -> checkCID cid 
      Right path -> checkFile (toFilePath path)
  Run pack nam -> do
    argPackageID <- readArgPackageID pack
    case argPackageID of
      Left  cid  -> normCID  nam cid >>= print
      Right path -> normFile nam (toFilePath path) >>= print
  Init -> do
    dir <- getCurrentDir
    exists <- doesDirExist (dir </> [reldir|.yatima|])
    if exists then return ()
    else do
      initYatimaProject dir
      putStrLn $ concat ["Initialized Yatima project at ", toFilePath dir]
  Put txt _ -> do
    let loadFile' x = (\(_,c,_) -> c) <$> loadFile (toFilePath x)
    cid <- either pure loadFile' =<< readArgPackageID txt
    localPutPackageDeps cid
  Get txt _ -> void $ localGetPackageDeps (cidFromText' txt)
  Show txt -> void $ showCIDJSON (cidFromText' txt)
  Repl -> do
    dir <- getCurrentDir
    projectDir <- maybe (parent dir) id <$> (findYatimaProjectDir (parent dir))
    initYatimaProject projectDir
    evalStateT shell (emptyReplState projectDir)

