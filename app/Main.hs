{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception
import Control.Monad.State.Strict
import Data.IPLD.Cid
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import Path
import Path.IO
import Repl hiding (Command (..))
import Yatima
import Yatima.IPFS.Client (runResolve)
import Yatima.IPLD
import Yatima.Parse.Package
import Prelude hiding (FilePath)

data Command
  = Check Text
  | Run Text Text
  | Show Text
  | Init
  | Repl
  | Put Text IPFSNode
  | Get Text
  | Clone Text Text
  deriving (Show)

data IPFSNode = LocalDaemon | Infura | Eternum deriving (Show)

main :: IO ()
main = do
  opts <- execParser pCommand
  run opts

pCommand :: ParserInfo Command
pCommand =
  info
    (helper <*> versionOption <*> programOptions)
    ( fullDesc <> progDesc "the Yatima command line interface"
        <> header "The Yatima Programming Language, version 0.0.1"
    )
  where
    versionOption :: Parser (a -> a)
    versionOption =
      infoOption
        "0.0.1"
        (long "version" <> short 'v' <> help "Show version")
    programOptions :: Parser Command
    programOptions =
      hsubparser $
        mconcat
          [ command "check" (info pCheck (progDesc "Typecheck a package")),
            command "run" (info pRun (progDesc "Run a program")),
            command "init" (info pInit (progDesc "Initialize a project")),
            command "repl" (info pRepl (progDesc "Start the REPL")),
            command "put" (info pPut (progDesc "Put a package into the IPLD graph")),
            command "get" (info pGet (progDesc "Get a package from the IPLD graph")),
            command "show" (info pShow (progDesc "Show a Yatima IPLD object")),
            command "clone" (info pClone (progDesc "Clone a package's source files"))
          ]

cacheCompleter :: Completer
cacheCompleter = listIOCompleter getCacheCids

getCacheCids :: IO [String]
getCacheCids = do
  cacheDir <- getYatimaCacheDir
  (_, files) <- listDir cacheDir
  return $ fromRelFile . filename <$> files

pCheck :: Parser Command
pCheck = Check <$> argument str argPackage

pClone :: Parser Command
pClone = Clone <$> argument str (metavar "EMPTY_DIR") <*> argument str argCid

argPackage :: Mod ArgumentFields Text
argPackage = (metavar "PACKAGE" <> completer cacheCompleter <> action "file")

argCid :: Mod ArgumentFields Text
argCid = (metavar "Cid" <> completer cacheCompleter)

pRun :: Parser Command
pRun =
  Run <$> argument str argPackage
    <*> (argument str (metavar "DEFINITION") <|> pure "main")

pInit :: Parser Command
pInit = pure Init

pRepl :: Parser Command
pRepl = pure Repl

nodeFlag :: Parser IPFSNode
nodeFlag =
  flag' Infura (long "infura" <> help "pin to the infura.io IPFS server")
    <|> flag' Eternum (long "eternum" <> help "pin to the eternum.io IPFS server")
    <|> flag' LocalDaemon (long "local" <> help "pin to the local IPFS daemon")
    <|> pure LocalDaemon

pPut :: Parser Command
pPut = Put <$> argument str argPackage <*> nodeFlag

pGet :: Parser Command
pGet = Get <$> argument str argCid

pShow :: Parser Command
pShow = Show <$> argument str argCid

data ArgID
  = ArgPath (Path Abs File)
  | ArgCid Cid

readArgID :: Text -> IO ArgID
readArgID txt = do
  path <- try @PathException (parseFilePath (T.unpack txt))
  let cid = cidFromText txt
  res <- runResolve txt
  case (cid, res, path) of
    (Right c, _, _) -> return $ ArgCid c
    (_, Right c, _) -> return $ ArgCid c
    (_, _, Right path) -> return $ ArgPath path
    (Left e1, Left e2, Left e3) ->
      fail $
        concat
          [ "Can't understand ",
            T.unpack txt,
            "\n - Failed to read as Cid: ",
            e1,
            "\n - Failed to resolve as IPNS: ",
            T.unpack e2,
            "\n - Failed to read as filepath. ",
            show e3
          ]

readArgIDToCid :: Text -> IO Cid
readArgIDToCid txt = do
  let loadFile' x = (\(_, _,_, c, _) -> c) <$> loadFile (toFilePath x)
  argID <- readArgID txt
  case argID of
    ArgPath p -> loadFile' p
    ArgCid c -> return c

run :: Command -> IO ()
run c = case c of
  Check pack -> void $ readArgIDToCid pack >>= checkCid
  Run pack nam -> readArgIDToCid pack >>= normCid nam >>= print
  Clone dir txt -> do
    cid <- readArgIDToCid txt
    dir <- parseDirPath (T.unpack dir)
    exists <- doesDirExist dir
    when exists (putStrLn $ concat ["Can't clone, directory ", toFilePath dir, " already exists"])
    ensureDir dir
    initYatimaProject dir
    putStrLn $ concat ["Cloning Yatima project to ", toFilePath dir]
    clonePackage dir cid
  Init -> do
    dir <- getCurrentDir
    exists <- doesDirExist (dir </> [reldir|.yatima|])
    if exists
      then return ()
      else do
        initYatimaProject dir
        putStrLn $ concat ["Initialized Yatima project at ", toFilePath dir]
  Put txt node -> do
    cid <- readArgIDToCid txt
    case node of
      LocalDaemon -> localPutPackage cid
      Infura -> infuraPutPackage cid
      Eternum -> eternumPutPackage cid
  Get txt -> void $ localGet (cidFromText' txt)
  Show txt -> void $ showCidJSON (cidFromText' txt)
  Repl -> do
    dir <- getCurrentDir
    projectDir <- maybe (parent dir) id <$> (findYatimaProjectDir (parent dir))
    initYatimaProject projectDir
    evalStateT shell (emptyReplState projectDir)
