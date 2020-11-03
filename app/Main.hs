{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude hiding (FilePath)
import Options.Applicative
import qualified System.Console.ANSI as ANSI

import           Control.Monad.State.Strict

import           Data.List                               (isPrefixOf)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Data.IPLD.CID
import           Path
import           Path.IO

import           Repl hiding (Command(..))

import           Yatima
import           Yatima.Parse.Package

data Command
  = Check Text
  | Run Text Text
  | Init
  | Repl
  deriving Show

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
      , command "init"  (info pInit (progDesc "Initialize a Yatima project"))
      , command "repl"  (info pRepl (progDesc "Start the Yatima REPL"))
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

pRun :: Parser Command
pRun = Run <$> argument str argPackage
           <*> (argument str (metavar "DEFINITION") <|> pure "main")

pInit :: Parser Command
pInit = pure Init

pRepl :: Parser Command
pRepl = pure Repl

run :: Command -> IO ()
run c = case c of
  Check pack -> case cidFromText pack of
    Left  e -> checkFile (T.unpack pack) >> return ()
    Right c -> checkCID c >> return ()
  Run pack nam -> case cidFromText pack of
    Left  e -> normFile nam (T.unpack pack) >>= print >> return ()
    Right c -> normCID  nam c >>= print >> return ()
  Init -> do
    dir <- getCurrentDir
    exists <- doesDirExist (dir </> [reldir|.yatima|])
    if exists then return () 
    else do
      initYatimaProject dir
      putStrLn $ concat ["Initialized Yatima project at ", toFilePath dir]
      return ()
  Repl -> do
    dir <- getCurrentDir
    projectDir <- maybe (parent dir) id <$> (findYatimaProjectDir (parent dir))
    initYatimaProject projectDir
    evalStateT shell (emptyReplState projectDir)
