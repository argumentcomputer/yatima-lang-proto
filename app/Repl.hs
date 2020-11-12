-- |
-- Module      : Repl
-- Description : This module implements the Yatima read-eval-print-loop
-- Copyright   : 2020 Yatima Inc.
-- License     : GPL-3
-- Maintainer  : john@yatima.io
-- Stability   : experimental
--
-- This module modifies work by [Stephen
-- Diehl](https://github.com/sdiehl/repline/blob/master/src/System/Console/Repline.hs)
-- which is licensed under MIT terms included with this package in the
-- @licenses/2016_2020_Stephen_Diehl@ file.
module Repl where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.RWS.Lazy hiding (All)
import Control.Monad.State.Strict
import Data.Char
import Data.IPLD.Cid
import Data.IPLD.DagPackage
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import HaskelineT
import Path
import qualified System.Console.Haskeline as H
import System.Console.Haskeline.Completion
import System.Console.Haskeline.MonadException
import Text.Megaparsec hiding (State)
import Yatima
import qualified Yatima.Core as Core
import qualified Yatima.Core.Ctx as Ctx
import Yatima.Core.Hoas
import Yatima.IPLD
import Yatima.Parse.Package
import Yatima.Parse.Parser
import Yatima.Parse.Term
import Yatima.Term

data ReplState = ReplState
  { _replIndex :: Index,
    _replProject :: Path Abs Dir
  }

emptyReplState root = ReplState emptyIndex root

type Repl = HaskelineT (StateT ReplState IO)

repl ::
  (Functor m, MonadException m) => -- Terminal monad ( often IO ).

  -- | prompt function
  HaskelineT m Text ->
  -- | quit function
  HaskelineT m () ->
  -- | process input function
  (Text -> HaskelineT m ()) ->
  -- | Tab completion function
  CompletionFunc m ->
  -- | Initialiser
  HaskelineT m a ->
  m ()
repl prompt quit process complete initial = runHaskelineT set (initial >> loop)
  where
    loop = do
      promptText <- prompt
      input <- H.handleInterrupt (return (Just "")) $ getInputLine promptText
      case input of
        Nothing -> quit
        Just input
          | input == T.empty -> loop
          | otherwise -> H.handleInterrupt quit $ process input >> loop
    set =
      H.Settings
        { H.complete = complete,
          H.historyFile = Just ".history",
          H.autoAddHistory = True
        }

prompt :: Repl Text
prompt = pure "⅄ "

quit :: Repl ()
quit = outputTxtLn "Goodbye."

data Command
  = Eval Term
  | Defn Name Def
  | Type Term
  | Load (Path Abs File)
  | Import Cid
  | Quit
  | Help
  | Browse
  deriving (Show)

parseLine :: ParserIO Command
parseLine = do
  space
  command <-
    choice
      [ (symbol ":help" <|> symbol ":h") >> return Help,
        --, (symbol ":quit" <|> symbol ":q") >> return Quit
        (symbol ":browse" <|> symbol ":b") >> return Browse,
        (symbol ":type" <|> symbol ":t") >> Type <$> (pExpr False),
        do
          (symbol ":load" <|> symbol ":l")
          nam <- takeWhile1P Nothing (not . isSpace)
          path <- liftIO $ parseFilePath (T.unpack nam)
          return $ Load path,
        --, (symbol ":with" <|> symbol ":w") >> Import <$> pCid
        try pDef >>= (\(n, d) -> return $ Defn n d),
        Eval <$> pExpr False
      ]
  eof
  return command

catchReplErr :: Show e => Except e a -> Repl a
catchReplErr x = case runExcept x of
  Right x -> return x
  Left e -> liftIO (putStrLn (show e)) >> abort

process :: Text -> Repl ()
process line = dontCrash' $ do
  index <- gets _replIndex
  let env = ParseEnv [] (indexEntries index)
  a <- liftIO $ parseIO parseLine env "" line
  procCommand a
  where
    procCommand :: Command -> Repl ()
    procCommand c = case c of
      Browse -> do
        index <- gets _replIndex
        defs <- liftIO $ indexToDefs index
        liftIO $ traverse (prettyIndexF defs) (M.toList $ indexEntries index)
        return ()
      Help -> liftIO $ putStrLn "help text fills you with determination "
      -- Quit   -> abort
      Type t -> do
        index <- gets _replIndex
        defs <- liftIO $ indexToDefs index
        let trm = termToHoas [] t
        (_, typ, _) <- catchReplErr (Core.infer defs Ctx.empty Once trm)
        liftIO $ print $ typ
        return ()
      Eval t -> do
        index <- gets _replIndex
        defs <- liftIO $ indexToDefs index
        let hterm = termToHoas [] t
        --(_,typ_) <- catchReplErr (Core.infer defs Ctx.empty Once hterm)
        --catchReplErr (Core.check defs Ctx.empty Once hterm typ_)
        liftIO $ print $ Core.norm defs hterm
        return ()
      Defn nam def -> do
        index <- gets _replIndex
        defs <- liftIO $ indexToDefs index
        let (trm, typ) = defToHoas nam def
        catchReplErr (Core.check defs Ctx.empty Once trm typ)
        cids <- liftIO $ cachePutDef def
        let i' = Index $ M.insert nam cids (indexEntries index)
        modify (\e -> e {_replIndex = i'})
        return ()
      Load file -> do
        index <- gets _replIndex
        (cid, p) <- liftIO $ checkFile (toFilePath file)
        case mergeIndex index (_index p) of
          Left (n1, c1, c2) -> do
            let err = ConflictingImportNames (_packageTitle p) cid (n1, c1) (n1, c2)
            liftIO $ putStrLn (show err)
            return ()
          Right index -> do
            modify (\e -> e {_replIndex = index})
            return ()

prefixes :: [String] -> String -> Bool
prefixes (p : ps) x = isPrefixOf p x || prefixes ps x
prefixes [] _ = False

complete :: CompletionFunc (StateT ReplState IO)
complete (ante, post)
  | prefixes ks p = noCompletion (ante, post)
  | otherwise = do
    ns <- gets (M.keys . indexEntries . _replIndex)
    let ks' = T.pack <$> ks
    let f word = T.unpack <$> filter (T.isPrefixOf (T.pack word)) (ks' ++ ns)
    completeWord Nothing " " (pure . (map simpleCompletion) . f) (ante, post)
  where
    p = reverse ante
    ks = [":help", ":browse", ":load", ":type"]

shell :: StateT ReplState IO ()
shell = repl prompt quit process complete ini
  where
    ini =
      liftIO $
        putStrLn $
          "Welcome to the Yatima interactive shell!\n"
            ++ "press Ctrl-d to quit"
