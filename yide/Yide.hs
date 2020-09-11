module Yide where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.RWS.Lazy                  hiding (All)
import           Control.Monad.State.Strict
import           Control.Monad.Trans
import           Control.Monad.Except

import           Text.Megaparsec                         hiding (State)

import           Data.List                               (isPrefixOf)
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as M
import           Data.Maybe                              (isJust)
import           Data.Set                                (Set)
import qualified Data.Set                                as Set
import           Data.Text                               (Text)
import qualified Data.Text                               as T

import           System.Process                          (callCommand)

import qualified System.Console.Haskeline                as H
import           System.Console.Haskeline.Completion
import           System.Console.Haskeline.MonadException

import           HaskelineT

import           Language.Yatima.Defs
import           Language.Yatima.HOAS
import           Language.Yatima.Parse
import           Language.Yatima.Print
import           Language.Yatima.Term


data YideState = YideState
  { _yideDefs :: Defs
  }


type Repl = HaskelineT (StateT YideState IO)

repl :: (Functor m, MonadException m)      -- Terminal monad ( often IO ).
         => HaskelineT m Text              -- ^ prompt function
         -> HaskelineT m ()                -- ^ quit function
         -> (Text -> HaskelineT m ())      -- ^ process input function
         -> CompletionFunc m               -- ^ Tab completion function
         -> HaskelineT m a                 -- ^ Initialiser
         -> m ()
repl prompt quit process complete initial = runHaskelineT set (initial >> loop)
  where
    loop = do
      promptText <- prompt
      input <- H.handleInterrupt (return (Just "")) $ getInputLine promptText
      case input of
        Nothing    -> quit
        Just input
          | input == T.empty -> loop
          | otherwise -> H.handleInterrupt quit $ process input >> loop
    set = H.Settings
      { H.complete       = complete
      , H.historyFile    = Just ".history"
      , H.autoAddHistory = True
      }

zhangFeiStyleName = "翼德"

prompt :: Repl Text
prompt = pure "yide> "

quit :: Repl ()
quit = outputTxtLn "Goodbye."

data Command
  = Eval Term
  | Defn Def
  -- | Load FilePath
  | Quit
  | Help
  | Browse
  deriving Show

parseLine :: Parser Command
parseLine = do
  space
  command <- choice
    [ (symbol ":help" <|> symbol ":h") >> return Help
    , (symbol ":quit" <|> symbol ":q") >> return Quit
    , (symbol ":browse") >> return Browse
    , Defn <$> (try $ pDef)
    , Eval <$> pExpr False
    ]
  eof
  return command

process :: Text -> Repl ()
process line = do
  defs <- gets _yideDefs
  case parse' parseLine (ParseEnv Set.empty defs) "" line of
    Left  e -> liftIO $ putStr (errorBundlePretty e)
    Right x -> procCommand x
  where
    procCommand :: Command -> Repl ()
    procCommand c = case c of
      Browse -> do
        ds <- gets _yideDefs
        liftIO $ print ds
      Help   -> liftIO $ putStrLn "help text fills you with determination "
      Quit   -> abort
      Eval t -> do
        ds <- gets _yideDefs
        t  <- liftIO $ catchDerefErr (toLOAS t [""] ds)
        liftIO $ putStrLn $ T.unpack $ printHOAS $ norm (toHOAS t []) ds
        return ()
      Defn d -> do
        ds <- gets _yideDefs
        ds' <- liftIO $ catchDerefErr (insertDef d ds)
        put (YideState ds')
        return ()


prefixes :: [String] -> String -> Bool
prefixes (p:ps) x = isPrefixOf p x || prefixes ps x
prefixes [] x = False

complete :: CompletionFunc (StateT YideState IO)
complete (ante, post)
  | prefixes [":q ", ":quit ", ":h ", ":help "] p = noCompletion (ante, post)
  | otherwise = do
     --ns <- gets (M.keys . Core._names . _fideModule)
     let ks = [":quit", ":help", ":browse"]
     let f word = T.unpack <$> filter (T.isPrefixOf (T.pack word)) (ks)
     completeWord Nothing " " (pure . (map simpleCompletion) . f)  (ante, post)
  where
    p = reverse ante

yide :: StateT YideState IO ()
yide = repl prompt quit process complete ini
  where
    ini = liftIO $ putStrLn
      "Welcome to yide, the Yatima interactive development environment!"
