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

import qualified Language.Yatima.Ctx as Ctx
import           Language.Yatima.Term
import qualified Language.Yatima.Core as Core
import           Language.Yatima.Parse
import           Language.Yatima.Print
import           Language.Yatima.Import
import           Language.Yatima.IPLD
import           Language.Yatima


data YideState = YideState
  { _yDefs :: Index
  , _yRoot :: FilePath
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

prompt :: Repl Text
prompt = pure "yide> "

quit :: Repl ()
quit = outputTxtLn "Goodbye."

data Command
  = Eval Term
  | Defn Def
  | Load FilePath
  | Import CID
  | Quit
  | Help
  | Browse
  deriving Show

parseLine :: ParserIO Command
parseLine = do
  space
  command <- choice
    [ (symbol ":help" <|> symbol ":h") >> return Help
    , (symbol ":quit" <|> symbol ":q") >> return Quit
    , (symbol ":browse") >> return Browse
    --, do
    --    (symbol ":load" <|> symbol ":l")
    --    nam <- pPackageName
    --    return $ Load $ (T.unpack nam) ++ ".ya"
    , Defn <$> (try $ pDef)
    , Eval <$> pExpr False
    ]
  eof
  return command

catchReplErr:: Show e => Except e a -> Repl a
catchReplErr x = case runExcept x of
    Right x -> return x
    Left  e -> liftIO (putStrLn (show e)) >> abort

process :: Text -> Repl ()
process line = do
  index <- gets _yDefs
  let env = ParseEnv Set.empty (M.keysSet index)
  a <- liftIO $ parseIO parseLine env "" line
  procCommand a
  where
    procCommand :: Command -> Repl ()
    procCommand c = case c of
      Browse -> do
        index <- gets _yDefs
        liftIO $ print index
      Help   -> liftIO $ putStrLn "help text fills you with determination "
      Quit   -> abort
      Eval t -> dontCrash' $ do
        index    <- gets _yDefs
        cache    <- tryAction $ liftIO $ readCache
        t        <- catchReplErr (validateTerm t [] index cache)
        defs     <- catchReplErr (indexToDefs index cache)
        let hterm = Core.termToHoas Ctx.empty t
        --(_,typ_) <- catchReplErr (Core.infer defs Ctx.empty Once hterm)
        --catchReplErr (Core.check defs Ctx.empty Once hterm typ_)
        liftIO $ print $ Core.norm defs hterm
        return ()
      --Defn d -> do
      --  index <- gets _yideDefs
      --  cache <- liftIO $ readCache
      --  (_,c')   <- liftIO $ catchDerefErr (insertDef d index cache)
      --  liftIO $ writeCache c'
      --  return ()
      --Load file -> do


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
