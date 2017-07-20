module Main where

import           Control.Monad          (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Semigroup         ((<>))
import           System.Exit            (exitFailure)
import           System.IO              (hPutStrLn, stderr)

import           Control.Monad.Reader   (runReaderT)
import           Control.Monad.State    (gets, runStateT)

import           Cli                    (runCli)
import           Config                 (getConfigE, getConfigFile)
import           Interface.Brick        (runBrick)
import           Interface.Prompt       (runLuckyPrompt, runPrompt)
import           StackOverflow          (query)
import           Types
import           Utils                  (code, err, exitWithError)

main :: IO ()
main = withConfig $ \cfg -> do
  initialState <- runCli cfg
  void $ runStateT (runReaderT runApp cfg) initialState

runApp :: App ()
runApp = do
  eQs <- query
  case eQs of
    Left e   -> liftIO $ exitWithError (show e)
    Right [] -> liftIO $ exitWithError "No questions found"
    Right qs -> do
      when (null qs) handleNoResults
      i  <- gets (oUi . sOptions)
      case i of
        Brick  -> runBrick qs
        Prompt -> liftIO $ exitWithError "Prompt interface not yet implemented"

withConfig :: (AppConfig -> IO a) -> IO a
withConfig action = getConfigE >>= either exitConfigError action

exitConfigError :: String -> IO a
exitConfigError e = do
  f <- getConfigFile
  exitWithError . concat
    $ [ "It looks like there is an error in your configuration. "
      , "If you're having trouble fixing it, you can always run:"
      , "\n\n"
      , code ("    " <> "rm " <> f)
      , "\n\n"
      , "to reset to defaults. "
      , "For reference, the yaml parsing error was:"
      , "\n\n"
      , err e ]
