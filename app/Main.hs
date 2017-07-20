module Main where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Semigroup         ((<>))

import           Control.Monad.Reader   (runReaderT)
import           Control.Monad.State    (gets, runStateT)

import           Cli                    (runCli)
import           Config                 (getConfigE, getConfigFile)
import           Interface.Brick        (runBrick)
import           Types
import           Utils                  (code, err, exitWithError)

main :: IO ()
main = withConfig $ \cfg -> do
  initialState <- runCli cfg
  evalAppT cfg initialState runApp
  void $ runStateT (runReaderT runApp cfg) initialState

runApp :: App ()
runApp = do
  -- TODO handle lucky case (queries with limit == 1 (withAppT) and hit SPACE for more)
  -- lucky <- gets (oLucky . sOptions)
  i <- gets (oUi . sOptions)
  case i of
    Brick  -> runBrick
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
