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
import           Prompt                 (runLuckyPrompt, runPrompt)
import           StackOverflow          (query)
import           Types
import           Utils                  (code, err)

main :: IO ()
main = withConfig $ \cfg -> do
  initialState <- runCli cfg
  void $ runStateT (runReaderT runApp cfg) initialState

runApp :: App ()
runApp = do
  eQs <- query
  either handleError runInterface eQs
  where
    handleError e  = liftIO $ print e
    runInterface qs = do
      i  <- gets (oUi . sOptions)
      when (i == Brick) $ do
        liftIO . putStrLn $ show i <> " is not yet implemented... here are the raw questions:"
        liftIO $ print qs
      when (i == Prompt) $
        void $ runPrompt qs

withConfig :: (AppConfig -> IO a) -> IO a
withConfig action = getConfigE >>= either exitConfigError action

exitConfigError :: String -> IO a
exitConfigError e = do
  f <- getConfigFile
  hPutStrLn stderr . concat
    $ [ "It looks like there is an error in your configuration. "
      , "If you're having trouble fixing it, you can always run:"
      , "\n\n"
      , code ("    " <> "rm " <> f)
      , "\n\n"
      , "to reset to defaults. "
      , "For reference, the yaml parsing error was:"
      , "\n\n"
      , err e ]
  exitFailure
