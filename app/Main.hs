{-# LANGUAGE NamedFieldPuns #-}
module Main where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Concurrent     (forkIO, newEmptyMVar)
import           Control.Monad          (unless, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Semigroup         ((<>))
import System.Exit (exitSuccess)

--------------------------------------------------------------------------------
-- Library imports:
import           Control.Monad.State    (gets)
import           Lens.Micro

--------------------------------------------------------------------------------
-- Local imports:
import           Cli                    (Cli (..), runCli)
import           Config                 (getConfigE, getConfigFile)
import           Interface.Brick        (runBrick)
import           StackOverflow          (query', queryLucky')
import           Types
import           Utils                  (code, err, exitWithError, whenDef)

main :: IO ()
main = withConfig $ \cfg -> do
  -- Get initial state from CLI
  Cli {options, query} <- runCli cfg
  empty1 <- newEmptyMVar
  empty2 <- newEmptyMVar
  let initialState = AppState { sQuery = query
                              , sOptions = options
                              , sQuestions = empty1
                              , sLuckyAnswer = empty2 }

  -- Start fetching questions asynchronously
  -- TODO consider using async library (or read more Simon Marlow for best approach)
  _ <- forkIO . evalAppT cfg initialState $ query'

  -- Run App
  void . evalAppT cfg initialState $ runApp

runApp :: App ()
runApp = do
  opts <- gets (sOptions)

  -- If @--lucky@, show single answer prompt
  continue <- whenDef False (opts ^. oLuckyL) $ do
    queryLucky'
    promptLucky

  -- Exit if the user got what they needed
  unless continue $ liftIO exitSuccess

  -- Otherwise execute chosen interface
  case opts ^. oUiL of
    Brick  -> runBrick Nothing
    Prompt -> liftIO $ exitWithError "Prompt interface not yet implemented"

promptLucky :: App Bool
promptLucky = undefined

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
