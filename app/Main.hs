{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Semigroup       ((<>))
import           System.Exit          (exitFailure)
import           System.IO            (stderr)

import           Control.Monad.Reader (runReaderT)
import           Control.Monad.State  (runStateT)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

import           Cli                  (runCli)
import           Config               (getConfigE, getConfigFile)
import           StackOverflow        (query)
import           Types                (AppConfig)
import           Utils                (code, err)

main :: IO ()
main = withConfig $ \cfg -> do
  initialState <- runCli cfg
  runStateT (runReaderT query cfg) initialState >>= print

withConfig :: (AppConfig -> IO a) -> IO a
withConfig action = getConfigE >>= either exitConfigError action

exitConfigError :: String -> IO a
exitConfigError e = do
  f <- T.pack <$> getConfigFile
  TIO.hPutStrLn stderr . T.concat
    $ [ "It looks like there is an error in your configuration. "
      , "If you're having trouble fixing it, you can always run:"
      , "\n\n"
      , code ("    " <> "rm " <> f)
      , "\n\n"
      , "to reset to defaults. "
      , "For reference, the yaml parsing error was:"
      , "\n\n"
      , T.pack (err e) ]
  exitFailure
