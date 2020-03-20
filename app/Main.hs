{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           System.Exit (exitSuccess)

--------------------------------------------------------------------------------
-- Library imports:
import           Control.Monad.Catch (catch)
import           Control.Monad.State (gets)
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import           Lens.Micro

--------------------------------------------------------------------------------
-- Local imports:
import Cli (Cli (..), runCli)
import Config (getConfig, getConfigFile)
import Interface.Common (gracefully)
import Interface.Prompt (execPrompt, putMdLn)
import Markdown (Markdown)
import StackOverflow (query, queryLucky)
import Types
import Utils (code, err, exitWithError, promptChar)

main :: IO ()
main = do
  catch getConfig exitConfigError
    >>= \cfg -> runCli cfg
    >>= \(Cli opts q) -> void . gracefully . evalAppT cfg (AppState opts q) $ app

app :: App ()
app = do
  (Options _ lucky _ _ _ _) <- gets (_sOptions)
  -- Start fetching questions asynchronously
  aQuestions <- appAsync query
  -- If @--lucky@, show single answer prompt
  when lucky $ queryLucky >>= liftIO . runLuckyPrompt
  -- Only interface available is currently Prompt
  execPrompt aQuestions

-- | Show single answer, returns when user elects to continue from prompt,
-- otherwise exits.
runLuckyPrompt :: Question NonEmpty Markdown -> IO ()
runLuckyPrompt question = do
  let answer = NE.head $ NE.sortWith (negate . _aScore) (question ^. qAnswers)
  putMdLn (answer ^. aBody)
  c <- promptChar "Press [SPACE] to see more results, or any other key to exit."
  case c of
    ' ' -> return ()
    _   -> exitSuccess

exitConfigError :: Yaml.ParseException -> IO a
exitConfigError e = do
  f <- T.pack <$> getConfigFile
  exitWithError . T.concat
    $ [ "It looks like there is an error in your configuration. "
      , "If you're having trouble fixing it, you can always run:"
      , "\n\n"
      , code ("    " <> "rm " <> f)
      , "\n\n"
      , "to reset to defaults. "
      , "For reference, the yaml parsing error was:"
      , "\n\n"
      , err . T.pack $ Yaml.prettyPrintParseException e
      ]
