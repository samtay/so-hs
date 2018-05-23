{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Monad          (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.List              (sortOn)
import           Data.Maybe             (listToMaybe)
import           Data.Semigroup         ((<>))
import           System.Exit            (exitSuccess)

--------------------------------------------------------------------------------
-- Library imports:
import           Control.Monad.State    (gets)
import qualified Data.Text              as T
import           Lens.Micro

--------------------------------------------------------------------------------
-- Local imports:
import           Cli                    (Cli (..), runCli)
import           Config                 (getConfigE, getConfigFile)
import           Interface.Brick        (execBrick)
import           Interface.Prompt       (execPrompt, putMdLn)
import           Markdown               (Markdown)
import           StackOverflow          (query, queryLucky)
import           Types
import           Utils                  (code, err, exitOnError, exitWithError,
                                         promptChar)

main :: IO ()
main = getConfigE >>= \case
  Left e -> exitConfigError e
  Right cfg -> do
    Cli opts q <- runCli cfg
    void . evalAppT cfg (AppState opts q) $ app

app :: App ()
app = do
  (Options _ lucky _ _ ui _) <- gets (_sOptions)
  -- Start fetching questions asynchronously
  aQuestions <- appAsync query
  -- If @--lucky@, show single answer prompt
  when lucky $ queryLucky >>= liftIO . exitOnError runLuckyPrompt
  -- Execute chosen interface
  case ui of
    Brick  -> execBrick aQuestions
    Prompt -> execPrompt aQuestions

-- | Show single answer, returns when user elects to continue from prompt,
-- otherwise exits.
runLuckyPrompt :: Question [] Markdown -> IO ()
runLuckyPrompt question = do
  let sortedAnswers = sortOn (negate . _aScore) (question ^. qAnswers)
      mAnswer       = listToMaybe sortedAnswers
  case mAnswer of
    Nothing     -> exitWithError "No answers found. Try a different question."
    Just answer -> do
      putMdLn (answer ^. aBody)
      c <- promptChar "Press [SPACE] to see more results, or any other key to exit."
      case c of
        ' ' -> return ()
        _   -> exitSuccess

exitConfigError :: String -> IO a
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
      , err (T.pack e) ]
