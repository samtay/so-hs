{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Text.IO           as TIO
import           Lens.Micro

--------------------------------------------------------------------------------
-- Local imports:
import           Cli                    (Cli (..), runCli)
import           Config                 (getConfigE, getConfigFile)
import           Interface.Brick        (runBrick)
import           Interface.Prompt       (runPrompt)
import           StackOverflow          (query, queryLucky)
import           Types
import           Utils                  (code, err, exitWithError, info,
                                         noBuffer)

main :: IO ()
main = withConfig $ \cfg -> do
  -- Get initial state from CLI
  (Cli opts qry) <- runCli cfg
  let initialState = AppState {sQuery = qry, sOptions = opts}

  -- Run App
  void . evalAppT cfg initialState $ runApp

runApp :: App ()
runApp = do
  -- Start fetching questions asynchronously
  aQuestions <- appAsync query

  opts <- gets (sOptions)

  -- If @--lucky@, show single answer prompt
  when (opts ^. oLuckyL)
    $ queryLucky >>= liftIO . exitOnError runLuckyPrompt

  -- Execute chosen interface
  case opts ^. oUiL of
    Brick  -> runBrick aQuestions
    Prompt -> liftIO $ exitWithError "Prompt interface not yet implemented"

-- | Show single answer, return whether or not to run full interface
runLuckyPrompt :: Question -> IO ()
runLuckyPrompt question = do
  let sortedAnswers = sortOn (negate . aScore) (question ^. qAnswersL)
      mAnswer       = listToMaybe sortedAnswers
  case mAnswer of
    Nothing     -> exitWithError "No answers found. Try a different question."
    Just answer -> do
      TIO.putStrLn (answer ^. aBodyL)
      putStrLn ("\n" <> info "Press [SPACE] to see more results, or any other key to exit.")
      c <- noBuffer getChar
      case c of
        ' ' -> return ()
        _   -> exitSuccess

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

exitOnError :: (a -> IO b) -> Either Error a -> IO b
exitOnError rightHandler (Right a) = rightHandler a
exitOnError _            (Left e)  = case e of
  ConnectionFailure ->
    exitWithError "Connection failure: are you connected to the internet?"
  ScrapingError ->
    exitWithError $ "Error scraping Google. Try " <> code "so --no-google" <> "."
  JSONError errMsg ->
    exitWithError $ "Error parsing StackOverflow API:\n\n" <> T.unpack errMsg
  UnknownError errMsg ->
    exitWithError $ "Unknown error:\n\n" <> T.unpack errMsg
  _ -> exitWithError "Unknown error"
