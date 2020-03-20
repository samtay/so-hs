module Interface.Common
  ( gracefully
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import Control.Exception (Handler (..), catches)

--------------------------------------------------------------------------------
-- Local imports:
import Types
import Utils

--------------------------------------------------------------------------------
-- Types

gracefully :: IO a -> IO a
gracefully = flip catches
  [ Handler $ \case
      NoResultsError ->
        exitWithError $ "No results found. Try a different question."
      ConnectionFailure ->
        exitWithError $ "Connection failure. Are you connected to the internet?"
      ScrapingError ->
        exitWithError $ "Error scraping Google. Try " <> code "so --no-google" <> "."
      JSONError errMsg ->
        exitWithError $ "Error parsing StackOverflow API:\n\n" <> errMsg
      YAMLError errMsg ->
        exitWithError $ "Error parsing config file:\n\n" <> errMsg
      UnknownError errMsg ->
        exitWithError $ "Unknown error:\n\n" <> errMsg
  ]
