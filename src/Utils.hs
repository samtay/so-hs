{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( exitOnError
  , module Utils.LowLevel
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Data.Semigroup ((<>))

--------------------------------------------------------------------------------
-- Library imports:
import qualified Data.Text      as T

--------------------------------------------------------------------------------
-- Local imports:
import           Types
import           Utils.LowLevel


exitOnError :: (a -> IO b) -> Either Error a -> IO b
exitOnError rightHandler (Right a) = rightHandler a
exitOnError _ (Left e) = exitWithError $
  case e of
    NoResultsError ->
      "No results found. Try a different question."
    ConnectionFailure ->
      "Connection failure. Are you connected to the internet?"
    ScrapingError ->
      "Error scraping Google. Try " <> code "so --no-google" <> "."
    JSONError errMsg ->
      "Error parsing StackOverflow API:\n\n" <> errMsg
    UnknownError errMsg ->
      "Unknown error:\n\n" <> errMsg
    _ ->
      "Unknown error:\n\n" <> T.pack (show e)
