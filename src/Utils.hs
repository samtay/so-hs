{-# LANGUAGE OverloadedStrings #-}
module Utils
  ( exitOnError
  , module Utils.LowLevel
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Data.Semigroup ((<>))

--------------------------------------------------------------------------------
-- Local imports:
import           Types
import           Utils.LowLevel


exitOnError :: (a -> IO b) -> Either Error a -> IO b
exitOnError rightHandler (Right a) = rightHandler a
exitOnError _ (Left e) =
  case e of
    ConnectionFailure ->
      exitWithError "Connection failure: are you connected to the internet?"
    ScrapingError ->
      exitWithError $
      "Error scraping Google. Try " <> code "so --no-google" <> "."
    JSONError errMsg ->
      exitWithError $ "Error parsing StackOverflow API:\n\n" <> errMsg
    UnknownError errMsg -> exitWithError $ "Unknown error:\n\n" <> errMsg
    _ -> exitWithError "Unknown error"
