{-# LANGUAGE OverloadedStrings #-}
module StackOverflow
  ( queryG
  , querySE
  ) where

import Data.Text (Text)

import StackOverflow.Types

-- | Google query
--
-- TODO sort SE api results by initial google results
queryG
  :: Text          -- ^ Query
  -> Text          -- ^ Site
  -> IO [Question] -- ^ Resulting questions & answers
queryG = undefined

-- | Query stack overflow directly
querySE
  :: Text    -- ^ Site
  -> Text    -- ^ Query
  -> IO [Question] -- ^ Resulting questions & answers
querySE = undefined


