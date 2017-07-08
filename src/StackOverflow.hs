{-# LANGUAGE OverloadedStrings #-}
module StackOverflow
  ( queryG
  , querySE
  , module StackOverflow.Types
  , module StackOverflow.Google
  ) where

import Data.Text (Text)

import StackOverflow.Types
import StackOverflow.Google

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


