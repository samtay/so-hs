{-# LANGUAGE OverloadedStrings #-}
module StackOverflow
  ( queryG
  , querySE
  ) where

import Data.ByteString.Lazy (ByteString)

import StackOverflow.Types

-- | Google query
--
-- TODO sort SE api results by initial google results
queryG :: ByteString    -- ^ Query
       -> ByteString    -- ^ Site
       -> IO [Question] -- ^ Resulting questions & answers
queryG = undefined

-- | Query stack overflow directly
querySE :: ByteString    -- ^ Site
        -> ByteString    -- ^ Query
        -> IO [Question] -- ^ Resulting questions & answers
querySE = undefined


