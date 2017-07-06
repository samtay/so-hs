-- wreq to query google
-- parse all URLS in SO domain whitelist
module StackOverflow.Google
  ( google
  ) where

import Data.Text (Text)

google
  :: Text  -- ^ Site
  -> Text  -- ^ Query
  -> IO [String] -- ^ Resulting question IDs
google = undefined
