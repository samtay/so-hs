-- wreq to query google
-- parse all URLS in SO domain whitelist
module StackOverflow.Google
  ( google
  ) where

import Data.ByteString.Lazy (ByteString)

google :: ByteString  -- ^ Site
       -> ByteString  -- ^ Query
       -> IO [String] -- ^ Resulting question IDs
google = undefined
