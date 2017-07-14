{-# LANGUAGE OverloadedStrings #-}
module StackOverflow.Google
  ( google
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.String                (fromString)

import           Control.Monad.State.Strict
import           Data.ByteString.Lazy       (ByteString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Lens.Micro
import qualified Network.Wreq               as W
import           Text.HTML.Scalpel.Core
import           Text.Regex.TDFA            (Regex, makeRegex)

import           Types

-- | Scrape google for a list of question IDs
--
-- TODO see if we can determine particular errors, if so use Either instead of Maybe
-- TODO determination between empty results and user agent error
google :: App (Maybe [ByteString])
google = do
  url   <- gets (sUrl . oSite . soOptions)
  num   <- gets (oLimit . soOptions)
  q     <- gets (soQuery)
  htmldoc  <- liftIO $ mkRequest url num q
  return $ parseIds htmldoc

-- | Make a google request
mkRequest :: Text -> Int -> Text -> IO ByteString
mkRequest url limit q = do
  let query = T.concat ["site:", url, " ", q]
      opts  = W.defaults & W.param "num" .~ [T.pack . show $ limit]
                         & W.param "q"   .~ [query]
  r <- W.getWith opts "http://google.com/search"
  return $ r ^. W.responseBody

-- | Parse html bytestring into a list of stack exchange question IDs
parseIds :: ByteString -> Maybe [ByteString]
parseIds = flip scrapeStringLike scraper

-- | As observed by wreq result
scraper :: Scraper ByteString [ByteString]
scraper = attrs urlAttr resultAnchors

-- | Attribute holding URL (as observed by wreq result)
urlAttr :: String
urlAttr = "href"

-- | Select result anchor tags
resultAnchors :: Selector
resultAnchors = "div" @: [hasClass "g"] // "h3" // "a" @: [isQuestionLink]

-- | Ensure attribuet value is a question link, as opposed to a user link
isQuestionLink :: AttributePredicate
isQuestionLink = fromString urlAttr @=~ re ".com\\/questions\\/."

re :: String -> Regex
re = makeRegex
