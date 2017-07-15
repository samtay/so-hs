{-# LANGUAGE OverloadedStrings #-}
module StackOverflow.Google
  ( google
  , parseIds
  , mkRequest
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.String                (fromString)

import           Control.Monad.State.Strict
import           Data.ByteString.Lazy       (ByteString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import           Lens.Micro
import qualified Network.Wreq               as W
import           Text.HTML.Scalpel.Core
import           Text.Regex.TDFA            (Regex, getAllTextSubmatches,
                                             makeRegex, (=~))

import           Types
import           Utils

-- | Scrape google for a list of question IDs
--
-- TODO see if we can determine particular errors, if so use Either instead of Maybe
-- TODO determination between empty results and user agent error
-- TODO finalize return type (ByteString, Text, Lazy, Strict..)
google :: App (Maybe [Text])
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
parseIds :: ByteString -> Maybe [Text]
parseIds bs = idFromUrl <$$> scrapeStringLike bs scraper
  where
    -- Note: we've already filtered for question links, so (!! 1) doesnt fail
    idFromUrl = bsToText . (!! 1) . getAllTextSubmatches . (=~ matchQStr)
    bsToText  = TL.toStrict . TL.decodeUtf8

-- | As observed by wreq result
scraper :: Scraper ByteString [ByteString]
scraper = attrs "href" resultAnchors

-- | Select result anchor tags
resultAnchors :: Selector
resultAnchors = "div" @: [hasClass "g"] // "h3" // "a" @: [isQuestionLink]

-- | Ensure attribuet value is a question link, as opposed to a user link
isQuestionLink :: AttributePredicate
isQuestionLink = fromString "href" @=~ re matchQStr

re :: String -> Regex
re = makeRegex

matchQStr :: String
matchQStr = ".com\\/questions\\/(.*)\\/."
