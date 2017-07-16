{-# LANGUAGE OverloadedStrings #-}
module StackOverflow.Google
  ( google
  , parseIds
  , mkRequest
  ) where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Maybe                 (catMaybes)
import           Data.String                (fromString)

import           Control.Monad.Catch        (tryJust)
import           Control.Monad.State.Strict (gets, (<=<))
import           Data.ByteString.Lazy       (ByteString)
import           Data.ByteString.Lazy.Char8 (readInt)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Lens.Micro                 ((&), (.~), (^.))
import qualified Network.HTTP.Client        as H
import qualified Network.Wreq               as W
import           Text.HTML.Scalpel.Core
import           Text.Regex.TDFA            (Regex, getAllTextSubmatches,
                                             makeRegex, (=~))

import           Types
import           Utils

-- | Scrape google for a list of question IDs
google :: App (Either Error [Int])
google = do
  url   <- gets (sUrl . oSite . soOptions)
  num   <- gets (oLimit . soOptions)
  q     <- gets (soQuery)
  eHtml <- tryJust toError. liftIO $ mkRequest url num q
  return $ eHtml >>= parseIds

-- | Make a google request
mkRequest :: Text -> Int -> Text -> IO ByteString
mkRequest url limit q = do
  let query = T.concat ["site:", url, " ", q]
      opts  = W.defaults & W.param "num" .~ [T.pack . show $ limit]
                         & W.param "q"   .~ [query]
  r <- W.getWith opts "http://google.com/search"
  return $ r ^. W.responseBody

-- | Parse html bytestring into a list of stack exchange question IDs
parseIds :: ByteString -> Either Error [Int]
parseIds bs = fmap catMaybes $ idFromUrl <$$> mToE (scrapeStringLike bs scraper)
  where
    idFromUrl :: ByteString -> Maybe Int
    idFromUrl = toInt <=< matchQuestionId

    matchQuestionId :: ByteString -> Maybe ByteString
    matchQuestionId = (!? 1) . getAllTextSubmatches . (=~ matchQStr)

    toInt :: ByteString -> Maybe Int
    toInt = fmap fst . readInt

    mToE :: Maybe a -> Either Error a
    mToE (Just x) = Right x
    mToE _        = Left ScrapingError

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
matchQStr = ".com\\/questions\\/([[:digit:]]*)\\/."

-- | Transform to custom error types
toError :: H.HttpException -> Maybe Error
toError (H.HttpExceptionRequest _ (H.StatusCodeException _ _)) = Just ScrapingError
toError _                                                      = Nothing
