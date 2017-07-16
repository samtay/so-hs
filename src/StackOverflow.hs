{-# LANGUAGE OverloadedStrings #-}
module StackOverflow
  ( query
  , queryG
  , querySE
  ) where

import           Control.Monad.Catch        (try)
import           Data.List                  (elemIndex, intercalate, sortOn)
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             ((<>))

import           Control.Monad.State.Strict (gets, liftIO)
import           Data.Aeson                 (eitherDecode)
import           Data.Aeson.Types           (parseEither)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Lens.Micro                 ((&), (.~), (^.))
import qualified Network.HTTP.Client        as H
import qualified Network.Wreq               as W

import           StackOverflow.Google
import           Types
import           Utils

-- | Query for stack exchange 'Question's based on 'SO' state options
query :: App (Either Error [Question])
query = do
  useG <- gets (oGoogle . soOptions)
  res <- try $ if useG then queryG else querySE
  return $ either (Left . toError) id res

-- | Query stack exchange by first scraping Google for relevant question links
--
-- Maybe in the future either propogate Left error or add to debug log, etc.
-- TODO sort SE api results by initial google results
queryG :: App (Either Error [Question])
queryG = do
  mIds <- google
  case mIds of
    Nothing  -> return . Left $ ScrapingError
    Just []  -> return . Right $ []
    Just ids -> sortByIds ids <$$> seRequest ("questions/" <> mkQString ids) []
  where
    mkQString     = intercalate ";" . map show
    position      = fromMaybe maxBound .*. elemIndex
    sortByIds ids = sortOn (flip position ids . qId)


-- | Query stack exchange via advanced search API
querySE :: App (Either Error [Question])
querySE = do
  q        <- gets soQuery
  seRequest "search/advanced" [ W.param "q"       .~ [q]
                              , W.param "answers" .~ ["1"]
                              , W.param "order"   .~ ["desc"]
                              , W.param "sort"    .~ ["relevance"] ]

-- | Default request options for SE API
-- TODO add api keys and whatnot
appDefaults :: App W.Options
appDefaults = do
  siteParam <- gets (sApiParam . oSite . soOptions)
  return $ W.defaults & W.header "Accept" .~ ["application/json"]
                      & W.param "filter"  .~ [seFilter] -- In the future get this from App
                      & W.param "site"    .~ [siteParam]

-- | Make SE API request
seRequest
  :: String                         -- ^ API resource to append to base URL
  -> [W.Options -> W.Options]       -- ^ Options in addition to 'seDefaults'
  -> App (Either Error [Question]) -- ^ Decoded question data
seRequest resource optMods = do
  baseOpts <- appDefaults
  let opts = foldr (.) id optMods baseOpts
      url  = seApiUrl <> resource
  r <- liftIO $ W.getWith opts url
  let parseResult = eitherDecode (r ^. W.responseBody) >>= parseEither questionsParser
  return
    . either (Left . JSONError . T.pack) Right
    $ parseResult

-- | SE API URL
seApiUrl :: String
seApiUrl = "http://api.stackexchange.com/2.2/"

-- | Filter ID, used to return SE data in particular JSON schema
-- TODO figure out how to handle this, maybe XDGA data with auto refresh
seFilter :: Text
seFilter = "0euqgThy5XMKqGfXzPS_nVSuunbQUZLlX7OuNJSlfvlW4"

-- | Transform to custom error types
toError :: H.HttpException -> Error
toError (H.HttpExceptionRequest _ (H.ConnectionFailure _)) = ConnectionFailure
toError e                                                  = UnknownError . T.pack . show $ e
