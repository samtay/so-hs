{-# LANGUAGE OverloadedStrings #-}
module StackOverflow
  ( query
  , queryLucky
  , queryG
  , querySE
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Monad        (join)
import           Data.List            (elemIndex, intercalate, sortOn)
import           Data.Maybe           (fromMaybe)
import           Data.Semigroup       ((<>))

--------------------------------------------------------------------------------
-- Library imports:
import           Control.Monad.Catch  (tryJust)
import           Control.Monad.Reader (asks)
import           Control.Monad.State  (get, gets, liftIO, put)
import           Data.Aeson           (eitherDecode)
import           Data.Aeson.Types     (parseEither)
import           Data.Bifunctor       (first)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Lens.Micro           ((&), (.~), (^.))
import qualified Network.HTTP.Client  as H
import qualified Network.Wreq         as W

--------------------------------------------------------------------------------
-- Local imports:
import           StackOverflow.Google
import           Types
import           Utils

-- | Get question results
query :: App (Either Error [Question])
query = do
  useG <- gets (_oGoogle . _sOptions)
  fmap join . tryJust toError $ if useG then queryG else querySE

-- | Get a single question result (hopefully decent performance boost)
queryLucky :: App (Either Error Question)
queryLucky = do
  initialState <- get
  let modifiedState = initialState & sOptions . oLimit .~ 1
  put modifiedState
  result <- query
  put initialState
  return $ case result of
    Right []    -> Left NoResultsError
    Right (q:_) -> Right q
    Left e      -> Left e

-- | Query stack exchange by first scraping Google for relevant question links
--
-- Maybe in the future either propogate Left error or add to debug log, etc.
queryG :: App (Either Error [Question])
queryG = do
  mIds <- google
  case mIds of
    Left e    -> return . Left $ e
    Right []  -> return . Right $ []
    Right ids -> sortByIds ids <$$> seRequest ("questions/" <> mkQString ids) []
  where
    mkQString     = intercalate ";" . map show
    position      = fromMaybe maxBound .*. elemIndex
    sortByIds ids = sortOn (flip position ids . _qId)


-- | Query stack exchange via advanced search API
querySE :: App (Either Error [Question])
querySE = do
  q <- gets _sQuery
  seRequest "search/advanced" [ W.param "q"       .~ [q]
                              , W.param "answers" .~ ["1"]
                              , W.param "order"   .~ ["desc"]
                              , W.param "sort"    .~ ["relevance"] ]

-- | Default request options for SE API
-- TODO add api keys and whatnot
appDefaults :: App W.Options
appDefaults = do
  key <- fromMaybe seKey <$> asks _cApiKey
  siteParam <- gets (_sApiParam . _oSite . _sOptions)
  return $ W.defaults & W.header "Accept" .~ ["application/json"]
                      & W.param "filter"  .~ [seFilter] -- In the future get this from App
                      & W.param "site"    .~ [siteParam]
                      & W.param "key"     .~ [key]

-- | Make SE API request
-- | TODO catch non-200 or allow non-200 and return Left error text
seRequest
  :: String                        -- ^ API resource to append to base URL
  -> [W.Options -> W.Options]      -- ^ Options in addition to 'seDefaults'
  -> App (Either Error [Question]) -- ^ Decoded question data
seRequest resource optMods = do
  baseOpts <- appDefaults
  let opts = foldr (.) id optMods baseOpts
      url  = seApiUrl <> resource
  r <- liftIO $ W.getWith opts url
  return
    . first (JSONError . T.pack)
    $ eitherDecode (r ^. W.responseBody)
        >>= parseEither questionsParser

-- | SE API URL
seApiUrl :: String
seApiUrl = "http://api.stackexchange.com/2.2/"

-- | Filter ID, used to return SE data in particular JSON schema
-- TODO figure out how to handle this, maybe XDGA data with auto refresh
seFilter :: Text
seFilter = "0euqgThy5XMKqGfXzPS_nVSuunbQUZLlX7OuNJSlfvlW4"

-- | API key (allows higher quota)
seKey :: Text
seKey = "8o9g7WcfwnwbB*Qp4VsGsw(("

-- | Transform to custom error types
toError :: H.HttpException -> Maybe Error
toError (H.HttpExceptionRequest _ (H.ConnectionFailure _)) = Just ConnectionFailure
toError e = Just . UnknownError . T.pack . show $ e
