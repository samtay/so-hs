module StackOverflow
  ( query
  , queryLucky
  , queryG
  , querySE
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Data.List (elemIndex, intercalate)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import           Data.Semigroup ((<>))

--------------------------------------------------------------------------------
-- Library imports:
import           Control.Monad.Catch (MonadCatch, bracket_, handle, throwM)
import           Control.Monad.Reader (asks)
import           Control.Monad.State (get, gets, liftIO, modify, put)
import           Data.Aeson (eitherDecode)
import           Data.Aeson.Types (parseEither)
import           Data.Text (Text)
import qualified Data.Text as T
import           Lens.Micro ((&), (.~), (^.))
import qualified Network.HTTP.Client as H
import qualified Network.Wreq as W

--------------------------------------------------------------------------------
-- Local imports:
import           Markdown
import qualified StackOverflow.Google.Deprecated as Deprecated
import           Types
import           Utils

-- | Get question results
query :: App (NonEmpty (Question NonEmpty Markdown))
query = do
  useG <- gets (_oGoogle . _sOptions)
  display <- gets (_oTextDisplay . _sOptions)
  qs <- httpToError $ if useG then queryG else querySE
  return $ markdown display <$$> qs

-- | Get a single question result (hopefully decent performance boost)
queryLucky :: App (Question NonEmpty Markdown)
queryLucky = do
  initialState <- get
  NE.head <$> bracket_
    (modify $ sOptions . oLimit .~ 1)
    (put initialState)
    query

-- | Query stack exchange by first scraping Google for relevant question links
--
-- Maybe in the future either propogate Left error or add to debug log, etc.
queryG :: App (NonEmpty (Question NonEmpty Text))
queryG = do
  mIds <- Deprecated.google
  case mIds of
    Left e    -> throwM e
    Right []  -> throwM NoResultsError
    Right ids -> sortByIds ids <$> seRequest ("questions/" <> mkQString ids) []
  where
    mkQString     = intercalate ";" . map show
    position      = fromMaybe maxBound .*. elemIndex
    sortByIds ids = NE.sortWith (flip position ids . _qId)


-- | Query stack exchange via advanced search API
querySE :: App (NonEmpty (Question NonEmpty Text))
querySE = do
  q   <- gets _sQuery
  lim <- gets $ _oLimit . _sOptions
  seRequest "search/advanced" [ W.param "q"        .~ [q]
                              , W.param "pagesize" .~ [tshow lim]
                              , W.param "page"     .~ ["1"]
                              , W.param "answers"  .~ ["1"]
                              , W.param "order"    .~ ["desc"]
                              , W.param "sort"     .~ ["relevance"]
                              ]

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
  :: String                   -- ^ API resource to append to base URL
  -> [W.Options -> W.Options] -- ^ Options in addition to 'seDefaults'
  -> App (NonEmpty (Question NonEmpty Text))   -- ^ Decoded question data
seRequest resource optMods = do
  baseOpts <- appDefaults
  let opts = foldr (.) id optMods baseOpts
      url  = seApiUrl <> resource
  r <- liftIO $ W.getWith opts url
  let decoded = eitherDecode (r ^. W.responseBody) >>= parseEither questionsParser
  case decoded of
    Left e   -> throwM $ JSONError $ T.pack e
    Right [] -> throwM NoResultsError
    Right (q:qs) -> pure $ q :| qs

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
httpToError :: MonadCatch m => m a -> m a
httpToError = handle $ \case
  (H.HttpExceptionRequest _ (H.ConnectionFailure _)) -> throwM ConnectionFailure
  e -> throwM e
