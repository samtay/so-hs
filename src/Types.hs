module Types
  ( module Types
  , module Types.StackOverflow
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Maybe               (fromMaybe)
import           Text.Read                (readMaybe)

--------------------------------------------------------------------------------
-- Library imports:
import           Control.Concurrent.Async (Async, async)
import           Control.Monad.Catch      (Exception, MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader     (MonadReader, ReaderT, ask,
                                           runReaderT)
import           Control.Monad.State      (MonadState, StateT, evalStateT,
                                           execStateT, get, runStateT)
import           Data.Default
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Yaml
import           Lens.Micro               ((^.))
import           Lens.Micro.TH            (makeLenses)

--------------------------------------------------------------------------------
-- Local imports:
import           Types.StackOverflow
import           Utils

--------------------------------------------------------------------------------
-- App
newtype App a = App { unApp :: ReaderT AppConfig (StateT AppState IO) a }
  deriving (Functor, Applicative, Monad,
            MonadReader AppConfig, MonadState AppState,
            MonadIO, MonadThrow, MonadCatch, MonadMask)

runAppWith :: (StateT AppState IO a -> AppState -> b) -> AppConfig -> AppState -> App a -> b
runAppWith runner c s app = runner (runReaderT (unApp app) c) s

runAppT :: AppConfig -> AppState -> App a -> IO (a, AppState)
runAppT = runAppWith runStateT

evalAppT :: AppConfig -> AppState -> App a -> IO a
evalAppT = runAppWith evalStateT

execAppT :: AppConfig -> AppState -> App a -> IO AppState
execAppT = runAppWith execStateT

appAsync :: App a -> App (Async a)
appAsync action = do
  cfg <- ask
  st  <- get
  liftIO . async . evalAppT cfg st $ action

--------------------------------------------------------------------------------
-- Types
data Interface = Brick | Prompt
  deriving (Eq, Show, Read)

-- Note emacs requires process substitution, check if possible with shelly/turtle
data Editor = Less | More | Vim | CustomEditor Text
  deriving (Eq, Show, Read)

data TextDisplay = Raw | HtmlEntities | Pretty
  deriving (Eq, Show, Read)

data AppState = AppState
  { _sOptions :: Options
  , _sQuery   :: Text
  }

data AppConfig = AppConfig
  { _cDefaultOpts :: Options      -- ^ Default CLI options
  , _cSites       :: [Site]       -- ^ Available SE sites
  , _cEditor      :: Maybe Editor -- ^ Custom editor to view answer
  , _cApiKey      :: Maybe Text
  } deriving (Eq, Show)

data Options = Options
  { _oGoogle      :: Bool
  , _oLucky       :: Bool
  , _oLimit       :: Int
  , _oSite        :: Site
  , _oUi          :: Interface
  , _oTextDisplay :: TextDisplay
  } deriving (Eq, Show)

makeLenses ''AppState
makeLenses ''AppConfig
makeLenses ''Options

data Error
  = ConnectionFailure
  | ScrapingError
  | JSONError Text
  | YAMLError Text
  | NoResultsError
  | UnknownError Text
  deriving (Eq, Show)

instance Exception Error

instance FromJSON AppConfig where
  parseJSON = withObject "config" $ \o -> do
    _cDefaultOpts <- o .:? "defaultOptions" .!= def
    _cSites'      <- o .:? "sites"          .!= []
    _cEditor      <- o .:? "editor"
    _cApiKey      <- o .:? "apiKey"
    let _cSites = if null _cSites' then [def] else site <$> _cSites'
    return AppConfig{..}

instance FromJSON Options where
  parseJSON = withObject "options" $ \o -> do
    _oGoogle         <- o .:? "google"      .!= (def ^. oGoogle)
    _oLucky          <- o .:? "lucky"       .!= (def ^. oLucky)
    _oLimit          <- o .:? "limit"       .!= (def ^. oLimit)
    _oSite'          <- o .:? "site"        .!= Site' (def ^. oSite)
    _oUi             <- o .:? "ui"          .!= (def ^. oUi)
    _oTextDisplay    <- o .:? "textDisplay" .!= (def ^. oTextDisplay)
    let _oSite = site _oSite'
    return Options{..}

-- Allow users to have a more intuitive yaml config than the JSON api
newtype Site' = Site' { site :: Site }
instance FromJSON Site' where
  parseJSON = withObject "site" $ \o -> do
    _sUrl      <- o .: "url"
    _sApiParam <- o .: "shortcode"
    return . Site' $ Site {..}

instance FromJSON Editor where
  parseJSON = withText "editor" $ \s -> do
    let c = capitalize . T.unpack $ s
    return $ fromMaybe (CustomEditor s) (readMaybe c)

instance FromJSON Interface where
  parseJSON = withText "interface" $
    \s -> case (T.toLower s) of
      "b"      -> return Brick
      "brick"  -> return Brick
      "p"      -> return Prompt
      "prompt" -> return Prompt
      _        -> fail "invalid interface"

instance FromJSON TextDisplay where
  parseJSON = withText "textDisplay" $
    \s -> case (T.toLower s) of
      "raw"           -> return Raw
      "pretty"        -> return Pretty
      "entities"      -> return HtmlEntities
      "htmlentities"  -> return HtmlEntities
      "html-entities" -> return HtmlEntities
      "html entities" -> return HtmlEntities
      _               -> fail "invalid interface"

instance Default Options where
  def = Options
    { _oGoogle      = True
    , _oLucky       = False
    , _oLimit       = 25
    , _oSite        = def
    , _oUi          = Brick
    , _oTextDisplay = Pretty
    }
