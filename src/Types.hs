{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
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
import           Control.Monad.Catch      (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader     (MonadReader, ReaderT, ask,
                                           runReaderT)
import           Control.Monad.State      (MonadState, StateT, evalStateT,
                                           execStateT, get, runStateT)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Yaml

--------------------------------------------------------------------------------
-- Local imports:
import           Types.StackOverflow
import           Utils

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


data AppState = AppState
  { sQuery   :: Text
  , sOptions :: Options
  }

data AppConfig = AppConfig
  { cDefaultOpts :: Options      -- ^ Default CLI options
  , cSites       :: [Site]       -- ^ Available SE sites
  , cEditor      :: Maybe Editor -- ^ Custom editor to view answer
  , cApiKey      :: Maybe Text
  } deriving (Eq, Show)

data Options = Options
  { oGoogle :: Bool
  , oLucky  :: Bool
  , oLimit  :: Int
  , oSite   :: Site
  , oUi     :: Interface
  } deriving (Eq, Show)

data Interface = Brick | Prompt
  deriving (Eq, Show, Read)

-- Note emacs requires process substitution, check if possible with shelly/turtle
data Editor = Less | More | Vim | CustomEditor Text
  deriving (Eq, Show, Read)

data Error
  = ConnectionFailure
  | ScrapingError
  | JSONError Text
  | YAMLError Text
  | NoResultsError
  | UnknownError Text
  deriving (Eq, Show)

instance FromJSON AppConfig where
  parseJSON = withObject "config" $ \o -> do
    cDefaultOpts <- o .:? "defaultOptions" .!= mempty
    cSites'      <- o .:? "sites"          .!= []
    cEditor      <- o .:? "editor"
    cApiKey      <- o .:? "apiKey"
    let cSites = if null cSites' then [defSite] else site <$> cSites'
    return AppConfig{..}

instance FromJSON Options where
  parseJSON = withObject "options" $ \o -> do
    oGoogle <- o .:? "google" .!= oGoogle defaultOptions
    oLucky  <- o .:? "lucky"  .!= oLucky defaultOptions
    oLimit  <- o .:? "limit"  .!= oLimit defaultOptions
    oSite'  <- o .:? "site"   .!= Site' (oSite defaultOptions)
    oUi     <- o .:? "ui"     .!= oUi defaultOptions
    let oSite = site oSite'
    return Options{..}

-- Allow users to have a more intuitive yaml config than the JSON api
newtype Site' = Site' { site :: Site }
instance FromJSON Site' where
  parseJSON = withObject "site" $ \o -> do
    sUrl      <- o .: "url"
    sApiParam <- o .: "shortcode"
    return . Site' $ Site {..}

instance Monoid Options where
  mempty         = defaultOptions
  _ `mappend` o2 = o2

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

defaultOptions :: Options
defaultOptions = Options
  { oGoogle = True
  , oLucky  = False
  , oLimit  = 25
  , oSite   = defSite
  , oUi     = Brick }

defSite :: Site
defSite = Site
  { sUrl = "https://stackoverflow.com"
  , sApiParam = "stackoverflow" }

suffixLenses ''AppState
suffixLenses ''AppConfig
suffixLenses ''Options
