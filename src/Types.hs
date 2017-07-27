{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Types
  ( module Types
  , module Types.StackOverflow
  ) where

import           Control.Concurrent   (MVar)
import           Data.Maybe           (fromMaybe)
import           Text.Read            (readMaybe)

import           Control.Monad.Reader (ReaderT, runReaderT)
import           Control.Monad.State  (StateT, evalStateT, execStateT,
                                       runStateT)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Yaml

import           Types.StackOverflow
import           Utils

-- TODO newtype this
type App = ReaderT AppConfig (StateT AppState IO)

runAppWith :: (StateT AppState IO a -> AppState -> b) -> AppConfig -> AppState -> App a -> b
runAppWith runner c s app = runner (runReaderT app c) s

runAppT :: AppConfig -> AppState -> App a -> IO (a, AppState)
runAppT = runAppWith runStateT

evalAppT :: AppConfig -> AppState -> App a -> IO a
evalAppT = runAppWith evalStateT

execAppT :: AppConfig -> AppState -> App a -> IO AppState
execAppT = runAppWith execStateT


data AppState = AppState
  { sQuery       :: Text
  , sOptions     :: Options
  , sQuestions   :: MVar (Either Error [Question])
  , sLuckyAnswer :: MVar (Either Error Answer)
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
