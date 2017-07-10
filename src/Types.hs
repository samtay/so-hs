{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Control.Monad.State.Strict
import qualified Data.Text as T
import Data.Text (Text)
import Data.Yaml

import StackOverflow.Types
import Utils

type App = StateT SO IO

data SO = SO
  { soQuery     :: Text
  , soSite      :: Site
  , soQuestions :: [Question]
  , soOptions   :: Options
  } deriving (Show)

data Config = Config
  { cDefaultOpts :: Options      -- ^ Default CLI options
  , cSites       :: [Site]       -- ^ Available SE sites
  , cEditor      :: Maybe Editor -- ^ Custom editor to view answer
  } deriving (Show)

data Options = Options
  { oGoogle :: Bool
  , oLucky  :: Bool
  , oLimit  :: Int
  , oSiteSC :: Text
  , oUi     :: Interface
  } deriving (Show)

data Interface = Brick | Prompt
  deriving (Show, Read)

-- Note emacs requires process substitution, check if possible with shelly/turtle
data Editor = Less | More | Vim | CustomEditor Text
  deriving (Show, Read)

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    cDefaultOpts <- o .:? "defaultOptions" .!= mempty
    cSites'      <- o .:? "sites"          .!= []
    cEditor      <- o .:? "editor"
    let cSites = if null cSites' then [defSite] else site <$> cSites'
    return Config{..}

instance FromJSON Options where
  parseJSON = withObject "options" $ \o -> do
    oGoogle <- o .:? "google" .!= oGoogle defaultOptions
    oLucky  <- o .:? "lucky"  .!= oLucky defaultOptions
    oLimit  <- o .:? "limit"  .!= oLimit defaultOptions
    oSiteSC <- o .:? "site"   .!= oSiteSC defaultOptions
    oUi     <- o .:? "ui"     .!= oUi defaultOptions
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
  , oSiteSC = sApiParam defSite
  , oUi     = Brick }

defSite :: Site
defSite = Site
  { sUrl = "https://stackoverflow.com"
  , sApiParam = "stackoverflow" }

suffixLenses ''Config
suffixLenses ''Options
