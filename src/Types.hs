{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Types
  ( module Types
  , module Types.StackOverflow
  ) where

import           Data.Maybe                 (fromMaybe)
import           Text.Read                  (readMaybe)

import           Control.Monad.State.Strict
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Yaml

import           Types.StackOverflow
import           Utils

type App = StateT SO IO

data SO = SO
  { soQuery     :: Text
  , soQuestions :: [Question]
  , soOptions   :: Options
  } deriving (Eq, Show)

data Config = Config
  { cDefaultOpts :: Options      -- ^ Default CLI options
  , cSites       :: [Site]       -- ^ Available SE sites
  , cEditor      :: Maybe Editor -- ^ Custom editor to view answer
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

suffixLenses ''Config
suffixLenses ''Options
