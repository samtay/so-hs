{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Config where

import Data.Char (toUpper, toLower)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Yaml
import Text.RawString.QQ

import StackOverflow
import Utils

data Config = Config
  { cDefaultOpts :: Options      -- ^ Default CLI options
  , cSites       :: [Site]       -- ^ Available SE sites
  , cEditor      :: Maybe Editor -- ^ Custom editor to view answer
  } deriving (Show)

data Options = Options
  { oGoogle :: Bool
  , oLucky :: Bool
  , oLimit :: Int
  , oSite :: Site
  , oUi :: Interface
  } deriving (Show)

data Interface = Brick | Prompt
  deriving (Show, Read)

-- Note emacs requires process substitution, check if possible with shelly/turtle
data Editor = Less | More | Vim | CustomEditor Text
  deriving (Show, Read)

suffixLenses ''Config
suffixLenses ''Options

getUserConfig :: IO (Maybe Config)
getUserConfig = testGetUserConfig
  where testGetUserConfig = return . decode $ defaultConfigFileContent

getUserConfigE :: IO (Either String Config)
getUserConfigE = testGetUserConfig
  where testGetUserConfig = return . decodeEither $ defaultConfigFileContent

resetUserConfig :: IO ()
resetUserConfig = undefined

instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    cDefaultOpts <- o .:? "defaultOptions" .!= mempty
    cSites'      <- o .:? "sites"          .!= []
    cEditor      <- o .:? "editor"
    let cSites = if null cSites' then [soSite] else site <$> cSites'
    return Config{..}

instance FromJSON Options where
  parseJSON = withObject "options" $ \o -> do
    oGoogle <- o .:? "google"    .!= oGoogle defaultOptions
    oLucky  <- o .:? "lucky"     .!= oLucky defaultOptions
    oLimit  <- o .:? "limit"     .!= oLimit defaultOptions
    oSite'  <- o .:? "site"      .!= Site' (oSite defaultOptions)
    oUi     <- o .:? "interface" .!= oUi defaultOptions
    let oSite = site oSite'
    return Options{..}

-- Allow users to have a more intuitive yaml config than the JSON api
newtype Site' = Site' { site :: Site }
instance FromJSON Site' where
  parseJSON = withObject "site" $ \o -> do
    sUrl      <- o .: "url"
    sApiParam <- o .: "shortcode"
    return . Site' $ Site {..}

instance ToJSON Config where
  toJSON = undefined

instance Monoid Options where
  mempty          = defaultOptions
  o1 `mappend` o2 = o2

instance FromJSON Editor where
  parseJSON = withText "editor" $ \s -> do
    let c = capitalize . T.unpack $ s
    return $ fromMaybe (CustomEditor s) (readMaybe c)

instance FromJSON Interface where
  parseJSON = withText "interface" $ \s -> do
    let c = capitalize . T.unpack $ s
    fromMaybe (fail "invalid interface") (return <$> readMaybe c)

defaultOptions :: Options
defaultOptions = Options
  { oGoogle = True
  , oLucky  = False
  , oLimit  = 25
  , oSite   = soSite
  , oUi     = Brick }

soSite :: Site
soSite = Site
  { sUrl = "https://stackoverflow.com"
  , sApiParam = "stackoverflow" }

capitalize :: String -> String
capitalize []       = []
capitalize (h:tail) = toUpper h : fmap toLower tail

-- Kept as ByteString instead of Config so that end users can see comments
defaultConfigFileContent :: ByteString
defaultConfigFileContent = [r|
# default CLI flags (see `so --help` for info)
defaultOptions:
  google: yes
  lucky: no

# ui options: brick, basic
ui: brick

# stack exchange sites available for searching
# you can find more at https://api.stackexchange.com/docs/sites
sites:

  - url: https://stackoverflow.com
    shortcode: stackoverflow

  - url: https://serverfault.com
    shortcode: serverfault

  - url: https://superuser.com
    shortcode: superuser

  - url: https://askubuntu.com
    shortcode: askubuntu

  - url: https://apple.stackexchange.com
    shortcode: apple

  - url: https://gaming.stackexchange.com
    shortcode: gaming

  - url: https://math.stackexchange.com
    shortcode: math

  - url: https://meta.stackexchange.com
    shortcode: meta

  - url: https://english.stackexchange.com
    shortcode: english

  - url: https://tex.stackexchange.com
    shortcode: tex

  - url: https://unix.stackexchange.com
    shortcode: unix
|]
