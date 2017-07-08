{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Config where

import Data.ByteString (ByteString)
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
  , oSite :: Text
  , oUi :: Interface
  } deriving (Show)

data Interface = Prompt | Brick
  deriving (Show)

-- Note emacs requires process substitution, check if possible with shelly/turtle
data Editor = Less | More | Vim | CustomEditor Text
  deriving (Show)

suffixLenses ''Config
suffixLenses ''Options

getUserConfig :: IO (Maybe Config)
getUserConfig = testGetUserConfig
  where testGetUserConfig = return . decode $ defaultConfigFileContent

getUserConfig' :: IO (Either String Config)
getUserConfig' = testGetUserConfig
  where testGetUserConfig = return . decodeEither $ defaultConfigFileContent

resetUserConfig :: IO ()
resetUserConfig = undefined

-- | TODO implement... then move where appropriate
editWith :: Text -> Editor -> IO ()
editWith t e = useTurtleTo $ t |> editCommand e
  where useTurtleTo = undefined
        (|>)        = undefined

editCommand :: Editor -> Text
editCommand Less             = "less"
editCommand More             = "more"
editCommand Vim              = "vim +':setlocal buftype = nofile' -"
editCommand (CustomEditor c) = c

instance FromJSON Config where
  parseJSON = undefined

instance ToJSON Config where
  toJSON = undefined

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

  - site_url: https://stackoverflow.com
    api_site_parameter: stackoverflow

  - site_url: https://serverfault.com
    api_site_parameter: serverfault

  - site_url: https://superuser.com
    api_site_parameter: superuser

  - site_url: https://askubuntu.com
    api_site_parameter: askubuntu

  - site_url: https://apple.stackexchange.com
    api_site_parameter: apple

  - site_url: https://gaming.stackexchange.com
    api_site_parameter: gaming

  - site_url: https://math.stackexchange.com
    api_site_parameter: math

  - site_url: https://meta.stackexchange.com
    api_site_parameter: meta

  - site_url: https://english.stackexchange.com
    api_site_parameter: english

  - site_url: https://tex.stackexchange.com
    api_site_parameter: tex

  - site_url: https://unix.stackexchange.com
    api_site_parameter: unix
|]
