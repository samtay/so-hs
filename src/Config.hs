{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Yaml
import Text.RawString.QQ

import StackOverflow.Types

data Config = Config
  { defaultOpts :: () -- TODO CLI OPTS
  , ui          :: () -- need names for (prompt,haskeline) and (brick tui)
  , sites       :: [Site]
  , editor      :: Maybe Editor
  } deriving (Show)

-- Note emacs requires process substitution, check if possible with shelly/turtle
data Editor = Less | More | Vim | CustomEditor Text
  deriving (Show)

instance FromJSON Config where
  parseJSON = undefined

instance ToJSON Config where
  toJSON = undefined

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
