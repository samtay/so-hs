{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Config where

import Control.Monad (unless)

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Yaml (decode, decodeEither)
import qualified System.Directory as D
import System.FilePath ((</>))
import Text.RawString.QQ

import Types

getConfigWith :: Monad m => (ByteString -> m Config) -> IO (m Config)
getConfigWith decoder = do
  f <- getConfigFile
  exists <- D.doesFileExist f
  unless exists resetConfig
  yml <- BS.readFile f
  return $ decoder yml

getConfigM :: IO (Maybe Config)
getConfigM = getConfigWith decode

getConfigE :: IO (Either String Config)
getConfigE = getConfigWith decodeEither

resetConfig :: IO ()
resetConfig = do
  f <- getConfigFile
  BS.writeFile f defaultConfigFileContent

getConfigFile :: IO FilePath
getConfigFile = getXdgaFilePath D.XdgConfig "config.yml"

getXdgaFilePath :: D.XdgDirectory -> FilePath -> IO FilePath
getXdgaFilePath d f = do
  xdg <- D.getXdgDirectory d "so"
  D.createDirectoryIfMissing True xdg
  return (xdg </> f)

-- Kept as ByteString instead of Config so that end users can see comments
defaultConfigFileContent :: ByteString
defaultConfigFileContent = [r|# stack exchange sites available for searching
# you can find more at https://api.stackexchange.com/docs/sites
sites:

  - &defaultSite
    url: https://stackoverflow.com
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

# default CLI options (see `so --help` for info)
defaultOptions:
  google: yes
  lucky: no
  ui: brick # options: brick, prompt
  site: *defaultSite
|]
