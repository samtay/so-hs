module Utils
  ( suffixLenses
  , (<$$>)
  ) where

import Brick.Types (suffixLenses)
import qualified Data.ByteString as BS
import qualified Data.Yaml as Y

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

eitherDecodeYamlFile :: Y.FromJSON a => FilePath -> IO (Either String a)
eitherDecodeYamlFile = fmap Y.decodeEither . BS.readFile
