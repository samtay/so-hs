module Utils
  ( suffixLenses
  , (<$$>)
  ) where

import Brick.Types (suffixLenses)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
