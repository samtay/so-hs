module Utils
  ( suffixLenses
  , capitalize
  , (<$$>)
  , (.*.)
  ) where

import Data.Char (toUpper, toLower)

import Brick.Types (suffixLenses)

-- | Lift twice
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

-- | Double composition (allow first function to accept two arguments)
(.*.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
g .*. f = \x y -> g (f x y)

capitalize :: String -> String
capitalize []       = []
capitalize (h:end) = toUpper h : fmap toLower end
