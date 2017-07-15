module Utils
  ( suffixLenses
  , capitalize
  , (<$$>)
  , (!?)
  , (.*.)
  ) where

import           Data.Char   (toLower, toUpper)

import           Brick.Types (suffixLenses)
import           Lens.Micro  (ix, (^?))

-- | Lift twice
infixl 3 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

-- | Safe !!
(!?) :: [a] -> Int -> Maybe a
l !? i = l ^? ix i

-- | Double composition (allow first function to accept two arguments)
(.*.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
g .*. f = \x y -> g (f x y)

capitalize :: String -> String
capitalize []      = []
capitalize (h:end) = toUpper h : fmap toLower end
