module Utils
  ( suffixLenses
  , capitalize
  , (<$$>)
  , (!?)
  , (.*.)
  , color
  , code
  , err
  ) where

import           Data.Char           (toLower, toUpper)
import           Data.Semigroup      (Semigroup, (<>))
import           Data.String         (IsString, fromString)

import           Brick.Types         (suffixLenses)
import           Lens.Micro          (ix, (^?))
import qualified System.Console.ANSI as A

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

-- | Capitalize first letter of string, lowercase rest
capitalize :: String -> String
capitalize []      = []
capitalize (h:end) = toUpper h : fmap toLower end

---- ANSI helpers

-- | Style code
code :: (Semigroup s, IsString s) => s -> s
code = color A.Vivid A.Cyan

-- | Style errors with vivid red
err :: (Semigroup s, IsString s) => s -> s
err = color A.Vivid A.Red

-- | Style strings with given intensity, color
color :: (Semigroup s, IsString s) => A.ColorIntensity -> A.Color -> s -> s
color i c s = start <> s <> reset
  where start = fromString . A.setSGRCode $ [A.SetColor A.Foreground i c]
        reset = fromString . A.setSGRCode $ [A.Reset]
