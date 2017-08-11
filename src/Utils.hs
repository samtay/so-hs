module Utils
  ( suffixLenses
  , capitalize
  , (<$$>)
  , (!?)
  , (.*.)
  , whenDef
  , unlessDef
  , exitWithError
  , color
  , code
  , err
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Data.Char           (toLower, toUpper)
import           Data.Semigroup      (Semigroup, (<>))
import           Data.String         (IsString, fromString)
import           System.Exit         (exitFailure)
import           System.IO           (hPutStrLn, stderr)

--------------------------------------------------------------------------------
-- Library imports:
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

whenDef :: Monad m
  => a -- ^ Default value
  -> Bool -- ^ Predicate
  -> m a  -- ^ Action to run when predicate is 'True'
  -> m a
whenDef def b action = if b then action else return def

unlessDef :: Monad m
  => a -- ^ Default value
  -> Bool -- ^ Predicate
  -> m a  -- ^ Action to run when predicate is 'False'
  -> m a
unlessDef def b action = if not b then action else return def

-- | Capitalize first letter of string, lowercase rest
capitalize :: String -> String
capitalize []      = []
capitalize (h:end) = toUpper h : fmap toLower end

---- ANSI helpers

exitWithError :: String -> IO a
exitWithError e = hPutStrLn stderr (err e) >> exitFailure

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
