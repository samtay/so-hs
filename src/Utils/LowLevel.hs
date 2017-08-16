{-# LANGUAGE OverloadedStrings #-}
module Utils.LowLevel
  ( capitalize
  , (<$$>)
  , (!?)
  , (.*.)
  , whenDef
  , unlessDef
  , exitWithError
  , color
  , code
  , info
  , err
  , promptChar
  , promptLine
  , noBuffer
  , noBufferOn
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Data.Char           (toLower, toUpper)
import           Data.Semigroup      (Semigroup, (<>))
import           Data.String         (IsString, fromString)
import           System.Exit         (exitFailure)
import           System.IO           (BufferMode (..), Handle, hGetBuffering,
                                      hSetBuffering, stderr, stdin)

--------------------------------------------------------------------------------
-- Library imports:
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
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

exitWithError :: Text -> IO a
exitWithError e = TIO.hPutStrLn stderr (err e) >> exitFailure

-- | Style code
code :: (Semigroup s, IsString s) => s -> s
code = color A.Vivid A.Cyan

-- | Style errors with vivid red
err :: (Semigroup s, IsString s) => s -> s
err = color A.Vivid A.Red

-- | Style info with dull yellow
info :: (Semigroup s, IsString s) => s -> s
info = color A.Dull A.Yellow

-- | Style strings with given intensity, color
color :: (Semigroup s, IsString s) => A.ColorIntensity -> A.Color -> s -> s
color i c s = start <> s <> reset
  where start = fromString . A.setSGRCode $ [A.SetColor A.Foreground i c]
        reset = fromString . A.setSGRCode $ [A.Reset]

-- | Prompt for line of text
promptLine :: Text -> IO String
promptLine = prompt getLine

-- | Prompt for a single char without waiting for newline
promptChar :: Text -> IO Char
promptChar = prompt (noBuffer getChar)

-- | Prompt utility
prompt
  :: IO a  -- ^ Retrieval method
  -> Text  -- ^ Text to show as prompt
  -> IO a  -- ^ Return from retrieval method
prompt action txt = do
  A.setSGR [A.SetColor A.Foreground A.Dull A.Yellow]
  TIO.putStrLn $ "\n" <> txt
  A.setSGR []
  action

-- | Allow retrieval from stdin with a temporary NoBuffering mode
--
-- Specialized version of 'noBufferOn', such that @noBufferOn == noBuffer stdin@
noBuffer :: IO a -> IO a
noBuffer = noBufferOn stdin

-- | Run an action with temporary NoBuffering mode on given handle
noBufferOn :: Handle -> IO a -> IO a
noBufferOn handle action = do
  mode <- hGetBuffering handle
  hSetBuffering handle NoBuffering
  result <- action
  hSetBuffering handle mode
  return result
