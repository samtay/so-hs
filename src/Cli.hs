{-# LANGUAGE OverloadedStrings #-}
module Cli
  ( execSO
  , parseSO
  ) where

import Control.Monad (join, when, forM_)
import Data.Function (on)
import Data.List (sortBy)
import Data.Semigroup (Semigroup, (<>))
import Data.String (IsString, fromString)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stderr)

import Control.Lens (each, (^.), (^..))
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Yaml (decode)
import Options.Applicative
import qualified System.Console.ANSI as A

import Config
import StackOverflow
import State
import Utils

-- | Parse args from command line and return resulting `SO` type
-- Exits with failure info on invalid args
execSO :: IO SO
execSO = do
  args <- getArgs
  eCfg <- getConfigE
  either showConfigError (execWith args) eCfg
  where
    execWith :: [String] -> Config -> IO SO
    execWith args cfg = join . handleParseResult $ parseSOPure cfg args

    showConfigError :: String -> IO SO
    showConfigError e = do
      f <- T.pack <$> getConfigFile
      TIO.hPutStrLn stderr . T.concat
        $ [ "It looks like there is an error in your configuration. "
          , "If you're having trouble fixing it, you can always run:"
          , "\n\n"
          , code ("    " <> "rm " <> f)
          , "\n\n"
          , "to reset to defaults. "
          , "For reference, the yaml parsing error was:"
          , "\n\n"
          , T.pack (err e) ]
      exitFailure

-- | Generalized 'execSO' that accepts 'Config' and args and returns
-- either an error message or resulting SO state
parseSO
  :: Config
  -> [String]
  -> IO (Either Text SO)
parseSO = handle .*. parseSOPure
  where handle :: ParserResult (IO SO) -> IO (Either Text SO)
        handle (Success ioa) = sequence (Right ioa)
        handle (Failure f)   = return . Left . T.pack . fst $ renderFailure f "so"
        handle _             = return . Left $ "Don't try to invoke completion from here!"

parseSOPure
  :: Config
  -> [String]
  -> ParserResult (IO SO)
parseSOPure = execParserPure defaultPrefs . fullCliInfo

fullCliInfo :: Config -> ParserInfo (IO SO)
fullCliInfo cfg =
  info (helper <*> parseCliExec cfg)
  (  fullDesc
  <> progDesc "Stack Overflow from your terminal"
  )

-- | Intermediary function that handles actions determined
-- by CLI that don't fit within the core `SO` state.
execCli
  :: Config  -- ^ User config
  -> Options -- ^ Options parsed from CLI
  -> Bool    -- ^ Print sites flag
  -> Text    -- ^ Query argument parsed from CLI
  -> IO SO   -- ^ Resulting state
execCli cfg opts printsites query = do
  when printsites (printSites . cSites $ cfg)  -- print sites and exit
  return $ SO query [] opts

-- | Parse full CLI options and args
parseCliExec
  :: Config         -- ^ User config
  -> Parser (IO SO) -- ^ Returns parser of IO action to return SO state
parseCliExec cfg = execCli cfg
  <$> parseOpts cfg
  <*> switch
      ( long "print-sites"
     <> help "Print Stack Exchange sites and exit"
      )
  <*> multiTextArg (metavar "QUERY")

-- | Parse CLI options that have defaults in 'Config'
parseOpts
  :: Config         -- ^ User config (used for default values)
  -> Parser Options -- ^ Returns parser of options
parseOpts cfg = Options
  <$> enableDisableOpt
      "google"
      "Enable/disable: using google to find relevant Stack Exchange links"
      (cfg ^. cDefaultOptsL ^. oGoogleL)
      showDefault
  <*> enableDisableOpt
      "lucky"
      "Enable/disable: just return the single most relevant answer"
      (cfg ^. cDefaultOptsL ^. oLuckyL)
      showDefault
  <*> option auto
      ( long "limit"
     <> short 'l'
     <> metavar "INT"
     <> help "Upper limit on number of questions to fetch"
     <> value (cfg ^. cDefaultOptsL ^. oLimitL)
     <> showDefault
      )
  <*> option (readSite (cSites cfg))
      ( long "site"
     <> short 's'
     <> metavar "CODE"
     <> help "Stack Exchange site to search. See --print-sites for available options."
     <> value (cfg ^. cDefaultOptsL ^. oSiteL)
     <> showDefaultWith (T.unpack . sApiParam)
     <> completeWith (T.unpack . sApiParam <$> cSites cfg)
      )
  <*> option readUi
      ( short 'i'
     <> metavar "brick|prompt"
     <> help "Interface for exploring questions and answers"
     <> value (cfg ^. cDefaultOptsL ^. oUiL)
     <> showDefault
      )

-- | Bool option determind by flag in [--option|--no-option]
enableDisableOpt
  :: String              -- ^ Long name
  -> String              -- ^ Help text
  -> Bool                -- ^ Default value
  -> Mod FlagFields Bool -- ^ Any additional mods
  -> Parser Bool
enableDisableOpt name helptxt def mods = -- last <$> some $ -- TODO finish this thought
  foldr (<|>) (pure def)
  [ flag' True
      ( hidden
     <> internal
     <> long name
     <> help helptxt
     <> mods
      )
  , flag' False
      ( hidden
     <> internal
     <> long ("no-" ++ name)
     <> help helptxt
     <> mods
      )
  , flag' def
      ( long ("[no-]" ++ name)
     <> help helptxt
     <> mods
      )
  ]

readUi :: ReadM Interface
readUi = str >>= \s -> maybe (rError s) return (decode s)
  where
    rError s = readerError . err . unwords
      $ [ BS.unpack s
        , "is not a valid interface. The available options are:"
        , "brick, prompt" ]

readSite :: [Site] -> ReadM Site
readSite sites = str >>= go sites
  where
    go [] sc = readerError . err . unwords
      $ [ sc
        , "is not a valid site shortcode."
        , "See --print-sites for help." ]
    go (s:ss) sc
      | T.pack sc == sApiParam s = return s
      | otherwise = go ss sc

-- | Takes 1 or more text arguments and returns them as single sentence argument
multiTextArg :: Mod ArgumentFields Text -> Parser Text
multiTextArg mods = T.unwords <$> many (strArgument mods)

-- | Print sites.
printSites :: [Site] -> IO ()
printSites sites = do
  let ordered = sortBy (compare `on` sApiParam) sites        -- alphabetically ordered
      m       = maximum . map (T.length . sApiParam) $ sites -- maximum shortcode length

  -- Print each site aligned on ":" separator
  forM_ ordered $ \s -> do
    let diff = m - T.length (sApiParam s)
        indent = T.replicate diff " "
    TIO.putStrLn . T.concat
      $ [indent, sApiParam s, ": ", sUrl s]

  exitSuccess

-- | Style code
code :: Text -> Text
code = color A.Vivid A.Cyan

-- | Style errors with vivid red
err :: (Semigroup s, IsString s) => s -> s
err = color A.Vivid A.Red

-- | Style strings with given intensity, color
color :: (Semigroup s, IsString s) => A.ColorIntensity -> A.Color -> s -> s
color i c s = start <> s <> reset
  where start = fromString . A.setSGRCode $ [A.SetColor A.Foreground i c]
        reset = fromString . A.setSGRCode $ [A.Reset]
