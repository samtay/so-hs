{-# LANGUAGE OverloadedStrings #-}
module Cli
  (
  ) where

import Control.Monad (join, when)
import Data.Semigroup ((<>))
import System.IO (stderr)
import System.Exit (exitSuccess, exitFailure)

import Control.Lens (each, (^.), (^..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative

import Config
import StackOverflow
import State
import Utils

-- | Get and parse args from command line and return resulting `SO` type
run :: IO SO
run = do
  ecfg <- getUserConfig'
  either showUserConfigError go ecfg
  where
    go :: Config -> IO SO
    go cfg = join . execParser $
      info (helper <*> parseCli cfg)
      (  fullDesc
      <> progDesc "Stack Overflow from your terminal"
      )
    showUserConfigError :: String -> IO SO
    showUserConfigError e = do
      TIO.hPutStrLn stderr . T.unwords
        $ [ "It looks like there is an error in your configuration."
          , "If you're having trouble fixing it, you can always run:"
          , "`so --reset-config` to reset to defaults."
          , "\n\nFor reference, the yaml parsing error was: "
          , T.pack e ]
      exitFailure

-- | Intermediary function that handles actions determined
-- by CLI that don't fit within the core `SO` state.
cli :: Config  -- ^ User config
    -> Options -- ^ Options parsed from CLI
    -> Bool    -- ^ Print sites flag
    -> Bool    -- ^ Reset configuration file flag
    -> Text    -- ^ Query argument parsed from CLI
    -> IO SO   -- ^ Resulting state
cli cfg opts printsites reset query = do
  when reset resetUserConfig
  when printsites $ do
    mapM_ TIO.putStrLn
      [T.concat [sApiParam s, ": ", sUrl s] | s <- cSites cfg]
    exitSuccess
  return $ SO query [] opts

-- | Parse full CLI options and args
parseCli :: Config -> Parser (IO SO)
parseCli cfg = cli cfg
  <$> parseOpts cfg
  <*> switch
      ( long "print-sites"
     <> help "Print StackExchange sites and exit"
      )
  <*> switch
      ( long "reset-config"
     <> help "Reset configuration to defaults"
      )
  <*> multiTextArg (metavar "QUERY")

-- | Parse CLI options that have defaults in 'Config'
parseOpts :: Config -> Parser Options
parseOpts cfg = Options
  <$> enableDisableOpt undefined undefined
  <*> enableDisableOpt undefined undefined
  <*> option auto
      ( long "limit"
     <> metavar "INT"
     <> help "Upper limit on number of questions to fetch"
     <> value (cfg ^. cDefaultOptsL ^. oLimitL)
      )
  <*> strOption
      ( long "site"
     <> short 's'
     <> metavar "SHORTCODE"
     <> help "StackExchange site to search. See --print-sites for available options."
     <> value (cfg ^. cDefaultOptsL ^. oSiteL)
     <> completeWith (T.unpack . sApiParam <$> cSites cfg)
      )
  <*> uiOption
      ( long "interface"
     <> short 'i'
     <> metavar "brick|prompt"
     <> help "Interface for exploring questions and answers."
     <> value (cfg ^. cDefaultOptsL ^. oUiL)
      )

-- | Bool option determind by flag in [--option|--no-option]
enableDisableOpt :: Bool -> Mod OptionFields String -> Parser Bool
enableDisableOpt = undefined

uiOption :: Mod OptionFields Interface -> Parser Interface
uiOption = undefined

multiTextArg :: Mod ArgumentFields Text -> Parser Text
multiTextArg mods = T.concat <$> some (strArgument mods)
