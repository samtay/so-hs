{-# LANGUAGE OverloadedStrings #-}
module Cli
  ( run
  ) where

import Control.Monad (join, when)
import Data.Semigroup ((<>))
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (stderr)

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
cli
  :: Config  -- ^ User config
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
parseCli
  :: Config         -- ^ User config
  -> Parser (IO SO) -- ^ Returns parser of IO action to return SO state
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
parseOpts
  :: Config         -- ^ User config (used for default values)
  -> Parser Options -- ^ Returns parser of options
parseOpts cfg = Options
  <$> undefined
  <*> undefined
  <*> option auto
      ( long "limit"
     <> metavar "INT"
     <> help "Upper limit on number of questions to fetch"
     <> value (cfg ^. cDefaultOptsL ^. oLimitL)
      )
  <*> strOption -- TODO READER FOR SITE
      ( long "site"
     <> short 's'
     <> metavar "SHORTCODE"
     <> help "StackExchange site to search. See --print-sites for available options."
     <> value (cfg ^. cDefaultOptsL ^. oSiteL)
     <> completeWith (T.unpack . sApiParam <$> cSites cfg)
      )
  <*> option readUi
      ( long "interface"
     <> short 'i'
     <> metavar "brick|prompt"
     <> help "Interface for exploring questions and answers."
     <> value (cfg ^. cDefaultOptsL ^. oUiL)
      )

-- | Bool option determind by flag in [--option|--no-option]
enableDisableOpt
  :: String
  -> String
  -> Bool
  -> Mod FlagFields Bool
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
readUi = str >>= go where
  go s
    | s `elem` ["b", "brick"]  = return Brick
    | s `elem` ["p", "prompt"] = return Prompt
    | otherwise = readerError . unwords
      $ [ s
        , "is not a valid interface. The available options are:"
        , "brick, prompt" ]

-- | Takes 1 or more text arguments and returns them as single sentence argument
multiTextArg :: Mod ArgumentFields Text -> Parser Text
multiTextArg mods = T.unwords <$> some (strArgument mods)