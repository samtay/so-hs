{-# LANGUAGE OverloadedStrings #-}
module Cli
  ( run
  ) where

import Control.Monad (join, when, forM_)
import Data.Function (on)
import Data.List (sortBy)
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
run = getConfigE >>= either showConfigError go
  where
    go :: Config -> IO SO
    go cfg = join . execParser $
      info (helper <*> parseCli cfg)
      (  fullDesc
      <> progDesc "Stack Overflow from your terminal"
      )
    showConfigError :: String -> IO SO
    showConfigError e = do
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
  when reset (resetConfig >> exitSuccess)
  when printsites (printSites . cSites $ cfg)
  return $ SO query [] opts

-- | Parse full CLI options and args
parseCli
  :: Config         -- ^ User config
  -> Parser (IO SO) -- ^ Returns parser of IO action to return SO state
parseCli cfg = cli cfg
  <$> parseOpts cfg
  <*> switch
      ( long "print-sites"
     <> help "Print Stack Exchange sites and exit"
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
readUi = str >>= go where
  go s
    | s `elem` ["b", "brick"]  = return Brick
    | s `elem` ["p", "prompt"] = return Prompt
    | otherwise = readerError . unwords
      $ [ s
        , "is not a valid interface. The available options are:"
        , "brick, prompt" ]

readSite :: [Site] -> ReadM Site
readSite sites = str >>= go sites
  where
    go [] sc = readerError . unwords
      $ [ sc
        , "is not a valid site shortcode."
        , "See --print-sites for help." ]
    go (s:ss) sc
      | T.pack sc == sApiParam s = return s
      | otherwise = go ss sc

-- | Takes 1 or more text arguments and returns them as single sentence argument
multiTextArg :: Mod ArgumentFields Text -> Parser Text
multiTextArg mods = T.unwords <$> many (strArgument mods)

-- | Print sites. TODO align and sort nicely
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
