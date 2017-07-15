{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Cli
  ( run
  ) where

import           Data.Function         (on)
import           Data.List             (sortBy)
import           Data.Maybe            (listToMaybe)
import           Data.Semigroup        (Semigroup, (<>))
import           Data.String           (IsString, fromString)
import           System.Exit           (exitFailure)
import           System.IO             (stderr)

import qualified Data.ByteString.Char8 as BS
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Data.Yaml             (decode)
import           Lens.Micro            ((^.))
import           Options.Applicative
import qualified System.Console.ANSI   as A

import           Config
import           Types

data Cli = Cli
  { options :: Options
  , query   :: Text
  }

-- | Parse args from command line and return resulting `SO` type
-- Exits with failure info on invalid args
run :: IO SO
run = getConfigE >>= either exitConfigError execCliParser >>= cliToSO
  where
    execCliParser :: Config -> IO Cli
    execCliParser = execParser . cliParserInfo

    cliToSO :: Cli -> IO SO
    cliToSO Cli{options, query} = return $ SO query [] options

exitConfigError :: String -> IO a
exitConfigError e = do
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

-- | Full parser info with --help and --print-sites options
cliParserInfo :: Config -> ParserInfo Cli
cliParserInfo cfg =
  info (helper <*> printSitesOption cfg <*> cliParser cfg)
  (  fullDesc
  <> progDesc "Stack Overflow from your terminal"
  )

-- | Parse 'Cli' options and query arg
cliParser
  :: Config     -- ^ User config
  -> Parser Cli -- ^ Returns 'Cli' parser
cliParser cfg = Cli
  <$> parseOpts cfg
  <*> multiTextArg (metavar "QUERY")

-- | Parse CLI options that have defaults in 'Config'
parseOpts
  :: Config         -- ^ User config (used for default values)
  -> Parser Options -- ^ Returns 'Options' parser
parseOpts cfg = Options
  <$> enableDisableOpt
      "google"
      "Use google to find relevant question links"
      (cfg ^. cDefaultOptsL ^. oGoogleL)
      showDefault
  <*> enableDisableOpt
      "lucky"
      "Return the single most relevant answer"
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

-- | Top level info option parser that lives in ParserInfo
--
-- By "top level" and "info option", I mean this is like --help
-- in that once encountered, regardless of any other args,
-- the program will spit out sites and exit.
printSitesOption :: Config -> Parser (a -> a)
printSitesOption cfg = infoOption (prettifiedSites (cSites cfg))
  ( long "print-sites"
 <> help "Print Stack Exchange sites and exit"
  )

-- | Bool option determind by flag in [--option|--no-option]
enableDisableOpt
  :: String              -- ^ Long name
  -> String              -- ^ Help text
  -> Bool                -- ^ Default value
  -> Mod FlagFields Bool -- ^ Any additional mods
  -> Parser Bool
enableDisableOpt name helptxt def mods =
  enabledOpt <|> disabledOpt <|> showOpt
  where
    enabledOpt  = mkOpt flag' True name (hidden <> internal)
    disabledOpt = mkOpt flag' False ("no-" ++ name) (hidden <> internal)
    showOpt     = mkOpt (flag def) (not def) ("[no-]" ++ name) mempty
    defWord     = if def then "enabled" else "disabled"
    mkOpt fn b n ms =
      fn b
        ( long n
       <> help (helptxt ++ " (default: " ++ defWord ++ ")")
       <> ms
       <> mods
        )

-- | Read interface option (uses same mechanism as FromJSON parser)
readUi :: ReadM Interface
readUi = str >>= \s -> maybe (rError s) return (decode s)
  where
    rError s = readerError . err . unwords
      $ [ BS.unpack s
        , "is not a valid interface. The available options are:"
        , "brick, prompt" ]

-- | Read site option, compare it against given 'sites' argument
readSite :: [Site] -> ReadM Site
readSite sites =
  str >>= \s -> maybe (throwErr s)
                      return
                      (findWith (matchSc s) sites)
  where
    findWith :: (a -> Bool) -> [a] -> Maybe a
    findWith p = listToMaybe . filter p

    matchSc :: Text -> Site -> Bool
    matchSc sc site = sc == site ^. sApiParamL

    throwErr :: Text -> ReadM Site
    throwErr sc = readerError . err . unwords
      $ [ T.unpack sc
        , "is not a valid site shortcode."
        , "See --print-sites for help." ]

-- | Takes 1 or more text arguments and returns them as single sentence argument
multiTextArg :: Mod ArgumentFields Text -> Parser Text
multiTextArg mods = T.unwords <$> many (strArgument mods)

-- | Align and alphabetically order sites
prettifiedSites :: [Site] -> String
prettifiedSites sites =
  T.unpack
    . T.unlines
    $ [ T.concat [indent s, sApiParam s, ": ", sUrl s] | s <- ordered ]
 where
  ordered  = sortBy (compare `on` sApiParam) sites        -- alphabetically ordered
  maxL     = maximum . map (T.length . sApiParam) $ sites -- maximum shortcode length
  diff s   = maxL - T.length (sApiParam s)
  indent s = T.replicate (diff s) " "

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
