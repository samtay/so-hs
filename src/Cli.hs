{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Cli
  ( runCli
  , Cli(..)
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Data.Function         (on)
import           Data.List             (sortBy)
import           Data.Maybe            (listToMaybe)
import           Data.Semigroup        ((<>))

--------------------------------------------------------------------------------
-- Library imports:
import qualified Data.ByteString.Char8 as BS
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Yaml             (decode)
import           Lens.Micro            ((^.))
import           Options.Applicative

--------------------------------------------------------------------------------
-- Local imports:
import           Types
import           Utils                 hiding (info)

data Cli = Cli
  { cOptions :: Options
  , cQuery   :: Text
  }

-- | Parse args from command line and return resulting `SO` type
-- Exits with failure info on invalid args
runCli :: AppConfig -> IO Cli
runCli = execParser . cliParserInfo

-- | Full parser info with --help and --print-sites options
cliParserInfo :: AppConfig -> ParserInfo Cli
cliParserInfo cfg =
  info (helper <*> printSitesOption cfg <*> cliParser cfg)
  (  fullDesc
  <> progDesc "Stack Overflow from your terminal"
  )

-- | Parse 'Cli' options and query arg
cliParser
  :: AppConfig  -- ^ User config
  -> Parser Cli -- ^ Returns 'Cli' parser
cliParser cfg = Cli
  <$> parseOpts cfg
  <*> multiTextArg (metavar "QUERY")

-- | Parse CLI options that have defaults in 'AppConfig'
parseOpts
  :: AppConfig      -- ^ User config (used for default values)
  -> Parser Options -- ^ Returns 'Options' parser
parseOpts cfg = Options
  <$> enableDisableOpt
      "google"
      "Use google to find relevant question links"
      (cfg ^. cDefaultOpts ^. oGoogle)
      showDefault
  <*> enableDisableOpt
      "lucky"
      "Return the single most relevant answer"
      (cfg ^. cDefaultOpts ^. oLucky)
      showDefault
  <*> option auto
      ( long "limit"
     <> short 'l'
     <> metavar "INT"
     <> help "Upper limit on number of questions to fetch"
     <> value (cfg ^. cDefaultOpts ^. oLimit)
     <> showDefault
      )
  <*> option (readSite (cfg ^. cSites))
      ( long "site"
     <> short 's'
     <> metavar "CODE"
     <> help "Stack Exchange site to search. See --print-sites for available options."
     <> value (cfg ^. cDefaultOpts ^. oSite)
     <> showDefaultWith (T.unpack . _sApiParam)
     <> completeWith (T.unpack . _sApiParam <$> cfg ^. cSites)
      )
  <*> option readUi
      ( short 'i'
     <> metavar "brick|prompt"
     <> help "Interface for exploring questions and answers"
     <> value (cfg ^. cDefaultOpts ^. oUi)
     <> showDefault
      )
  <*> enableDisableOpt
      "raw"
      "Display raw markdown text"
      (cfg ^. cDefaultOpts ^. oRaw)
      showDefault

-- | Top level info option parser that lives in ParserInfo
--
-- By "top level" and "info option", I mean this is like --help
-- in that once encountered, regardless of any other args,
-- the program will spit out sites and exit.
printSitesOption :: AppConfig -> Parser (a -> a)
printSitesOption cfg = infoOption (prettifiedSites (cfg ^. cSites))
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
    matchSc sc site = sc == site ^. sApiParam

    throwErr :: Text -> ReadM Site
    throwErr sc = readerError . err . unwords
      $ [ T.unpack sc
        , "is not a valid site shortcode."
        , "See --print-sites for help." ]

-- | Takes 1 or more text arguments and returns them as single sentence argument
multiTextArg :: Mod ArgumentFields Text -> Parser Text
multiTextArg mods = T.unwords <$> some (strArgument mods)

-- | Align and alphabetically order sites
prettifiedSites :: [Site] -> String
prettifiedSites sites =
  T.unpack . T.unlines
    $ [T.concat [indent s, _sApiParam s, ": ", _sUrl s] | s <- ordered]
  where
    ordered = sortBy (compare `on` _sApiParam) sites -- alphabetically ordered
    maxL = maximum . map (T.length . _sApiParam) $ sites -- maximum shortcode length
    diff s = maxL - T.length (_sApiParam s)
    indent s = T.replicate (diff s) " "
