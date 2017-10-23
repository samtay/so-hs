{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Markdown where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Applicative  (empty)
import           Control.Monad        (guard)
import           Data.Foldable        (asum)
import           Data.Maybe           (fromMaybe)

--------------------------------------------------------------------------------
-- Library imports:
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.Megaparsec
import           Text.Megaparsec.Text

--------------------------------------------------------------------------------
-- Local imports:

--------------------------------------------------------------------------------
-- Types:

-- | This representation of markdown is purposefully kept simple. Not many
-- markdown specifications are implemented here because there is only so much
-- we can render, helpfully, in a terminal.
--
-- TODO decide if combined text styles is worth implementing
-- (e.g. ***example*** being italic and bold)
data Markdown = Markdown [Segment]
  deriving (Show)

-- | Segment represents a chunk of markdown text in a particular style
data Segment
  = SPlain Text
  | SBold Text
  | SItalic Text
  | SCode Text
  | SQuote Text
  deriving (Show)

--------------------------------------------------------------------------------
-- Parser functions:

-- | Simple text to markdown function, never fails, worst case scenario the
-- markdown is kept as a single plain segment
markdown :: Text -> Markdown
markdown raw = fromMaybe (Markdown [SPlain raw]) $ parseMaybe parseMarkdown raw

parseMarkdown :: Parser Markdown
parseMarkdown = Markdown <$> (many parseSegment) <* eof

-- TODO use strategy from enclosedBy to gather plain segments
parseSegment :: Parser Segment
parseSegment =
      try (SCode <$> parseCode)
  <|> try (SQuote <$> parseQuote)
  <|> try (SBold <$> (enclosedByNoSpace "**" <|> enclosedByNoSpace "__"))
  <|> try (SItalic <$> (enclosedByNoSpace "*" <|> enclosedByNoSpace "_"))
  <|> SPlain . T.pack <$> many anyChar

parseCode :: Parser Text
parseCode = parseCodeInline <|> fmap T.pack parseCodeBlock
  where
    parseCodeInline = asum $ enclosedBy <$> ["```", "``", "`"]
    parseCodeBlock = unlines <$> do
      _ <- eol
      some $ string "    " *> someTill anyChar eol

-- | If I decide this is useful I'll implement it
parseQuote :: Parser Text
parseQuote = empty

-- | Match any text enclosed by a delimiter, but requiring a non-space character
-- directly following and preceding the first and final delimiter, respectively.
--
-- For example, **this matches** but ** this doesn't **
-- TODO consider capturing escaped delimiters..
enclosedByNoSpace :: String -> Parser Text
enclosedByNoSpace d = T.pack . concat <$> do
  string d
  notFollowedBy spaceChar
  someTill
    (try ((:) <$> spaceChar <*> string d) <|> pure <$> anyChar) $ do
      notFollowedBy spaceChar
      string d

-- | Match any text enclosed by a delimiter
enclosedBy :: String -> Parser Text
enclosedBy d = fmap T.pack $
  string d >> someTill anyChar (string d)
