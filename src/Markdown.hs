{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module Markdown where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Applicative      (empty)
import           Control.Monad            (void, unless)
import           Data.Foldable            (asum, fold)
import           Data.Maybe               (fromMaybe)

--------------------------------------------------------------------------------
-- Library imports:
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Text.HTML.TagSoup.Entity (lookupEntity)
import           Text.Megaparsec
import           Text.Megaparsec.Text

--------------------------------------------------------------------------------
-- Local imports:
import           Types

--------------------------------------------------------------------------------
-- Types:

-- | This representation of markdown is purposefully kept simple. Not many
-- markdown specifications are implemented here because there is only so much
-- we can render, helpfully, in a terminal.
--
-- TODO possibly parse <kbd></kbd> (this is easy anyway)
--
-- TODO decide if combined text styles is worth implementing
-- (e.g. ***example*** being italic and bold)
data Markdown = Markdown [Segment Text]
  deriving (Show)

-- | Segment represents a chunk of markdown text in a particular style
data Segment a
  = SPlain a
  | SBold a
  | SItalic a
  | SCode a
  | SQuote a
  deriving (Show, Functor)

--------------------------------------------------------------------------------
-- Parser functions:

-- | Simple text to markdown function, never fails, worst case scenario the
-- markdown is kept as a single plain segment. Accepts different levels of parsing.
--
-- Note, oddly, that SO API returns html entities even within code sections.
markdown :: TextDisplay -> Text -> Markdown
markdown Raw raw          = Markdown [SPlain raw]
markdown HtmlEntities raw = Markdown [SPlain (replaceEntities raw)]
markdown Pretty raw       = Markdown . (fmap . fmap) replaceEntities .
  fromMaybe [SPlain raw] $ parseMaybe parseSegments raw

replaceEntities :: Text -> Text
replaceEntities raw = fromMaybe raw $ parseMaybe parseEntities raw
  where
    parseEntities :: Parser Text
    parseEntities = fmap fold . many $ fmap T.pack entity <|> fmap T.singleton anyChar
    entity :: Parser String
    entity = do
      e <- char '&' >> someTill anyChar (char ';')
      return . fromMaybe ('&' : e ++ [';']) $ lookupEntity e

parseMarkdown :: Parser Markdown
parseMarkdown = Markdown <$> parseSegments <* eof

parseSegments :: Parser [Segment Text]
parseSegments = reverse . collapse [] <$> many (eitherP parseFormatted anyChar)
  where
    parseFormatted = try (SCode <$> parseCode)
                 <|> try (SQuote <$> parseQuote)
                 <|> try (SBold <$> (enclosedByNoSpace "**" <|> enclosedByNoSpace "__"))
                 <|> try (SItalic <$> (enclosedByNoSpace "*" <|> enclosedByNoSpace "_"))
    collapse ss                []               = ss
    collapse ss                ((Left s) : cs)  = collapse (s : ss)                        cs
    collapse ((SPlain t) : ss) ((Right c) : cs) = collapse ((SPlain $ T.snoc t c) : ss)    cs
    collapse ss                ((Right c) : cs) = collapse ((SPlain $ T.singleton c) : ss) cs

parseCode :: Parser Text
parseCode = parseCodeInline <|> fmap T.pack parseCodeBlock
  where
    parseCodeInline = asum $ enclosedBy <$> ["```", "``", "`"]
    parseCodeBlock = unlines <$> do
      void eol <|> bof
      some $ string "    " *> someTill anyChar (void eol <|> eof)

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
  _ <- string d
  notFollowedBy spaceChar
  someTill
    (try ((:) <$> spaceChar <*> string d) <|> pure <$> anyChar) $ do
      notFollowedBy spaceChar
      string d

-- | Match any text enclosed by a delimiter
enclosedBy :: String -> Parser Text
enclosedBy d = fmap T.pack $
  string d >> someTill anyChar (string d)

-- | Match beginning of file
bof :: Parser ()
bof = getPosition >>= \(SourcePos _ l c) ->
  unless (l == c && unPos c == 1) empty
