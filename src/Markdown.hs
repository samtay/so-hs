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

-- | Match any text enclosed by a delimiter, requiring a non-space character
-- directly following and preceding the first and final delimiter, respectively.
--
-- For example, **this matches** but ** this doesn't **
-- TODO consider capturing escaped delimiters..
enclosedByNoSpace :: String -> Parser Text
enclosedByNoSpace d = do
  string d  
  notFollowedBy spaceChar
  res <- go False []
  guard . not $ null res
  return $ T.pack res
  where
    go prevSp cs = do
      res <- eitherP (string d) (anyChar)
      case (prevSp, res) of
        (False, Left _) -> return cs
        (False, Right c) -> go (c == ' ') (cs ++ [c])
        (True, Left d')  -> go False (cs ++ d')
        (True, Right c)  -> go (c == ' ') (cs ++ [c])

-- | Match any text enclosed by a delimiter
-- TODO see if go is manyTill
enclosedBy :: String -> Parser Text
enclosedBy d = do
  string d
  res <- go []
  guard . not $ null res
  return $ T.pack res
  where
    go cs = do
      res <- eitherP (string d) (anyChar)
      case res of
        Left _  -> return cs
        Right c -> go (cs ++ [c])
