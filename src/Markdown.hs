{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
module Markdown
  ( Markdown(..)
  , markdown
  , Segment(..)
  , fromSegment
  , parseMarkdown
  , parseSegments, parseSegmentExcluding, Exclusion(..) -- TODO remove
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Applicative      (empty)
import           Control.Monad            (guard, unless, void)
import           Data.Char                (isPunctuation, isSeparator)
import           Data.Foldable            (asum, fold)
import           Data.Maybe               (fromMaybe, isJust, isNothing)
import           Data.Monoid              ((<>))
import           Data.String              (IsString (..))

--------------------------------------------------------------------------------
-- Library imports:
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
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
-- TODO decide if combined text styles is worth implementing
-- (e.g. ***example*** being italic and bold)
data Markdown = Markdown [Segment Text]
  deriving (Show, Eq)

instance IsString Markdown where
  fromString = Markdown . (: []) . fromString

instance Monoid Markdown where
  mempty = Markdown mempty
  mappend (Markdown segsA) (Markdown segsB) = Markdown (mappend segsA segsB)

-- | Segment represents a chunk of markdown text in a particular style
data Segment a
  = SPlain a
  | SBold a
  | SItalic a
  | SCode a
  | SKbd a
  | SQuote a
  deriving (Show, Eq, Functor)

instance IsString a => IsString (Segment a) where
  fromString = SPlain . fromString

fromSegment :: Segment a -> a
fromSegment = \case
  SPlain a -> a
  SBold a -> a
  SItalic a -> a
  SCode a -> a
  SKbd a -> a
  SQuote a -> a

-- | This can be used for more complicated things in the future,
-- especially if handling nested styles.
data Exclusion
  = Underscores
  deriving (Show, Eq, Ord)

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
parseSegments = reverse . collapse [] <$> loop mempty
  where
    loop exclusions = do
      (seg, nextExclusions) <- parseSegmentExcluding exclusions <|> pure ("", mempty)
      if T.null (fromSegment seg)
         then return []
         else (seg:) <$> loop nextExclusions

    collapse ((SPlain t1) : xs) ((SPlain t2) : ys) =
      collapse ((SPlain $ t1 <> t2) : xs) ys
    collapse xs (y:ys) = collapse (y : xs) ys
    collapse xs [] = xs

parseSegmentExcluding :: Set Exclusion -> Parser (Segment Text, Set Exclusion)
parseSegmentExcluding exclusions =
      tryPure (SCode <$> parseCode)
  <|> tryPure (SKbd <$> parseKbd)
  <|> tryPure (SQuote <$> parseQuote)
  <|> tryPure (SBold <$> ( (enclosedByNoInnerSpace "**")
                       <|> (unlessExcluding Underscores >> enclosedByNoInnerSpace' "__")
                         ))
  <|> tryPure (SItalic <$> ( (enclosedByNoInnerSpace "*")
                         <|> (unlessExcluding Underscores >> enclosedByNoInnerSpace'' "_")
                           ))
  <|> do plain <- some (noneOf delims) <|> ((:[]) <$> anyChar)
         let c = last plain
             nextExclusion = if (isPunctuation c || isSeparator c) && c /= '_'
                                then mempty
                                else Set.singleton Underscores
         return (SPlain $ T.pack plain, nextExclusion)
  where
    tryPure = fmap (, mempty) . try
    unlessExcluding ex = guard $ Set.notMember ex exclusions
    delims :: [Char]
    delims = "*_`<\n"

parseCode :: Parser Text
parseCode = parseCodeInline <|> fmap T.pack parseCodeBlock
  where
    parseCodeInline = asum $ enclosedBy <$> ["```", "``", "`"]
    parseCodeBlock = unlines <$> do
      void eol <|> bof
      some $ string "    " *> someTill anyChar (void eol <|> eof)

parseKbd :: Parser Text
parseKbd = fmap T.pack $
  string "<kbd>" >> someTill anyChar (string "</kbd>")

-- | If I decide this is useful I'll implement it
parseQuote :: Parser Text
parseQuote = empty

-- | Match any text enclosed by a delimiter, but requiring a non-space character
-- directly following and preceding the first and final delimiter, respectively.
--
-- For example, **this matches** but ** this doesn't **
enclosedByNoInnerSpace :: String -> Parser Text
enclosedByNoInnerSpace d = T.pack . concat <$> do
  _ <- string d
  notFollowedBy spaceChar
  someTill
    (try ((:) <$> spaceChar <*> string d) <|> (:[]) <$> anyChar) $ do
      notFollowedBy spaceChar
      string d

-- | Just like 'enclosedByNoInnerSpace', but requires separation/punctuation character
-- following the final delimiter. This is very specifically used for underscores.
-- Note that really this is half of a preceding/following separation rule, but the
-- first half of this rule, by megaparsec design, must be handled in outer scope.
-- See 'parseSegmentExcluding'.
--
-- For example, __this matches__ but __ this doesn't __ and text__this__doesn't__
enclosedByNoInnerSpace' :: String -> Parser Text
enclosedByNoInnerSpace' d = do
  inner <- enclosedByNoInnerSpace d
  notFollowedBy (char '_')
  lookAhead $ void separatorChar <|> void punctuationChar <|> eof
  return inner

-- | Again this is a really specific underscores parser. StackOverflow is just
-- weird when it comes to underscores. This one ensures that the parsed text
-- content either has a space or doesn't have an underscore
-- TODO see if this is all just one big rule ( the first/second halves above)
enclosedByNoInnerSpace'' :: String -> Parser Text
enclosedByNoInnerSpace'' d = do
  inner <- enclosedByNoInnerSpace' d
  guard $ (isNothing $ T.findIndex (== '_') inner) || (isJust $ T.findIndex isSeparator inner)
  return inner

-- | Match any text enclosed by a delimiter
enclosedBy :: String -> Parser Text
enclosedBy d = fmap T.pack $
  string d >> someTill anyChar (string d)

-- | Match beginning of file
bof :: Parser ()
bof = getPosition >>= \(SourcePos _ l c) ->
  unless (l == c && unPos c == 1) empty
