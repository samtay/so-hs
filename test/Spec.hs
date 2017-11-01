{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson           as A
import qualified Data.Aeson.Types     as AT
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Data.Yaml            as Y
import           Test.Hspec

import           Markdown
import           StackOverflow.Google
import           Types
import           Utils

main :: IO ()
main = hspec $ do

  describe "Google Scraper" $ do
    it "parses english.meta question links" $
      parseIds <$> englishMetaBSL
        `shouldReturn` Right [4453, 2867, 2002, 3404]
    it "fails gracefully on botched parse" $
      pending
    it "fails gracefully on no connection" $
      pendingWith "look at other libraries that simulate this by throwing IO exceptions"

  describe "StackExchange API JSON" $ do
    it "parses json correctly" $
      allAnswerIds <$$> decodeQFromFile validQuestionsFile2
        `shouldReturn` Right [41781938,6932450,6936983,38290282,6937075]
    it "filters unanswered questions" $
      length <$$> decodeQFromFile validQuestionsFile30
        `shouldReturn` Right 29
    it "fails gracefully on botched attempt" $
      decodeQFromFile invalidQuestionsFile
        `shouldReturn` Left "Error in $.items[0]: key \"body_markdown\" not present"
    it "fails gracefully on no connection" $
      pendingWith "look at other libraries that simulate this by throwing IO exceptions"

  describe "Configuration" $ do
    it "handles bad yaml configuration" $
      decodeCfgFromFile siteStringFile
        `shouldReturn` Left "Error in $.defaultOptions.site: expected site, encountered String"
    it "reads site configuration properly" $
      _sApiParam . _oSite . _cDefaultOpts <$$> decodeCfgFromFile serverfaultFile
        `shouldReturn` Right "serverfault"
    it "always allows stackoverflow search" $
      _sApiParam <$$> _cSites <$$> decodeCfgFromFile noSitesFile
        `shouldReturn` Right ["stackoverflow"]
    it "handles bad custom ui command gracefully" $
      decodeCfgFromFile badUIFile
        `shouldReturn` Left "Error in $.defaultOptions.ui: invalid interface"

  describe "Markdown Parser" $ do
    it "parses raw mode as an identity op" $
      markdown Raw <$> validMarkdown
        `shouldReturn` Markdown [ SPlain "This is some **bold text**, __other bold text__ and\
                                         \ some _italicized text_.\nBut make sure that if\
                                         \ spaces_are_not_surrounding the delimiters, then\nthe\
                                         \ text__is__plain for underscores - but not*for*asterisks.\
                                         \ But backticks\nwill `always produce` code ``ya know``?\
                                         \ And if you need two backticks,\nyou can ```do `` within\
                                         \ three backticks```.\n\n    parseMarkdown :: Parser\
                                         \ Markdown\n    parseMarkdown = Markdown <$> parseSegments\
                                         \ <* eof\n\nAnd know that <kbd>z</kbd><kbd>z</kbd> centers\
                                         \ your cursor location in normal mode.\nAnd make sure that\
                                         \ &quot;quoted strings&quot; get their damn html entities\
                                         \ decoded.\n" ]
    it "replaces html entities" $
      markdown HtmlEntities <$> validMarkdown
        `shouldReturn` Markdown [ SPlain "This is some **bold text**, __other bold text__ and\
                                         \ some _italicized text_.\nBut make sure that if\
                                         \ spaces_are_not_surrounding the delimiters, then\nthe\
                                         \ text__is__plain for underscores - but not*for*asterisks.\
                                         \ But backticks\nwill `always produce` code ``ya know``?\
                                         \ And if you need two backticks,\nyou can ```do `` within\
                                         \ three backticks```.\n\n    parseMarkdown :: Parser\
                                         \ Markdown\n    parseMarkdown = Markdown <$> parseSegments\
                                         \ <* eof\n\nAnd know that <kbd>z</kbd><kbd>z</kbd> centers\
                                         \ your cursor location in normal mode.\nAnd make sure that\
                                         \ \"quoted strings\" get their damn html entities\
                                         \ decoded.\n" ]
    it "parses valid markdown to pretty format" $
      markdown Pretty <$> validMarkdown
        `shouldReturn` Markdown [ SPlain "This is some "
                                , SBold "bold text"
                                , SPlain ", "
                                , SBold "other bold text"
                                , SPlain " and some "
                                , SItalic "italicized text"
                                , SPlain ".\nBut make sure that if spaces_are_not_surrounding the\
                                         \ delimiters, then\nthe text__is__plain for underscores\
                                         \ - but not"
                                , SItalic "for"
                                , SPlain "asterisks. But backticks\nwill "
                                , SCode "always produce"
                                , SPlain " code "
                                , SCode "ya know"
                                , SPlain "? And if you need two backticks,\nyou can "
                                , SCode "do `` within three backticks"
                                , SPlain ".\n"
                                , SCode "parseMarkdown :: Parser Markdown\nparseMarkdown = Markdown\
                                        \ <$> parseSegments <* eof\n"
                                , SPlain "\nAnd know that "
                                , SKbd "z"
                                , SKbd "z"
                                , SPlain " centers your cursor location in normal mode.\nAnd make\
                                        \ sure that \"quoted strings\" get their damn html entities\
                                        \ decoded.\n" ]
    it "mimics stackoverflow underscore oddities" $
      markdown Pretty (T.unwords [ "t_what_ _what_t __what_ !_what_ *what_ _what* _what_ _what_"
                                 , "snake_case_words _*asterisk* *_underscore_ and__no__bolding" ])
        `shouldBe` Markdown [ SPlain "t_what_ _what_t __what_ !"
                            , SItalic "what"
                            , SPlain " "
                            , SItalic "what_ _what"
                            , SPlain " "
                            , SItalic "what"
                            , SPlain " "
                            , SItalic "what"
                            , SPlain " snake_case_words _"
                            , SItalic "asterisk"
                            , SPlain " *"
                            , SItalic "underscore"
                            , SPlain " and__no__bolding"
                            ]
    it "doesnt nest inner styles" $
      markdown Pretty (T.unwords [ "t_what_ _what_t __what_ !_what_ *what_ _what* _what_ _what_"
                                 , "snake_case_words _*asterisk* *_underscore_ and__" ])
        `shouldBe` Markdown [ SPlain "t_what_ _what_t "
                            , SBold "what_ !_what_ *what_ _what* _what_ _what_ snake_case_words \
                                    \_*asterisk* *_underscore_ and"
                            ]

allAnswerIds :: [Question Text] -> [Int]
allAnswerIds = concatMap ((fmap _aId) . _qAnswers)

decodeQFromFile :: FilePath -> IO (Either String [Question Text])
decodeQFromFile f = do
  b <- BSL.readFile f
  return $ A.eitherDecode b >>= AT.parseEither questionsParser

decodeCfgFromFile :: FilePath -> IO (Either String AppConfig)
decodeCfgFromFile = fmap Y.decodeEither . BS.readFile

validQuestionsFile2 :: FilePath
validQuestionsFile2 = "test/fixtures/stackexchange/valid_questions_api_response_2.json"

validQuestionsFile30 :: FilePath
validQuestionsFile30 = "test/fixtures/stackexchange/valid_questions_api_response_30.json"

invalidQuestionsFile :: FilePath
invalidQuestionsFile = "test/fixtures/stackexchange/invalid_questions_api_response.json"

siteStringFile :: FilePath
siteStringFile = "test/fixtures/config/site_as_string.yml"

serverfaultFile :: FilePath
serverfaultFile = "test/fixtures/config/serverfault_default.yml"

noSitesFile :: FilePath
noSitesFile = "test/fixtures/config/no_sites.yml"

badUIFile :: FilePath
badUIFile = "test/fixtures/config/bad_ui.yml"

validMarkdown :: IO Text
validMarkdown = TIO.readFile "test/fixtures/markdown/valid.md"

englishMetaBSL :: IO BSL.ByteString
englishMetaBSL = BSL.readFile "test/fixtures/google/english_meta.html"
