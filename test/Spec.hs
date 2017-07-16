{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Aeson           as A
import qualified Data.Aeson.Types     as AT
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Yaml            as Y
import           Test.Hspec

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
      sApiParam . oSite . cDefaultOpts <$$> decodeCfgFromFile serverfaultFile
        `shouldReturn` Right "serverfault"
    it "always allows stackoverflow search" $
      sApiParam <$$> cSites <$$> decodeCfgFromFile noSitesFile
        `shouldReturn` Right ["stackoverflow"]
    it "handles bad custom ui command gracefully" $
      decodeCfgFromFile badUIFile
        `shouldReturn` Left "Error in $.defaultOptions.ui: invalid interface"

allAnswerIds :: [Question] -> [Int]
allAnswerIds = concatMap ((fmap aId) . qAnswers)

decodeQFromFile :: FilePath -> IO (Either String [Question])
decodeQFromFile f = do
  b <- BSL.readFile f
  return $ A.eitherDecode b >>= AT.parseEither questionsParser

decodeCfgFromFile :: FilePath -> IO (Either String Config)
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

englishMetaBSL :: IO BSL.ByteString
englishMetaBSL = BSL.readFile "test/fixtures/google/english_meta.html"
