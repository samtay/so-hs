module Main where

import Control.Monad ((<=<))

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BS
import Test.Hspec

import StackOverflow.Types
import Utils

main :: IO ()
main = hspec $ do

  describe "Google Scraper" $ do
    it "parses english.meta question links" $
      pending
    it "fails gracefully on botched attempt" $
      pending

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

  describe "Configuration" $ do
    it "handles bad yaml configuration" $
      pending
    it "reads site configuration properly" $
      pending
    it "always allows stackoverflow search" $
      pending
    it "handles bad custom ui command gracefully" $
      pending

allAnswerIds :: [Question] -> [Int]
allAnswerIds = concatMap ((fmap aId) . qAnswers)

decodeQFromFile :: FilePath -> IO (Either String [Question])
decodeQFromFile f = do
  b <- BS.readFile f
  return $ A.eitherDecode b >>= AT.parseEither questionsParser

validQuestionsFile2 :: FilePath
validQuestionsFile2 = "test/fixtures/valid_questions_api_response_2.json"

validQuestionsFile30 :: FilePath
validQuestionsFile30 = "test/fixtures/valid_questions_api_response_30.json"

invalidQuestionsFile :: FilePath
invalidQuestionsFile = "test/fixtures/invalid_questions_api_response.json"
