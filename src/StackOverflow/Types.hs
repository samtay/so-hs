{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module StackOverflow.Types where

import Control.Lens (view)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)

import Utils

data Question = Question
  { qId      :: Int
  , qScore   :: Int
  , qAnswers :: [Answer]
  , qTitle   :: Text
  , qBody    :: Text
  } deriving (Show)

data Answer = Answer
  { aId       :: Int
  , aScore    :: Int
  , aBody     :: Text
  , aAccepted :: Bool
  } deriving (Show)

data Site = Site
  { sUrl :: Text
  , sApiParam :: Text
  } deriving (Eq, Show)

suffixLenses ''Question
suffixLenses ''Answer

instance Eq Question where
  q1 == q2 = qId q1 == qId q2

instance Eq Answer where
  a1 == a2 = aId a1 == aId a2

instance FromJSON Question where
  parseJSON = withObject "question" $ \o -> do
    qId      <- o .: "question_id"
    qScore   <- o .: "score"
    qAnswers <- o .:? "answers" .!= []
    qTitle   <- o .: "title"
    qBody    <- o .: "body_markdown"
    return Question {..}

instance FromJSON Answer where
  parseJSON = withObject "answer" $ \o -> do
    aId       <- o .: "answer_id"
    aScore    <- o .: "score"
    aBody     <- o .: "body_markdown"
    aAccepted <- o .: "is_accepted"
    return Answer {..}

instance FromJSON Site where
  parseJSON = withObject "site" $ \o -> do
    sUrl      <- o .: "site_url"
    sApiParam <- o .: "api_site_parameter"
    return Site {..}

questionsParser :: Value -> Parser [Question]
questionsParser = filter answered <$$> withObject "questions" (.: "items")
  where answered = not . null . qAnswers
