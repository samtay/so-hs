{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module StackOverflow.Types where

import Control.Lens.TH
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)

data Question = Question
  { _qId      :: Int
  , _qScore   :: Int
  , _qAnswers :: [Answer]
  , _qTitle   :: Text
  , _qBody    :: Text
  } deriving (Show)

data Answer = Answer
  { _aId       :: Int
  , _aScore    :: Int
  , _aBody     :: Text
  , _aAccepted :: Bool
  } deriving (Show)

data Site = Site
  { _sUrl :: Text
  , _sApiParam :: Text
  } deriving (Eq, Show)

makeLenses ''Question
makeLenses ''Answer

instance Eq Question where
  q1 == q2 = _qId q1 == _qId q2

instance Eq Answer where
  a1 == a2 = _aId a1 == _aId a2

instance FromJSON Question where
  parseJSON = withObject "question" $ \o -> do
    _qId      <- o .: "question_id"
    _qScore   <- o .: "score"
    _qAnswers <- o .: "answers"
    _qTitle   <- o .: "title"
    _qBody    <- o .: "body_markdown"
    return Question {..}

instance FromJSON Answer where
  parseJSON = withObject "answer" $ \o -> do
    _aId       <- o .: "answer_id"
    _aScore    <- o .: "score"
    _aBody     <- o .: "body_markdown"
    _aAccepted <- o .: "false"
    return Answer {..}

instance FromJSON Site where
  parseJSON = withObject "site" $ \o -> do
    _sUrl      <- o .: "site_url"
    _sApiParam <- o .: "api_site_parameter"
    return Site {..}

questionsParser :: Value -> Parser [Question]
questionsParser = withObject "questions" (.: "items")
