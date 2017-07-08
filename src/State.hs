module State
  ( SO(..)
  ) where

import Data.Text (Text)

import Config
import StackOverflow.Types

data SO = SO
  { soQuery     :: Text
  , soQuestions :: [Question]
  , soOptions   :: Options
  } deriving (Show)
