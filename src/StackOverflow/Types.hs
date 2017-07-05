{-# LANGUAGE TemplateHaskell #-}
module StackOverflow.Types where

import Control.Lens.TH
import Data.ByteString.Lazy (ByteString)

data Question = Question
  { _qid      :: Int
  , _qscore   :: Int
  , _qanswers :: [Answer]
  , _qtitle   :: ByteString
  , _qbody    :: ByteString
  } deriving (Show)

data Answer = Answer
  { _aid    :: Int
  , _ascore :: Int
  , _abody  :: ByteString
  } deriving (Show)

makeLenses ''Question
makeLenses ''Answer

instance Eq Question where
  q1 == q2 = _qid q1 == _qid q2

instance Eq Answer where
  a1 == a2 = _aid a1 == _aid a2
