{-# LANGUAGE UndecidableInstances #-}
module Types.StackOverflow where

--------------------------------------------------------------------------------
-- Base imports:
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
--------------------------------------------------------------------------------
-- Library imports:
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default
import Data.Text (Text)
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)

--------------------------------------------------------------------------------
-- Local imports:
import Utils

type Question = Question' NonEmpty

data Question' t a = Question
  { _qId      :: Int
  , _qScore   :: Int
  , _qAnswers :: t (Answer a)
  , _qTitle   :: Text
  , _qBody    :: a
  } deriving (Functor)

data Answer a = Answer
  { _aId       :: Int
  , _aScore    :: Int
  , _aBody     :: a
  , _aAccepted :: Bool
  } deriving (Show, Functor)

data Site = Site
  { _sUrl      :: Text
  , _sApiParam :: Text
  } deriving (Eq, Show)

makeLenses ''Question'
makeLenses ''Answer
makeLenses ''Site

deriving instance (Show a, Show (t (Answer a))) => Show (Question' t a)

instance Eq (Question' t a) where
  q1 == q2
    = q1 ^. qId == q2 ^. qId

instance Eq (Answer a) where
  a1 == a2
    = a1 ^. aId == a2 ^. aId

instance FromJSON (Question' [] Text) where
  parseJSON = withObject "question" $ \o -> do
    _qId      <- o .: "question_id"
    _qScore   <- o .: "score"
    _qAnswers <- o .:? "answers" .!= []
    _qTitle   <- o .: "title"
    _qBody    <- o .: "body_markdown"
    return Question {..}

instance FromJSON (Answer Text) where
  parseJSON = withObject "answer" $ \o -> do
    _aId       <- o .: "answer_id"
    _aScore    <- o .: "score"
    _aBody     <- o .: "body_markdown"
    _aAccepted <- o .: "is_accepted"
    return Answer {..}

instance FromJSON Site where
  parseJSON = withObject "site" $ \o -> do
    _sUrl      <- o .: "site_url"
    _sApiParam <- o .: "api_site_parameter"
    return Site {..}

instance Default Site where
  def = Site
    { _sUrl = "https://stackoverflow.com"
    , _sApiParam = "stackoverflow"
    }

-- TODO handle API errors; maybe data ApiResponse = Error | [Question]
--    which works if Parser is instance of Alternative
questionsParser :: Value -> Parser [Question Text]
questionsParser = mapMaybe answered <$$> withObject "questions" (.: "items")
  where
    answered q = do
      ans <- NE.nonEmpty $ q ^. qAnswers
      pure $ q & qAnswers .~ ans
