module Interface.Brick
  ( runBrick
  ) where

import           Control.Monad.Trans (liftIO)

import           StackOverflow
import           Types

data BState = BState
  { aState    :: AppState
  , aConfig   :: AppConfig
  , questions :: [Question]
  }

runBrick :: App ()
runBrick = query >>= liftIO . print
  -- two empty MVars
  --    one for querying and retrieving questions
  --    one for --verbose logging window (maybe AppState has logger mvar?)
  -- when query is done, need to tell BChan to retrieve it...
  --    OR questions sent directly to BChan
