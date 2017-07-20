module Interface.Brick
  ( runBrick
  ) where

import Types

data BState = BState
  { aState    :: AppState
  , aConfig   :: AppConfig
  , questions :: [Question]
  }

runBrick :: [Question] -> App ()
runBrick qs = do

app :: App (May

