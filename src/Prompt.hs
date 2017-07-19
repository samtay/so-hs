{-# LANGUAGE OverloadedStrings #-}
module Prompt where

import           Control.Monad.Trans   (liftIO)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Lens.Micro
import           System.Console.Byline

import           Types


-- | Show single answer, return whether or not to run full interface
runLuckyPrompt :: App Bool
runLuckyPrompt = undefined

-- | Run prompt with questions
runPrompt :: [Question] -> App (Maybe ())
runPrompt qs =
  if null qs
     then runByline noResultsPrompt
     else runByline (questionsPrompt qs)

questionsPrompt :: [Question] -> Byline App ()
questionsPrompt qs = do
  let prompt = mkPrompt "Enter n° of question to view"
  q <- askWithMenuRepeatedly (questionsMenu qs) prompt onError
  liftIO $ putStrLn $ "you picked: " ++ show q

questionsMenu :: [Question] -> Menu Question
questionsMenu = opts . (`menu` styleQuestion)
  where
    opts            = suffix " "
    styleQuestion q = styleScore (q ^. qScoreL) <> " " <> text (q ^. qTitleL)
    styleScore n    =
      ("(" <> (text . T.pack . show) n <> ")")
        <> bold <> fg (if n > 0 then green else red)

noResultsPrompt :: Byline App ()
noResultsPrompt = undefined

mkPrompt :: Text -> Stylized
mkPrompt p = let arrow = ("==> " <> fg yellow)
                 len   = T.length p
              in arrow <> text p <> "\n"
              <> arrow <> text (T.replicate len "-") <> "\n"
              <> arrow

onError :: Stylized
onError = "invalid selection derp"
