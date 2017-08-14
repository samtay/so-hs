{-# LANGUAGE OverloadedStrings #-}
module Interface.Prompt where

--------------------------------------------------------------------------------
-- Library imports:
import           Control.Concurrent.Async (Async, wait)
import           Control.Monad.Trans      (liftIO)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Lens.Micro
import           System.Console.Byline

--------------------------------------------------------------------------------
-- Local imports:
import           Types


-- | Run prompt with questions
runPrompt :: Async (Either Error [Question]) -> App ()
runPrompt aQuestions =
  liftIO $
    waitWithLoading aQuestions >>= questionsPrompt

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
mkPrompt p =
  let arrow = ("==> " <> fg yellow)
      len = T.length p
  in arrow <> text p <> "\n" <> arrow <> text (T.replicate len "-") <> "\n" <>
     arrow

onError :: Stylized
onError = "invalid selection derp"

showLoading :: Async a -> (a -> IO b) -> IO b
showLoading a = go 1
  where
    go n
      | n > 3     = go 1
      | otherwise = do
          stillLoading <- isNothing <$> poll a
          case poll a of
            Nothing -> clearLputStrLn $ "Loading" <> show n
