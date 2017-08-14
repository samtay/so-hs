{-# LANGUAGE OverloadedStrings #-}
module Interface.Prompt
  ( runPrompt
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Concurrent       (threadDelay)
import           Control.Exception        (throwIO)
import           Control.Monad            (void)

--------------------------------------------------------------------------------
-- Library imports:
import           Control.Concurrent.Async (Async, poll)
import           Control.Monad.Trans      (liftIO)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Lens.Micro               ((^.))
import qualified System.Console.ANSI      as A
import           System.Console.Byline
--------------------------------------------------------------------------------
-- Local imports:
import           Types
import           Utils


-- | Run prompt with questions
runPrompt :: Async (Either Error [Question]) -> App ()
runPrompt aQuestions = liftIO $
  waitWithLoading aQuestions >>= exitOnError (void . runByline . questionsPrompt)

questionsPrompt :: [Question] -> Byline IO ()
questionsPrompt qs = do
  let prompt = mkPrompt "Enter n° of question to view"
  q <- askWithMenuRepeatedly (questionsMenu qs) prompt onError
  liftIO $ putStrLn $ "you picked: " ++ show q

questionsMenu :: [Question] -> Menu Question
questionsMenu = opts . (`menu` styleQuestion)
  where
    opts            = suffix " "
    styleQuestion q = styleScore (q ^. qScore) <> " " <> text (q ^. qTitle)
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

waitWithLoading :: Async a -> IO a
waitWithLoading a = go 1
  where
    go n
      | n > 3     = go 1
      | otherwise = do
          clearLine
          currentA <- poll a
          case currentA of
            Nothing -> putStrLn "Loading" <> replicate n '.'
            Left e  -> throw e
            Right r -> return r
