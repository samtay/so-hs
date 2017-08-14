{-# LANGUAGE OverloadedStrings #-}
module Interface.Prompt
  ( runPrompt
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Concurrent       (forkIO, killThread, threadDelay)
import           Control.Monad            (void)
import           System.IO                (stdout)

--------------------------------------------------------------------------------
-- Library imports:
import           Control.Concurrent.Async (Async, wait)
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
waitWithLoading a = do
  loadingThreadId <- forkIO showLoadingAnimation
  res <- wait a
  killThread loadingThreadId
  A.clearLine
  A.showCursor
  return res

showLoadingAnimation :: IO ()
showLoadingAnimation =
  noBufferOn stdout $ do
    A.hideCursor
    A.clearLine
    TIO.putStr loadingPrefix
    go 1
  where
    loadingPrefix = "Loading"
    go n
      | n < 7 = do
        threadDelay 200000
        TIO.putStr "."
        go (n + 1)
      | otherwise = do
        A.setCursorColumn (T.length loadingPrefix)
        A.clearFromCursorToLineEnd
        go 1
