{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE LambdaCase #-}
module Interface.Prompt
  ( execPrompt
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import Data.Maybe (listToMaybe)
import Data.List (elemIndex)
import           Control.Concurrent       (forkIO, killThread, threadDelay)
import           Control.Monad            (forM_, void)
import           System.Exit              (exitSuccess)
import           System.IO                (stdout)

--------------------------------------------------------------------------------
-- Library imports:
import           Control.Concurrent.Async (Async, wait)
import           Control.Monad.State      (StateT, get, gets, modify, put,
                                           runStateT, MonadState)
import           Control.Monad.Trans      (lift, liftIO, MonadIO)
import           Data.Default
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Lens.Micro               (to, (&), (.~), (^.))
import           Lens.Micro.TH            (makeLenses)
import qualified System.Console.ANSI      as A
import           System.Console.Byline

--------------------------------------------------------------------------------
-- Local imports:
import           Types
import           Utils

--------------------------------------------------------------------------------
-- Types

data PromptState = PromptState
  { _pQuestions :: [Question]
  , _pCurrQ     :: Maybe (Int, Question) -- TODO remove (Int,) indices, just have inc/dec funcs
  , _pCurrA     :: Maybe (Int, Answer)
  , _pMenu      :: PromptMenu
  }

type PromptApp = StateT PromptState IO

type Op = PromptState -> IO PromptState

data Command = Command
  { _cKey  :: Char
  , _cOp   :: Op
  , _cHelp :: Text
  }

data PromptMenu = PromptMenu
  { _mCommands   :: [Command]
  , _mPromptText :: Text
  }

makeLenses ''PromptState
makeLenses ''PromptMenu

instance Default PromptMenu where
  def = PromptMenu
    { _mCommands   = [ Command 'b' back "Go back"
                     , Command 'j' next "Next selection"
                     , Command 'k' previous "Previous selection"
                     , Command 'q' quit "Quit"
                     , Command '?' help "Show this help"
                     ]
    , _mPromptText = ""
    }

--------------------------------------------------------------------------------
-- Main execution

-- | Run prompt with questions
execPrompt :: Async (Either Error [Question]) -> App ()
execPrompt aQuestions = liftIO $
  waitWithLoading aQuestions
    >>= exitOnError runner
  where
    runner qs =
      if null qs
        then exitWithError "No results found. Try a different question."
        else void $ runStateT (runByline runPrompt) (initPromptState qs)

waitWithLoading :: Async a -> IO a
waitWithLoading a = do
  loadingThreadId <- forkIO showLoadingAnimation
  res             <- wait a
  killThread loadingThreadId
  A.clearLine
  A.setCursorColumn 0
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
    loadingPrefix :: Text
    loadingPrefix = "Loading"
    go :: Int -> IO ()
    go n
      | n < 7 = do
        threadDelay 200000
        TIO.putStr "."
        go (n + 1)
      | otherwise = do
        A.setCursorColumn (T.length loadingPrefix)
        A.clearFromCursorToLineEnd
        go 1

--------------------------------------------------------------------------------
-- Prompt functions

initPromptState :: [Question] -> PromptState
initPromptState qs = PromptState qs Nothing Nothing def

runPrompt :: Byline PromptApp ()
runPrompt =
  lift get >>= \case
    (PromptState qs Nothing Nothing _)     -> questionsPrompt qs
    (PromptState _ (Just (_,q)) Nothing _) -> answersPrompt q
    (PromptState _ _ (Just (_,a)) _)       -> answerPrompt a

questionsPrompt :: [Question] -> Byline PromptApp ()
questionsPrompt qs = do
  lift $ modify (pMenu . mPromptText .~  "Enter n° of question to view")
  runMenu
    (questionsMenu qs)
    (\q -> lift $ modify (pCurrQ .~ ((, q) <$> elemIndex q qs)))

answersPrompt :: Question -> Byline PromptApp ()
answersPrompt q = do
  let answers = q ^. qAnswers
  liftIO $ TIO.putStrLn $ "\n" <> q ^. qBody <> "\n"
  lift $ modify (pMenu . mPromptText .~  "Enter n° of answer to view")
  runMenu
    (answersMenu answers)
    (\a -> lift $ modify (pCurrA .~ ((, a) <$> elemIndex a answers)))

answerPrompt :: Answer -> Byline PromptApp ()
answerPrompt a = do
  liftIO $ TIO.putStrLn $ "\n" <> a ^. aBody <> "\n"
  lift $ modify (pMenu . mPromptText .~  "")
  -- empty menu, kind of a type system sham :/
  runMenu
    (allowNoItems True $ defaultFirst False $ mkMenu (const "") [])
    (const $ return ())

runMenu
  :: Menu a                      -- Byline Menu
  -> (a -> Byline PromptApp ())  -- Action on match
  -> Byline PromptApp ()
runMenu bylineMenu action = do
  liftIO $ putStrLn ""
  prompt <- mkPrompt <$> lift (gets _pMenu)
  loop bylineMenu prompt
  where
    loop bMenu prompt = do
      choice <- askWithMenu bMenu prompt
      case choice of
        Match q -> action q >> runPrompt
        Other t -> do
          cs <- lift $ gets (_mCommands . _pMenu)
          let mOp = findOpByKey t cs
          case mOp of
            Nothing -> do
              loop (beforePrompt (onInvalid t) bMenu) prompt
            Just op -> do
              lift $ modifyIO op
              runPrompt
        NoItems -> error "This shouldn't happen!"

findOpByKey :: Text -> [Command] -> Maybe Op
findOpByKey k = fmap _cOp . listToMaybe . filter ((==k) . T.singleton . _cKey)

questionsMenu :: [Question] -> Menu Question
questionsMenu = mkMenu styleQ
  where
    styleQ q = score (q ^. qScore) <> " " <> text (q ^. qTitle)

answersMenu :: [Answer] -> Menu Answer
answersMenu = mkMenu styleA
  where
    styleA a =
      score (a ^. aScore)
      <> (if a ^. aAccepted then check else " ")
      <> text (answerTitle a)

mkMenu :: (a -> Stylized) -> [a] -> Menu a
mkMenu stylizer xs =
  suffix " " $
  menu xs stylizer

check :: Stylized
check = fg green <> " ✔ "

answerTitle :: Answer -> Text
answerTitle a = T.replace "\n" ".. " $ T.take 60 $ a ^. aBody

score :: Int -> Stylized
score n =
  let num     = text . T.pack . show $ n
      fgColor = if n > 0 then green else red
  in bold <> fg fgColor <> "(" <>  num <> ")"

mkPrompt :: PromptMenu -> Stylized
mkPrompt pmenu =
  arrow <> text pTxt <> text cTxt <> "\n" <>
  arrow <> text (T.replicate len "-") <> "\n" <>
  arrow
  where
    pTxt        = pmenu ^. mPromptText
    cTxt        = if T.null pTxt then "Enter action " <> cTxtOnly else cTxtSuffix
    cTxtSuffix  = " (or " <> cTxtOnly <> ")"
    cTxtOnly    = "[" <> commandText (pmenu ^. mCommands) <> "]"
    arrow       = ("==> " <> fg yellow)
    len         = T.length pTxt + T.length cTxt
    commandText = T.intercalate "," . map (T.singleton . _cKey)

onInvalid :: Text -> Stylized
onInvalid t = "invalid selection: " <> text t

--------------------------------------------------------------------------------
-- Menu commands

help :: Op
help pstate = do
  putStrLn ""
  A.setSGR [ A.SetColor A.Foreground A.Dull A.Red
           , A.SetConsoleIntensity A.BoldIntensity
           ]
  forM_ (pstate ^. pMenu ^. mCommands) $ \(Command k _ helpText) -> do
    TIO.putStrLn $ T.singleton k <> " - " <> helpText
  A.setSGR []
  return pstate

move :: (Int -> Int -> Int) -> Op
move (+/-) ps = return $
  case (ps ^. pCurrQ) of
    Nothing ->
      ps & pCurrQ .~ Just (1, ps ^. pQuestions ^. to head)
    Just (qIx, currQ) ->
      case (ps ^. pCurrA) of
        Nothing ->
          let ix = nextQIx qIx
          in ps & pCurrQ .~ Just (ix, (ps ^. pQuestions) !! ix)
        Just (aIx, _) ->
          let ix = nextAIx currQ aIx
          in ps & pCurrA .~ Just (ix, (currQ ^. qAnswers) !! ix)
  where
    nextAIx :: Question -> Int -> Int
    nextAIx q currIx = (currIx +/- 1) `mod` length (q ^. qAnswers)

    nextQIx :: Int -> Int
    nextQIx currIx = (currIx +/- 1) `mod` size ps

next :: Op
next = move (+)

previous :: Op
previous = move (-)

back :: Op
back ps = return $
  case ps ^. pCurrA of
    Just _ ->
      ps & pCurrA .~ Nothing
    Nothing ->
      case ps ^. pCurrQ of
        Just _ ->
          ps & pCurrQ .~ Nothing
        Nothing ->
          ps

quit :: Op
quit = const exitSuccess

size :: PromptState -> Int
size pstate = pstate ^. pQuestions ^. to length

--------------------------------------------------------------------------------
-- Utility functions

-- | A variant of 'modify' that allows resulting state returned within IO
modifyIO :: (MonadState s m, MonadIO m) => (s -> IO s) -> m ()
modifyIO modifyOp = do
  s <- get
  newS <- liftIO $ modifyOp s
  put newS
