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
  runCommandPrompt

-- | Similar to Byline's askWithMenu, except it allows
-- the current commands within PromptApp menu state
-- to be matched and executed. Also, in the case of a match
-- of type 'a' from the byline menu, instead of returning 'a',
-- a handler for modifying state is passed as the second argument.
runMenu
  :: Menu a                      -- Byline Menu
  -> (a -> Byline PromptApp ())  -- Action on match
  -> Byline PromptApp ()
runMenu bylineMenu action = do
  liftIO $ putStrLn ""
  prompt <- menuToPrompt <$> lift (gets _pMenu)
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

-- | This is like 'runMenu' but when there are no items, hence the only
-- available selections are the commands.
runCommandPrompt :: Byline PromptApp ()
runCommandPrompt = do
  cmds <- lift (gets (_mCommands . _pMenu))
  opKey <- askUntil
    (mkPrompt $ "Next action: " <> commandsToText cmds)
    Nothing
    (confirmer cmds)
  case findOpByKey opKey cmds of
    Nothing -> runCommandPrompt
    Just op -> do
      lift $ modifyIO op
      runPrompt
  where
    confirmer cmds input = return $
      case findOpByKey input cmds of
        Nothing -> Left $ onInvalid input
        Just _  -> Right input

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
answerTitle a = (<> "...") $ T.replace "\r\n" ".. " $ T.take 65 $ a ^. aBody

score :: Int -> Stylized
score n =
  let num     = text . T.pack . show $ n
      fgColor = if n > 0 then green else red
  in bold <> fg fgColor <> surround "(" num ")"

menuToPrompt :: PromptMenu -> Stylized
menuToPrompt pmenu = mkPrompt (pTxt <> cTxtSuffix)
  where
    pTxt       = pmenu ^. mPromptText
    cTxtSuffix = surround " (or " cTxtOnly ")"
    cTxtOnly   = commandsToText (pmenu ^. mCommands)

mkPrompt :: Text -> Stylized
mkPrompt promptText =
  arrow <> text promptText <> "\n" <>
  arrow <> text (T.replicate (T.length promptText) "-") <> "\n" <>
  arrow <> text (T.pack (A.setSGRCode []))

commandsToText :: [Command] -> Text
commandsToText = ("[" <>) . (<> "]") . T.intercalate "," . map (T.singleton . _cKey)

onInvalid :: Text -> Stylized
onInvalid t = "invalid selection: " <> text t

-- | Using direct rgb because of bug in byline
-- ref: https://github.com/pjones/byline/issues/1
arrow :: Stylized
arrow = ("==> " <> fg (rgb 255 255 0))

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

surround :: (Monoid m) => m -> m -> m -> m
surround l center r = l <> center <> r
