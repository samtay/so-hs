{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Interface.Brick
  ( execBrick
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Monad            (forever, void)
import           Data.Monoid              ((<>))

--------------------------------------------------------------------------------
-- Library imports:
import           Brick                    hiding (App)
import qualified Brick
import           Brick.BChan              (BChan, newBChan, writeBChan)
import qualified Brick.Widgets.Border     as B
import qualified Brick.Widgets.Center     as C
import           Brick.Widgets.List       (List)
import qualified Brick.Widgets.List       as L
import           Control.Concurrent.Async (Async, async, wait)
import           Control.Monad.Reader     (ask)
import           Control.Monad.State      (get)
import           Control.Monad.Trans      (liftIO)
import qualified Data.Text                as T
import           Data.Vector              (fromList)
import qualified Graphics.Vty             as V
import           Lens.Micro               ((%~), (&), (.~), (^.))
import           Lens.Micro.TH            (makeLenses)
import           Text.RawString.QQ        (r)

--------------------------------------------------------------------------------
-- Local imports:
import           Markdown
import           StackOverflow
import           Types

--------------------------------------------------------------------------------
-- Types

-- | Events that we pipe to the event handler asynchronously
data BEvent
  = NewQueryResult (Either Error [Question [] Markdown])
  | TimeTick

data BError
  = AppError Error
  | NoResults
  deriving (Eq)

-- | Fetcher provides a way to communicate with a service to
-- fetch new questions and asynchronously pipe them to the brick event channel
data Fetcher = Fetcher { _fChan  :: BChan BEvent }

-- | Resource names
data Name
  = QuestionList
  | QuestionView
  | AnswerList
  | AnswerView
  deriving (Eq, Ord, Show)

data ShiftDirection
  = North
  | East
  | South
  | West
  deriving (Eq)

-- | TODO add a bunch of stuff to this.. editor context, etc..
-- Remember I might need turtle to pipe stuff to copy-paste command
-- Also possibly add to util backup defaults, check if pbcopy, xclip, etc. is in PATH.
data BState = BState
  { _bQuestions  :: List Name (Question (List Name) Markdown)
  , _bError      :: Maybe BError
  , _bLoading    :: Maybe Int
  , _bShowSplash :: Bool
  , _bShowHelp   :: Bool
  , _bAppState   :: AppState
  , _bAppConfig  :: AppConfig
  , _bFetcher    :: Fetcher
  }
makeLenses ''BState

--------------------------------------------------------------------------------
-- Execution

execBrick :: Async (Either Error [Question [] Markdown]) -> App ()
execBrick aQuestions = do
  state <- get
  conf  <- ask
  _ <- liftIO $ do
    chan  <- newBChan 10
    passToChannel aQuestions chan
    startTimeTicker chan
    -- TODO perhaps extents can help keep track of list & viewport heights ??
    let initialBState = BState { _bQuestions  = L.list QuestionList [] 10
                               , _bError      = Nothing
                               , _bLoading    = Just 0
                               , _bShowSplash = True
                               , _bShowHelp   = False
                               , _bAppState   = state
                               , _bAppConfig  = conf
                               , _bFetcher    = Fetcher chan
                               }
    customMain (V.mkVty V.defaultConfig) (Just chan) app initialBState
  return () -- TODO figure out end game

startTimeTicker :: BChan BEvent -> IO ()
startTimeTicker chan = void . forkIO . forever $ do
  writeBChan chan TimeTick
  threadDelay 100000

--------------------------------------------------------------------------------
-- App Definition

app :: Brick.App BState BEvent Name
app = Brick.App { appDraw         = drawUI
                , appChooseCursor = neverShowCursor
                , appHandleEvent  = handleEvent
                , appStartEvent   = return
                , appAttrMap      = const theMap
                }

--------------------------------------------------------------------------------
-- Fetcher

-- add --verbose logging window (maybe AppState has logger mvar?)

-- | Fetch new questions
fetch :: Fetcher -> AppConfig -> AppState -> IO ()
fetch (Fetcher chan) config state = do
  aQuestions <- async $ evalAppT config state query
  passToChannel aQuestions chan

-- | Fork a process that will wait for async result and pass to BChan
passToChannel :: Async (Either Error [Question [] Markdown]) -> BChan BEvent -> IO ()
passToChannel aQuestions chan = void . forkIO $ do
  qResult <- wait aQuestions
  writeBChan chan . NewQueryResult $ qResult
  where

--------------------------------------------------------------------------------
-- Event Handling

handleEvent :: BState -> BrickEvent Name BEvent -> EventM Name (Next BState)
handleEvent bs = \case
  AppEvent TimeTick                    -> continue $ bs & bLoading %~ fmap (+. 1)
  AppEvent (NewQueryResult (Left e))   -> continue $ bs & bError .~ Just (AppError e)
                                                        & fetched
  AppEvent (NewQueryResult (Right qs)) -> continue $ bs & replaceQAs qs
                                                        & fetched
  VtyEvent (V.EvKey (V.KChar 'k') []) -> continue bs -- TODO scroll
  VtyEvent (V.EvKey (V.KChar 'l') []) -> continue bs -- TODO scroll
  VtyEvent (V.EvKey (V.KChar 'j') []) -> continue bs -- TODO scroll
  VtyEvent (V.EvKey (V.KChar 'h') []) -> continue bs -- TODO scroll
  VtyEvent (V.EvKey (V.KChar 'k') [V.MCtrl]) -> adjustView (Just North) bs
  VtyEvent (V.EvKey (V.KChar 'l') [V.MCtrl]) -> adjustView (Just East) bs
  VtyEvent (V.EvKey (V.KChar 'j') [V.MCtrl]) -> adjustView (Just South) bs
  VtyEvent (V.EvKey (V.KChar 'h') [V.MCtrl]) -> adjustView (Just West) bs
  VtyEvent (V.EvResize _ _)                  -> adjustView Nothing bs
  VtyEvent (V.EvKey (V.KChar '?') []) -> continue $ bs & bShowHelp .~ True
  VtyEvent (V.EvKey V.KEsc        []) -> continue $ bs & bShowHelp .~ False
                                                       & bError .~ Nothing
  VtyEvent (V.EvKey (V.KChar 'q') []) -> halt bs
  _                                   -> continue bs
  where
    fetched :: BState -> BState
    fetched = (bLoading .~ Nothing) .  (bShowSplash .~ False)
    replaceQAs :: [Question [] Markdown] -> BState -> BState
    replaceQAs [] bs' = bs' & bError .~ Just NoResults
    replaceQAs qs bs' =
      let height = bs' ^. bQuestions ^. L.listItemHeightL
          qs' = fmap (qAnswers %~ (\as -> L.list AnswerList (fromList as) height)) qs
       in bs' & bError .~ Nothing
              & bQuestions %~ L.listReplace (fromList qs') (Just 0)

    -- TODO see if resize w,h is helpful here
    -- TODO implement
    adjustView :: Maybe ShiftDirection -> BState -> EventM Name (Next BState)
    adjustView _ = continue

--------------------------------------------------------------------------------
-- Drawing

-- TODO decide whether empty panes displayed in background on startup
-- if not, replace centerLayer with center
drawUI :: BState -> [Widget Name]
drawUI bs = [ case bs ^. bShowHelp of
                True  -> helpWidget
                False -> emptyWidget
            , case (bs ^. bLoading, bs ^. bShowSplash) of
                (Just n, False) -> C.centerLayer . B.border $ drawLoading n
                (Just n, True)  -> C.centerLayer $ splashWidget <=> drawLoading n
                _               -> emptyWidget
            , case bs ^. bError of
                Just e -> drawError e
                _      -> emptyWidget
            , drawQAPanes (bs ^. bQuestions)
            ]

helpWidget :: Widget Name
helpWidget = C.centerLayer . B.borderWithLabel (txt "Help") $
  txt "Help not yet implemented"

-- TODO make this cooler
drawLoading :: Int -> Widget Name
drawLoading n = txt .
  T.justifyLeft totalSize '.' $ T.replicate leading "." <> loadingString
  where
    leading = if n <= halfCount then n else loadingDotCount - n
    halfCount = loadingDotCount `div` 2
    loadingString = "Loading"
    totalSize = halfCount + T.length loadingString

loadingDotCount :: Int
loadingDotCount = 48

-- TODO need some attributes in this bitch
drawError :: BError -> Widget Name
drawError = C.centerLayer . B.border . txt . \case
  NoResults                      -> "No results found. Try another query."
  AppError ConnectionFailure     -> "Connection failure: are you connected to the internet?"
  AppError ScrapingError         -> "Error scraping Google. Try so --no-google."
  AppError (JSONError errMsg)    -> "Error parsing StackOverflow API:\n\n" <> errMsg
  AppError (UnknownError errMsg) -> "Unknown error:\n\n" <> errMsg
  _                              -> "Unknown error"

splashWidget :: Widget Name
splashWidget = padAll 1 . txt $ [r|
      ___           ___
     /\  \         /\  \
    /::\  \       /::\  \
   /:/\ \  \     /:/\:\  \
  _\:\~\ \  \   /:/  \:\  \
 /\ \:\ \ \__\ /:/__/ \:\__\
 \:\ \:\ \/__/ \:\  \ /:/  /
  \:\ \:\__\    \:\  /:/  /
   \:\/:/  /     \:\/:/  /
    \::/  /       \::/  /
     \/__/         \/__/
|]

drawQAPanes
  :: List Name (Question (List Name) Markdown)
  -> Widget Name
drawQAPanes _ = emptyWidget

--------------------------------------------------------------------------------
-- Styling

theMap :: AttrMap
theMap = attrMap V.defAttr []

--------------------------------------------------------------------------------
-- Utilities

-- | Addition modulo loading dots
(+.) :: Int -> Int -> Int
n +. m = (n + m) `mod` loadingDotCount
