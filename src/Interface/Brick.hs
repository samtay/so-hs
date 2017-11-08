{-# LANGUAGE LambdaCase        #-}
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
import           Control.Concurrent.Async (Async, async, wait)
import           Control.Monad.Reader     (ask)
import           Control.Monad.State      (get)
import           Control.Monad.Trans      (liftIO)
import qualified Data.Text                as T
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
  = NewQueryResult (Either Error [Question Markdown])
  | TimeTick

-- | Fetcher provides a way to communicate with a service to
-- fetch new questions and asynchronously pipe them to the brick event channel
data Fetcher = Fetcher { _fChan  :: BChan BEvent }

-- | TODO add a bunch of stuff to this.. editor context, etc..
-- Remember I might need turtle to pipe stuff to copy-paste command
-- Also possibly add to util backup defaults, check if pbcopy, xclip, etc. is in PATH.
data BState = BState
  { _bQuestions  :: [Question Markdown]
  , _bError      :: Maybe Error
  , _bLoading    :: Maybe Int
  , _bShowSplash :: Bool
  , _bShowHelp   :: Bool
  , _bAppState   :: AppState
  , _bAppConfig  :: AppConfig
  , _bFetcher    :: Fetcher
  }
makeLenses ''BState

-- | Resource names
data Name
  = QuestionList
  | QuestionView
  | AnswerList
  | AnswerView
  deriving (Eq, Ord)

--------------------------------------------------------------------------------
-- Execution

execBrick :: Async (Either Error [Question Markdown]) -> App ()
execBrick aQuestions = do
  state <- get
  conf  <- ask
  _ <- liftIO $ do
    chan  <- newBChan 10
    passToChannel aQuestions chan
    startTimeTicker chan
    let initialBState = BState { _bQuestions  = []
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
-- TODO might make sense to have this within transformer and update appstate
-- to show new loading symbol
fetch :: Fetcher -> AppConfig -> AppState -> IO ()
fetch (Fetcher chan) config state = do
  aQuestions <- async $ evalAppT config state query
  passToChannel aQuestions chan

-- | Fork a process that will wait for async result and pass to BChan
passToChannel :: Async (Either Error [Question Markdown]) -> BChan BEvent -> IO ()
passToChannel aQuestions chan = void . forkIO $ do
  qResult <- wait aQuestions
  writeBChan chan $ NewQueryResult qResult

--------------------------------------------------------------------------------
-- Event Handling

handleEvent :: BState -> BrickEvent Name BEvent -> EventM Name (Next BState)
handleEvent bs = \case
  AppEvent TimeTick                   -> continue $ bs & bLoading %~ fmap (+. 1)
  VtyEvent (V.EvKey (V.KChar '?') []) -> continue $ bs & bShowHelp .~ True
  VtyEvent (V.EvKey V.KEsc        []) -> continue $ bs & bShowHelp .~ False
  VtyEvent (V.EvKey (V.KChar 'q') []) -> halt bs
  _                                   -> continue bs

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
                (Just n, True)  -> C.centerLayer $ splashWidget <=> {-padLeft (Pad 1)-} (drawLoading n)
                _               -> emptyWidget
            , maybe emptyWidget drawError $ bs ^. bError
            , drawQAPanes bs
            ]

helpWidget :: Widget Name
helpWidget = C.centerLayer . B.borderWithLabel (txt "Help") $
  txt "Help not yet implemented"

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

drawError :: Error -> Widget Name
drawError = const emptyWidget

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

drawQAPanes :: BState -> Widget Name
drawQAPanes = const emptyWidget

--------------------------------------------------------------------------------
-- Styling

theMap :: AttrMap
theMap = attrMap V.defAttr []

--------------------------------------------------------------------------------
-- Utilities

-- | Addition modulo loading dots
(+.) :: Int -> Int -> Int
n +. m = (n + m) `mod` loadingDotCount
