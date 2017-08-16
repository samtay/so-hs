module Interface.Brick
  ( execBrick
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Concurrent       (forkIO)
import           Control.Monad            (void)

--------------------------------------------------------------------------------
-- Library imports:
import           Brick                    hiding (App)
import qualified Brick
import           Brick.BChan              (BChan, newBChan, writeBChan)
import           Control.Concurrent.Async (Async, async, wait)
import           Control.Monad.Reader     (ask)
import           Control.Monad.State      (get)
import           Control.Monad.Trans      (liftIO)
import           Data.Bifunctor           (first)
import qualified Graphics.Vty             as V

--------------------------------------------------------------------------------
-- Local imports:
import           StackOverflow
import           Types


--------------------------------------------------------------------------------
-- Types

-- Aside from app level errors,
-- we need to represent async fetching of questions in progress
data BError = StillInProgress | AppError Error

-- | TODO add a bunch of stuff to this.. editor context, etc..
-- Remember I might need turtle to pipe stuff to copy-paste command
-- Also possibly add to util backup defaults, check if pbcopy, xclip, etc. is in PATH.
data BState = BState
  { _bAppState  :: AppState
  , _bAppConfig :: AppConfig
  , _bQuestions :: Either BError [Question]
  , _bFetcher   :: Fetcher
  }

-- | Events that we pipe to the event handler asynchronously
data BEvent = NewQueryResult (Either Error [Question])

-- | Resource names
type Name = ()

-- | Fetcher provides a way to communicate with a service to
-- fetch new questions and asynchronously pipe them to the brick event channel
data Fetcher = Fetcher { _fChan  :: BChan BEvent }


--------------------------------------------------------------------------------
-- Executtion

execBrick :: Async (Either Error [Question]) -> App ()
execBrick aQuestions = do
  state <- get
  conf  <- ask
  _ <- liftIO $ do
    chan  <- newBChan 10
    passToChannel aQuestions chan
    let initialBState = BState { _bAppState  = state
                               , _bAppConfig = conf
                               , _bQuestions = Left StillInProgress
                               , _bFetcher   = Fetcher chan
                               }
    customMain (V.mkVty V.defaultConfig) (Just chan) app initialBState
  return () -- TODO figure out end game

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
passToChannel :: Async (Either Error [Question]) -> BChan BEvent -> IO ()
passToChannel aQuestions chan = void . forkIO $ do
  qResult <- wait aQuestions
  writeBChan chan $ NewQueryResult qResult

--------------------------------------------------------------------------------
-- Event Handling

handleEvent = undefined

--------------------------------------------------------------------------------
-- Drawing

drawUI = undefined

--------------------------------------------------------------------------------
-- Styling

theMap = undefined
