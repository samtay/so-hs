module Interface.Brick
  ( runBrick
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Concurrent   (MVar, forkIO, newEmptyMVar, takeMVar)
import           Control.Monad        (forever)

--------------------------------------------------------------------------------
-- Library imports:
import           Brick                hiding (App)
import qualified Brick
import           Brick.BChan          (BChan, newBChan)
import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get)
import           Control.Monad.Trans  (liftIO)
import           Data.Bifunctor       (first)
import qualified Graphics.Vty         as V

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
data BEvent = NewQuestions [Question]

-- | Resource names
type Name = ()

-- | Fetcher provides a way to communicate with a service to
-- fetch new questions and asynchronously pipe them to the brick event channel
data Fetcher = Fetcher
  { _fCmd  :: (MVar FetchCommand)
  , _fChan :: BChan BEvent
  , _fCfg  :: AppConfig
  }

data FetchCommand = FetchCommand { fcState :: AppState }


--------------------------------------------------------------------------------
-- Executtion

runBrick :: App ()
runBrick = do
  state <- get
  conf  <- ask
  chan  <- liftIO $ newBChan 10
  fet   <- liftIO $ initFetcher conf chan
  let initialBState = BState { _bAppState  = state
                             , _bAppConfig = conf
                             , _bQuestions = Left StillInProgress
                             , _bFetcher   = fet
                             }
  finalState <- liftIO $ customMain (V.mkVty V.defaultConfig) (Just chan) app initialBState
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

-- | Initializes the "fetcher" service which can listen for
-- a supplied query and send the new questions to the given brick channel
initFetcher :: AppConfig -> (BChan BEvent) -> IO Fetcher
initFetcher cfg chan = do
  m <- newEmptyMVar
  let f = Fetcher m chan cfg
  forkIO (fetcher f)
  return f

-- | This is the "fetcher" services that 'initFetcher' forks off. It repeatedly
-- blocks when waiting for more fetch commands, and when it receives them,
-- it runs a query and sends the 'Question' results to the supplied channel.
--
-- add one for --verbose logging window (maybe AppState has logger mvar?)
fetcher :: Fetcher -> IO ()
fetcher (Fetcher mvar chan cfg) = forever $ do
  (FetchCommand state) <- takeMVar mvar
  eResult <- evalAppT cfg state query
  return $ first AppError eResult

--------------------------------------------------------------------------------
-- Event Handling

handleEvent = undefined

--------------------------------------------------------------------------------
-- Drawing

drawUI = undefined

--------------------------------------------------------------------------------
-- Styling

theMap = undefined
