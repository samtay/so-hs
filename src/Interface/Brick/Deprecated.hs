{-# OPTIONS_GHC -fno-warn-unused-top-binds #-} -- we don't use all the generated lenses
module Interface.Brick.Deprecated
  ( execBrick
  ) where

--------------------------------------------------------------------------------
-- Base imports:
import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Exception
import           Control.Monad            (forever, void)
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))

--------------------------------------------------------------------------------
-- Library imports:
import           Brick                    hiding (App, Direction)
import qualified Brick
import           Brick.BChan              (BChan, newBChan, writeBChan)
import           Brick.Focus
import           Brick.Markup
import qualified Brick.Widgets.Border     as B
import qualified Brick.Widgets.Center     as C
import           Brick.Widgets.List
import qualified Brick.Widgets.List       as L
import           Control.Concurrent.Async (Async, async, wait)
import           Control.Monad.Reader     (ask)
import           Control.Monad.State      (get)
import           Control.Monad.Trans      (liftIO)
import           Data.CircularList        (CList)
import qualified Data.CircularList        as CL
import qualified Data.Text                as T
import           Data.Vector              (Vector, fromList)
import qualified Graphics.Vty             as V
import           Lens.Micro               (to, (%~), (&), (.~), (^.))
import           Lens.Micro.TH            (makeLenses)
import           Text.RawString.QQ        (r)

--------------------------------------------------------------------------------
-- Local imports:
import           Markdown
import           StackOverflow
import           Types
import           Utils

--------------------------------------------------------------------------------
-- Types

-- | Events that we pipe to the event handler asynchronously
data BEvent
  = NewQueryResult [Question (GenericList Name Vector) Markdown]
  | NewQueryError Error
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
  | AnswerList
  | QuestionView
  | AnswerView
  deriving (Eq, Ord, Show, Enum, Bounded)

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq)

-- | TODO add a bunch of stuff to this.. editor context, etc..
-- Remember I might need turtle to pipe stuff to copy-paste command
-- Also possibly add to util backup defaults, check if pbcopy, xclip, etc. is in PATH.
data BState = BState
  { _bQuestions  :: GenericList Name Vector (Question (GenericList Name Vector) Markdown)
  , _bFocusRing  :: FocusRing Name
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

execBrick :: Async [Question [] Markdown] -> App ()
execBrick aQuestions = do
  state <- get
  conf  <- ask
  void . liftIO $ do
    chan  <- newBChan 10
    passToChannel aQuestions chan
    startTimeTicker chan
    -- TODO perhaps extents can help keep track of list & viewport heights ??
    let initialBState = BState { _bQuestions  = list QuestionList mempty 1
                               , _bFocusRing  = focusRing [minBound..maxBound]
                               , _bError      = Nothing
                               , _bLoading    = Just 0
                               , _bShowSplash = True
                               , _bShowHelp   = False
                               , _bAppState   = state
                               , _bAppConfig  = conf
                               , _bFetcher    = Fetcher chan
                               }
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    customMain initialVty buildVty (Just chan) app initialBState
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
passToChannel :: Async ([Question [] Markdown]) -> BChan BEvent -> IO ()
passToChannel aQuestions chan = void . forkIO $ do
  writeBChan chan =<< catch (mkRes <$> wait aQuestions)
    \(e :: Error) -> pure $ NewQueryError e
  where
    mkRes = NewQueryResult . fmap (qAnswers %~ (\as -> list AnswerList (fromList as) 1))

--------------------------------------------------------------------------------
-- Event Handling

handleEvent :: BState -> BrickEvent Name BEvent -> EventM Name (Next BState)
handleEvent bs = \case
  -- App events
  AppEvent TimeTick            -> continue $ bs & bLoading %~ fmap (+. 1)
  AppEvent (NewQueryError e)   -> continue $ bs & bError .~ Just (AppError e) & fetched
  AppEvent (NewQueryResult qs) -> continue $ replaceQAs qs & fetched
  -- Resize pane events
  VtyEvent (V.EvKey (V.KChar 'k') [V.MCtrl, V.MShift]) -> adjustView (Just North)
  VtyEvent (V.EvKey (V.KChar 'l') [V.MCtrl, V.MShift]) -> adjustView (Just East)
  VtyEvent (V.EvKey (V.KChar 'j') [V.MCtrl, V.MShift]) -> adjustView (Just South)
  VtyEvent (V.EvKey (V.KChar 'h') [V.MCtrl, V.MShift]) -> adjustView (Just West)
  VtyEvent (V.EvResize _ _)                            -> adjustView Nothing
  -- Miscellanenous events
  VtyEvent (V.EvKey (V.KChar '?') []) -> continue $ bs & bShowHelp .~ True
  VtyEvent (V.EvKey V.KEsc        []) -> continue $ bs & bShowHelp .~ False
                                                       & bError .~ Nothing
  VtyEvent (V.EvKey (V.KChar 'q') []) -> halt bs
  -- Change focus events
  VtyEvent (V.EvKey (V.KChar 'k') [V.MCtrl])   -> changeFocus North
  VtyEvent (V.EvKey (V.KChar 'l') [V.MCtrl])   -> changeFocus East
  VtyEvent (V.EvKey (V.KChar 'j') [V.MCtrl])   -> changeFocus South
  VtyEvent (V.EvKey (V.KChar 'h') [V.MCtrl])   -> changeFocus West
  VtyEvent (V.EvKey (V.KChar '\t') [])         -> continue $ bs & bFocusRing %~ focusNext
  VtyEvent (V.EvKey (V.KChar '\t') [V.MShift]) -> continue $ bs & bFocusRing %~ focusPrev
  VtyEvent ev ->
    case (ev, bs ^. bFocusRing ^. to focusGetCurrent) of
      -- Change focus events
      (V.EvKey (V.KChar 'l') [], Just QuestionList) -> continue $ setFocus AnswerList
      (V.EvKey (V.KChar 'h') [], Just AnswerList)   -> continue $ setFocus QuestionList
      -- Scrolling events
      (V.EvKey (V.KChar k) mods, Just v)
        -- TODO add || here against (k,mods) existing in known dictionary
        | v `elem` [QuestionView, AnswerView] -> handleScrollKey k mods (viewToVP v) >> continue bs
      -- List events
      (_, Just QuestionList) -> do
        l <- handleList ev (bs ^. bQuestions)
        continue $ bs & bQuestions .~ l
      (_, Just AnswerList) -> do
        op <- case bs ^. bQuestions ^. to listSelectedElement of
                Nothing -> return id
                Just (_, q) -> (qAnswers .~) <$> handleList ev (q ^. qAnswers)
        continue $ bs & bQuestions %~ listModify op
      _ -> continue bs
  _ -> continue bs
  where
    fetched :: BState -> BState
    fetched = (bLoading .~ Nothing) . (bShowSplash .~ False)

    replaceQAs :: [Question (GenericList Name Vector) Markdown] -> BState
    replaceQAs [] = bs & bError .~ Just NoResults
    replaceQAs qs = bs & bError .~ Nothing
                       & bQuestions %~ listReplace (fromList qs) (Just 0)

    -- TODO see if resize w,h is helpful here
    -- TODO implement
    adjustView :: Maybe Direction -> EventM Name (Next BState)
    adjustView _ = continue bs

    changeFocus :: Direction -> EventM Name (Next BState)
    changeFocus dir = continue $
      case (bs ^. bFocusRing ^. to focusGetCurrent, dir) of
        (Just QuestionList, East)  -> setFocus AnswerList
        (Just QuestionList, South) -> setFocus QuestionView
        (Just AnswerList, West)    -> setFocus QuestionList
        (Just AnswerList, South)   -> setFocus AnswerView
        (Just QuestionView, East)  -> setFocus AnswerView
        (Just QuestionView, North) -> setFocus QuestionList
        (Just AnswerView, West)    -> setFocus QuestionView
        (Just AnswerView, North)   -> setFocus AnswerList
        _                          -> bs

    setFocus :: Name -> BState
    setFocus n = bs & bFocusRing %~ focusRingModify (tryRotateTo n)

    handleList :: Ord n => V.Event -> List n e -> EventM n (List n e)
    handleList ev l = L.handleListEventVi L.handleListEvent ev l

    handleScrollKey :: Char -> [V.Modifier] -> ViewportScroll Name -> EventM Name ()
    handleScrollKey 'k' [] v        = vScrollBy v (-1)
    handleScrollKey 'l' [] v        = hScrollBy v 1
    handleScrollKey 'j' [] v        = vScrollBy v 1
    handleScrollKey 'h' [] v        = hScrollBy v (-1)
    handleScrollKey '^' [] v        = hScrollToBeginning v
    handleScrollKey '0' [] v        = hScrollToBeginning v
    handleScrollKey '$' [] v        = hScrollToEnd v
    handleScrollKey 'g' [] v        = vScrollToBeginning v
    handleScrollKey 'G' [] v        = vScrollToEnd v
    handleScrollKey 'b' [V.MCtrl] v = vScrollPage v Up
    handleScrollKey 'f' [V.MCtrl] v = vScrollPage v Down
    handleScrollKey 'u' [] v        = vScrollPage v Up
    handleScrollKey 'd' [] v        = vScrollPage v Down
    handleScrollKey _ _ _           = return ()

    viewToVP QuestionView = qViewport
    viewToVP AnswerView   = aViewport
    viewToVP _            = error "Invalid viewport!"

qViewport, aViewport :: ViewportScroll Name
qViewport = viewportScroll QuestionView
aViewport = viewportScroll AnswerView

--------------------------------------------------------------------------------
-- Drawing

drawUI :: BState -> [Widget Name]
drawUI bs = [ case bs ^. bShowHelp of
                True  -> helpWidget
                False -> emptyWidget
            , case (bs ^. bLoading, bs ^. bShowSplash) of
                (Just n, False) -> C.centerLayer . B.border $ drawLoading n
                (Just n, True)  -> C.center $ splashWidget <=> drawLoading n
                _               -> emptyWidget
            , case bs ^. bError of
                Just e -> drawError e
                _      -> emptyWidget
            , drawQAPanes (bs ^. bQuestions) (bs ^. bFocusRing)
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

-- TODO need some attributes
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
  :: List Name (Question (GenericList Name Vector) Markdown)
  -> FocusRing Name
  -> Widget Name
drawQAPanes qList ring = do
  let [qListFocused, aListFocused, qViewFocused, aViewFocused] = map isFocused [minBound..maxBound]
      qFocused = qListFocused || qViewFocused
      aFocused = aListFocused || aViewFocused
      listFocused = qListFocused || aListFocused
      viewFocused = qViewFocused || aViewFocused
  vBox [ hBox [ drawQList qListFocused qList
              , highlightIf listFocused B.vBorder
              , drawAList aListFocused aList
              ]
       , hBox [ highlightIf qFocused B.hBorder
              , str " "
              , highlightIf aFocused B.hBorder
              ]
       , hBox [ drawQ currQ
              , highlightIf viewFocused B.vBorder
              , drawA currA
              ]
       ]
  where
    currQ = snd <$> listSelectedElement qList
    aList = _qAnswers <$> currQ
    currA = aList >>= fmap snd . listSelectedElement
    highlightIf b = if b then updateAttrMap $ applyAttrMappings [(B.borderAttr, fg V.green)]
                         else id
    isFocused name = maybe False (== name) (focusGetCurrent ring)

drawQList :: Bool -> List Name (Question (GenericList Name Vector) Markdown) -> Widget Name
drawQList b l = padRight Max $ L.renderList renderQ b l
  where
    renderQ s q = drawScore s (q ^. qScore) <+> txt (q ^. qTitle)

drawAList :: Bool -> Maybe (List Name (Answer Markdown)) -> Widget Name
drawAList b ml = padRight Max $ maybe emptyWidget (L.renderList renderA b) ml
  where
    -- TODO add checkmark for accepted
    -- TODO steal approach from Prompt module
    renderA s a = drawScore s (a ^. aScore) <+> str (take 30 . show $ a ^. aBody)

-- TODO report extent and set h/vlimits equal to extent - why no easy function to do this?
-- TODO once markdownWrap is implemented, only allow vertical scrolling
drawQ :: Maybe (Question t Markdown) -> Widget Name
drawQ = viewport QuestionView Both . maybe emptyWidget (drawMarkdown . _qBody)

-- TODO once markdownWrap is implemented, only allow vertical scrolling
drawA :: Maybe (Answer Markdown) -> Widget Name
drawA = viewport AnswerView Both . maybe emptyWidget (drawMarkdown . _aBody)

drawMarkdown :: Markdown -> Widget Name
drawMarkdown (Markdown segments) = markup . mconcat . ffor segments $
  \case
    SPlain t -> fromText t
    SBold t -> t @? boldAttr
    SItalic t -> t @? italicAttr
    SCode t -> t @? codeAttr
    SKbd t -> t @? kbdAttr
    SQuote t -> t @? quoteAttr
  where
    fromText = (@? "")

drawScore :: Bool -> Int -> Widget Name
drawScore s n = padRight (Pad 1) . withAttr attr .
  str $ "(" <> show n <> ")"
  where attr = (if s then listSelectedAttr else mempty)
                 <> case compare n 0 of
                      LT -> scoreNegativeAttr
                      EQ -> scoreZeroAttr
                      GT -> scorePositiveAttr

--------------------------------------------------------------------------------
-- Styling

-- TODO look up how to allow themes and have the one here be "default"
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (focusedAttr, fg V.green)
  , (listSelectedAttr, V.defAttr `V.withStyle` V.reverseVideo)
  , (scoreNegativeAttr, fg V.red `V.withStyle` V.bold)
  , (scoreZeroAttr, fg V.green `V.withStyle` V.bold)
  , (scorePositiveAttr, fg V.green `V.withStyle` V.bold)
  -- TODO why isn't hierarcy working? sigh
  , (listSelectedAttr <> scoreNegativeAttr, fg V.red `V.withStyle` V.bold `V.withStyle` V.reverseVideo)
  , (listSelectedAttr <> scoreZeroAttr, fg V.green `V.withStyle` V.bold `V.withStyle` V.reverseVideo)
  , (listSelectedAttr <> scorePositiveAttr, fg V.green `V.withStyle` V.bold `V.withStyle` V.reverseVideo)
  , (boldAttr, V.defAttr `V.withStyle` V.bold)
  , (italicAttr, V.defAttr `V.withStyle` V.standout)
  , (codeAttr, fg V.cyan)
  ]

focusedAttr :: AttrName
focusedAttr = "focused"

scoreNegativeAttr, scoreZeroAttr, scorePositiveAttr :: AttrName
scoreNegativeAttr = "scoreNegative"
scoreZeroAttr = "scoreZero"
scorePositiveAttr = "scorePositive"

boldAttr, italicAttr, codeAttr, kbdAttr, quoteAttr :: AttrName
boldAttr = "bold"
italicAttr = "italic"
codeAttr = "code"
kbdAttr = "kbd"
quoteAttr = "quote"

--------------------------------------------------------------------------------
-- Utilities

-- | Addition modulo loading dots
(+.) :: Int -> Int -> Int
n +. m = (n + m) `mod` loadingDotCount

-- | Safe version of Cl.rotateTo
tryRotateTo :: Eq a => a -> CList a -> CList a
tryRotateTo a cl = fromMaybe cl (CL.rotateTo a cl)
