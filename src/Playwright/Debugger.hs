module Playwright.Debugger
  ( Debugger
  , DebuggerLocation (..)
  , PausedDetails (..)
  , ApiCallStatus (..)
  , ApiCallUpdate (..)
  , armPausedStateChanged
  , debuggerEnable
  , debuggerFromContext
  , debuggerNext
  , debuggerRequestPause
  , debuggerResume
  , debuggerRunTo
  , onApiCallsUpdated
  , onPausedStateChanged
  , pausePage
  ) where

import Data.Text
  ( Text
  )
import Playwright.BrowserContext
  ( BrowserContext (browserContextChannel)
  )
import Playwright.Channel
  ( Channel (Channel, channelGuid)
  , DebuggerTag
  , send
  )
import Playwright.Connection
  ( lookupObject
  , registerEventHandler
  , registerFreshEventWaiter
  , waitForWaiter
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.Page
  ( Page (pageContextChannel)
  )
import Playwright.Point qualified as Pt
import Playwright.CssPixel
  ( CssPixel (CssPixel)
  )
import Playwright.Protocol
  ( BrowserContextPauseResult
  , mkBrowserContextPauseParams
  , Point (pointX, pointY)
  , BrowserContextInitializer (browserContextInitializerDebugger)
  , DebuggerApiCallsUpdatedEvent (debuggerApiCallsUpdatedEventApiCalls)
  , DebuggerApiCallsUpdatedEventApiCalls
      ( debuggerApiCallsUpdatedEventApiCallsError
      , debuggerApiCallsUpdatedEventApiCallsId
      , debuggerApiCallsUpdatedEventApiCallsNewLogEntries
      , debuggerApiCallsUpdatedEventApiCallsActionPoint
      , debuggerApiCallsUpdatedEventApiCallsLocation
      , debuggerApiCallsUpdatedEventApiCallsStatus
      , debuggerApiCallsUpdatedEventApiCallsTitle
      )
  , DebuggerApiCallsUpdatedEventApiCallsLocation
      ( debuggerApiCallsUpdatedEventApiCallsLocationColumn
      , debuggerApiCallsUpdatedEventApiCallsLocationFile
      , debuggerApiCallsUpdatedEventApiCallsLocationLine
      )
  , DebuggerApiCallsUpdatedEventApiCallsStatus
      ( DebuggerApiCallsUpdatedEventApiCallsStatusError
      , DebuggerApiCallsUpdatedEventApiCallsStatusRunning
      , DebuggerApiCallsUpdatedEventApiCallsStatusSuccess
      )
  , DebuggerEnableResult
  , DebuggerNextResult
  , DebuggerPausedStateChangedEvent (debuggerPausedStateChangedEventPausedDetails)
  , DebuggerPausedStateChangedEventPausedDetails
      ( debuggerPausedStateChangedEventPausedDetailsLocation
      , debuggerPausedStateChangedEventPausedDetailsStack
      , debuggerPausedStateChangedEventPausedDetailsTitle
      )
  , DebuggerPausedStateChangedEventPausedDetailsLocation
      ( debuggerPausedStateChangedEventPausedDetailsLocationColumn
      , debuggerPausedStateChangedEventPausedDetailsLocationFile
      , debuggerPausedStateChangedEventPausedDetailsLocationLine
      )
  , DebuggerRequestPauseResult
  , DebuggerResumeResult
  , DebuggerRunToParamsLocation
      ( DebuggerRunToParamsLocation
      , debuggerRunToParamsLocationColumn
      , debuggerRunToParamsLocationFile
      , debuggerRunToParamsLocationLine
      )
  , DebuggerRunToResult
  , mkDebuggerEnableParams
  , mkDebuggerNextParams
  , mkDebuggerRequestPauseParams
  , mkDebuggerResumeParams
  , mkDebuggerRunToParams
  )

import Data.Maybe
  ( mapMaybe
  )

-- | The Playwright debugger attached to a browser context. Every method here
-- talks to the driver's Debugger channel.
newtype Debugger = Debugger (Channel DebuggerTag)

-- | Where the debugger is (or where a call came from).
data DebuggerLocation = DebuggerLocation
  { debuggerLocationFile :: Text
  , debuggerLocationLine :: Maybe Int
  , debuggerLocationColumn :: Maybe Int
  }
  deriving stock (Eq, Show)

-- | Details of a pause: the call that is about to run and where it was made.
data PausedDetails = PausedDetails
  { pausedDetailsLocation :: DebuggerLocation
  , pausedDetailsTitle :: Text
  , pausedDetailsStack :: Maybe Text
  }
  deriving stock (Eq, Show)

data ApiCallStatus
  = ApiCallRunning
  | ApiCallSuccess
  | ApiCallError
  deriving stock (Eq, Show)

-- | One entry of an apiCallsUpdated event.
data ApiCallUpdate = ApiCallUpdate
  { apiCallUpdateId :: Text
  , apiCallUpdateTitle :: Text
  , apiCallUpdateLocation :: Maybe DebuggerLocation
  , apiCallUpdateStatus :: ApiCallStatus
  , apiCallUpdateError :: Maybe Text
  , apiCallUpdateActionPoint :: Maybe Pt.Point
  , apiCallUpdateNewLogEntries :: [Text]
  }
  deriving stock (Eq, Show)

debuggerFromContext :: BrowserContext -> Playwright Debugger
debuggerFromContext ctx = do
  obj <- lookupObject (channelGuid (browserContextChannel ctx))
  initializer <- throwDecode (remoteInitializer obj)
  pure (Debugger (Channel (channelRefGuid (browserContextInitializerDebugger initializer))))

debuggerGuid :: Debugger -> Guid
debuggerGuid (Debugger channel) = channelGuid channel

-- | Enables the debugger's API call tracking. Needed before 'onApiCallsUpdated'
-- delivers anything.
debuggerEnable :: Debugger -> Playwright ()
debuggerEnable (Debugger channel) = do
  _ :: DebuggerEnableResult <-
    send channel (MethodName "enable") mkDebuggerEnableParams
  pure ()

-- | Pauses before the next API call runs.
debuggerRequestPause :: Debugger -> Playwright ()
debuggerRequestPause (Debugger channel) = do
  _ :: DebuggerRequestPauseResult <-
    send channel (MethodName "requestPause") mkDebuggerRequestPauseParams
  pure ()

debuggerResume :: Debugger -> Playwright ()
debuggerResume (Debugger channel) = do
  _ :: DebuggerResumeResult <-
    send channel (MethodName "resume") mkDebuggerResumeParams
  pure ()

debuggerNext :: Debugger -> Playwright ()
debuggerNext (Debugger channel) = do
  _ :: DebuggerNextResult <-
    send channel (MethodName "next") mkDebuggerNextParams
  pure ()

-- | Resumes until the given source location is reached.
debuggerRunTo :: Debugger -> DebuggerLocation -> Playwright ()
debuggerRunTo (Debugger channel) location = do
  _ :: DebuggerRunToResult <-
    send
      channel
      (MethodName "runTo")
      ( mkDebuggerRunToParams
          DebuggerRunToParamsLocation
            { debuggerRunToParamsLocationFile = debuggerLocationFile location
            , debuggerRunToParamsLocationLine = debuggerLocationLine location
            , debuggerRunToParamsLocationColumn = debuggerLocationColumn location
            }
      )
  pure ()

-- | Starts listening for the next pausedStateChanged event. Arm before the
-- action that may pause; the returned action waits for that event. A
-- 'Nothing' result means the pause ended (no paused details).
armPausedStateChanged :: Debugger -> Playwright (Playwright (Maybe PausedDetails))
armPausedStateChanged dbg = do
  waitId <- registerFreshEventWaiter (debuggerGuid dbg) (MethodName "pausedStateChanged")
  pure $ do
    params <- waitForWaiter waitId
    event <- throwDecode params
    pure (pausedDetailsFromEvent event)

-- | Runs the callback for every pausedStateChanged event. Nothing means the
-- debugger resumed.
onPausedStateChanged :: Debugger -> (Maybe PausedDetails -> Playwright ()) -> Playwright ()
onPausedStateChanged dbg run =
  registerEventHandler (debuggerGuid dbg) (MethodName "pausedStateChanged") $ \params -> do
    event <- throwDecode params
    run (pausedDetailsFromEvent event)

-- | Runs the callback for every apiCallsUpdated event. Each event carries the
-- calls that changed since the previous one.
onApiCallsUpdated :: Debugger -> ([ApiCallUpdate] -> Playwright ()) -> Playwright ()
onApiCallsUpdated dbg run =
  registerEventHandler (debuggerGuid dbg) (MethodName "apiCallsUpdated") $ \params -> do
    event <- throwDecode params
    run (mapMaybe apiCallFromGenerated (debuggerApiCallsUpdatedEventApiCalls event))

pausedDetailsFromEvent :: DebuggerPausedStateChangedEvent -> Maybe PausedDetails
pausedDetailsFromEvent event =
  fmap fromDetails (debuggerPausedStateChangedEventPausedDetails event)
  where
    fromDetails details =
      PausedDetails
        { pausedDetailsLocation = locationFromPaused (debuggerPausedStateChangedEventPausedDetailsLocation details)
        , pausedDetailsTitle = debuggerPausedStateChangedEventPausedDetailsTitle details
        , pausedDetailsStack = debuggerPausedStateChangedEventPausedDetailsStack details
        }

locationFromPaused :: DebuggerPausedStateChangedEventPausedDetailsLocation -> DebuggerLocation
locationFromPaused location =
  DebuggerLocation
    { debuggerLocationFile = debuggerPausedStateChangedEventPausedDetailsLocationFile location
    , debuggerLocationLine = debuggerPausedStateChangedEventPausedDetailsLocationLine location
    , debuggerLocationColumn = debuggerPausedStateChangedEventPausedDetailsLocationColumn location
    }

apiCallFromGenerated :: DebuggerApiCallsUpdatedEventApiCalls -> Maybe ApiCallUpdate
apiCallFromGenerated call = do
  status <- statusFromGenerated (debuggerApiCallsUpdatedEventApiCallsStatus call)
  pure
    ApiCallUpdate
      { apiCallUpdateId = debuggerApiCallsUpdatedEventApiCallsId call
      , apiCallUpdateTitle = debuggerApiCallsUpdatedEventApiCallsTitle call
      , apiCallUpdateLocation = fmap apiLocation (debuggerApiCallsUpdatedEventApiCallsLocation call)
      , apiCallUpdateStatus = status
      , apiCallUpdateError = debuggerApiCallsUpdatedEventApiCallsError call
      , apiCallUpdateActionPoint = fmap toPoint (debuggerApiCallsUpdatedEventApiCallsActionPoint call)
      , apiCallUpdateNewLogEntries = debuggerApiCallsUpdatedEventApiCallsNewLogEntries call
      }
  where
    apiLocation location =
      DebuggerLocation
        { debuggerLocationFile = debuggerApiCallsUpdatedEventApiCallsLocationFile location
        , debuggerLocationLine = debuggerApiCallsUpdatedEventApiCallsLocationLine location
        , debuggerLocationColumn = debuggerApiCallsUpdatedEventApiCallsLocationColumn location
        }

toPoint :: Point -> Pt.Point
toPoint point =
  Pt.Point (CssPixel (pointX point)) (CssPixel (pointY point))

statusFromGenerated :: DebuggerApiCallsUpdatedEventApiCallsStatus -> Maybe ApiCallStatus
statusFromGenerated status =
  case status of
    DebuggerApiCallsUpdatedEventApiCallsStatusRunning -> Just ApiCallRunning
    DebuggerApiCallsUpdatedEventApiCallsStatusSuccess -> Just ApiCallSuccess
    DebuggerApiCallsUpdatedEventApiCallsStatusError -> Just ApiCallError

-- | Pauses the page the same way a test calls page.pause(). The call returns
-- once the debugger resumes.
pausePage :: Page -> Playwright ()
pausePage page = do
  _ :: BrowserContextPauseResult <-
    send (pageContextChannel page) (MethodName "pause") mkBrowserContextPauseParams
  pure ()
