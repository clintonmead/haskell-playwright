module Playwright.Session
  ( Session (..)
  , ScreencastSession (..)
  , HarRecorder (..)
  , ScopedRuns (..)
  , emptySession
  , removeWaiter
  ) where

import Control.Concurrent
  ( ThreadId
  )
import Control.Exception
  ( SomeException
  )
import Data.Aeson qualified as Aeson
import Data.Map.Strict
  ( Map
  )
import Data.Map.Strict qualified as Map
import Data.Sequence
  ( Seq
  )
import Data.Set
  ( Set
  )
import Data.Set qualified as Set
import Data.Scientific
  ( Scientific
  )
import Data.Text
  ( Text
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.EventHandler
  ( EventHandler
  )
import Playwright.EventWaiter
  ( EventWaiter
  )
import Playwright.Guid
  ( Guid
  , rootGuid
  )
import Playwright.Json.ChannelRef
  ( ChannelRef
  )
import Playwright.MethodName
  ( MethodName
  )
import Playwright.Protocol
  ( ChannelType (ChannelTypeRoot)
  , SelectorEngine
  )
import Playwright.RemoteObject
  ( RemoteObject (RemoteObject, remoteGuid, remoteInitializer, remoteParent, remoteType)
  )
import Playwright.Logger
  ( Logger
  )
import Playwright.RequestId
  ( RequestId (RequestId)
  )
import Playwright.RequestOverrides
  ( RequestOverrides
  )
import Playwright.StoredLocatorHandler
  ( StoredLocatorHandler
  )
import Playwright.TimeoutDefaults
  ( TimeoutDefaults
  )
import Playwright.Url
  ( Url
  )
import Playwright.UrlMatch
  ( UrlMatch
  )
import Playwright.WaitId
  ( WaitId (WaitId)
  )

-- | A page's running screencast: the generation that tags its frame handler,
-- the recording artifact the driver returned at start, and the path to save
-- that artifact to when the screencast stops.
data ScreencastSession = ScreencastSession
  { screencastSessionGeneration :: Int
  , screencastSessionArtifact :: Maybe Guid
  , screencastSessionPath :: Maybe FilePath
  }

-- | A browser context's running HAR recording: the driver's harId and the
-- path the HAR is written to when the context closes.
data HarRecorder = HarRecorder
  { harRecorderId :: Text
  , harRecorderPath :: FilePath
  , harRecorderTracing :: Guid
  }

data Session = Session
  { sessionObjects :: Map Guid RemoteObject
  , sessionResults :: Map RequestId (Either PlaywrightError Aeson.Value)
  , sessionPending :: Set RequestId
  , sessionNextId :: RequestId
  , sessionClosed :: Maybe PlaywrightError
  , sessionEventWaiters :: [WaitId]
  , sessionWaiters :: Map WaitId EventWaiter
  , sessionNextWaitId :: WaitId
  , sessionEventBuffer :: Map (Guid, MethodName) (Seq Aeson.Value)
  , sessionClosedGuids :: Set Guid
  , sessionDetachedFrames :: Set Guid
  , sessionEventHandlers :: [EventHandler]
  , sessionRouteHandlers :: Map Guid [(UrlMatch, ChannelRef -> IO Bool)]
  , sessionRouteListening :: Set Guid
  , sessionWebSocketRouteHandlers :: Map Guid [(UrlMatch, Guid -> IO ())]
  , sessionWebSocketRouteListening :: Set Guid
  , sessionRequestOverrides :: Map Guid RequestOverrides
  , sessionContextBaseUrls :: Map Guid Url
  , sessionBrowserContexts :: Map Guid [Guid]
  , sessionContextBrowsers :: Map Guid Guid
  , sessionPagesOwningContext :: Set Guid
  , sessionContextsWithOwnerPage :: Set Guid
  , sessionScreencasts :: Map Guid ScreencastSession
  , sessionHarRecorders :: Map Guid [HarRecorder]
  , sessionBrowserTypes :: Map Guid Guid
  , sessionSelectorEngines :: [SelectorEngine]
  , sessionTimeoutDefaults :: Map Guid TimeoutDefaults
  , sessionMainFramePages :: Map Guid Guid
  , sessionFinishedRequests :: Set Guid
  , sessionDisposedObjects :: Map Guid RemoteObject
  , sessionRequestEndTiming :: Map Guid Scientific
  , sessionRequestFailures :: Map Guid Text
  , sessionScopedRuns :: Map (Guid, MethodName) ScopedRuns
  , sessionPendingHandlers :: Int
  , sessionHandlerThreads :: Set ThreadId
  , sessionLocatorHandlers :: Map Guid (Map Int StoredLocatorHandler)
  , sessionLocatorListening :: Set Guid
  , sessionPlaywrightGuid :: Maybe Guid
  , sessionChromiumGuid :: Maybe Guid
  , sessionFirefoxGuid :: Maybe Guid
  , sessionWebkitGuid :: Maybe Guid
  , sessionLoggers :: Map Guid Logger
  }

-- | Runs queued for one scope and method: how many are still pending, and the
-- errors the finished ones raised, newest first.
data ScopedRuns = ScopedRuns
  { scopedRunsPending :: Int
  , scopedRunsErrors :: [SomeException]
  }

emptySession :: Session
emptySession =
  Session
    { sessionObjects = Map.singleton rootGuid rootObject
    , sessionResults = Map.empty
    , sessionPending = Set.empty
    , sessionNextId = RequestId 1
    , sessionClosed = Nothing
    , sessionEventWaiters = []
    , sessionWaiters = Map.empty
    , sessionNextWaitId = WaitId 1
    , sessionEventBuffer = Map.empty
    , sessionClosedGuids = Set.empty
    , sessionDetachedFrames = Set.empty
    , sessionEventHandlers = []
    , sessionRouteHandlers = Map.empty
    , sessionRouteListening = Set.empty
    , sessionWebSocketRouteHandlers = Map.empty
    , sessionWebSocketRouteListening = Set.empty
    , sessionRequestOverrides = Map.empty
    , sessionContextBaseUrls = Map.empty
    , sessionBrowserContexts = Map.empty
    , sessionContextBrowsers = Map.empty
    , sessionPagesOwningContext = Set.empty
    , sessionContextsWithOwnerPage = Set.empty
    , sessionScreencasts = Map.empty
    , sessionHarRecorders = Map.empty
    , sessionBrowserTypes = Map.empty
    , sessionSelectorEngines = []
    , sessionTimeoutDefaults = Map.empty
    , sessionMainFramePages = Map.empty
    , sessionFinishedRequests = Set.empty
    , sessionDisposedObjects = Map.empty
    , sessionRequestEndTiming = Map.empty
    , sessionRequestFailures = Map.empty
    , sessionScopedRuns = Map.empty
    , sessionPendingHandlers = 0
    , sessionHandlerThreads = Set.empty
    , sessionLocatorHandlers = Map.empty
    , sessionLocatorListening = Set.empty
    , sessionPlaywrightGuid = Nothing
    , sessionChromiumGuid = Nothing
    , sessionFirefoxGuid = Nothing
    , sessionWebkitGuid = Nothing
    , sessionLoggers = Map.empty
    }

rootObject :: RemoteObject
rootObject =
  RemoteObject
    { remoteType = ChannelTypeRoot
    , remoteGuid = rootGuid
    , remoteParent = rootGuid
    , remoteInitializer = Aeson.object []
    }

removeWaiter :: WaitId -> Session -> Session
removeWaiter waitId session =
  session
    { sessionWaiters = Map.delete waitId (sessionWaiters session)
    , sessionEventWaiters = filter (/= waitId) (sessionEventWaiters session)
    }
