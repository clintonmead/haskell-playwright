module Playwright.Session
  ( Session (..)
  , emptySession
  , removeWaiter
  ) where

import Control.Concurrent
  ( ThreadId
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
import Playwright.UrlGlob
  ( UrlGlob
  )
import Playwright.WaitId
  ( WaitId (WaitId)
  )

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
  , sessionEventHandlers :: [EventHandler]
  , sessionRouteHandlers :: Map Guid [(UrlGlob, ChannelRef -> IO Bool)]
  , sessionRouteListening :: Set Guid
  , sessionRequestOverrides :: Map Guid RequestOverrides
  , sessionContextBaseUrls :: Map Guid Url
  , sessionBrowserContexts :: Map Guid [Guid]
  , sessionContextBrowsers :: Map Guid Guid
  , sessionPagesOwningContext :: Set Guid
  , sessionContextsWithOwnerPage :: Set Guid
  , sessionBrowserTypes :: Map Guid Guid
  , sessionSelectorEngines :: [SelectorEngine]
  , sessionTimeoutDefaults :: Map Guid TimeoutDefaults
  , sessionMainFramePages :: Map Guid Guid
  , sessionFinishedRequests :: Set Guid
  , sessionRequestEndTiming :: Map Guid Scientific
  , sessionRequestFailures :: Map Guid Text
  , sessionPendingHandlers :: Int
  , sessionHandlerThreads :: Set ThreadId
  , sessionLocatorHandlers :: Map Guid (Map Int StoredLocatorHandler)
  , sessionLocatorListening :: Set Guid
  , sessionPlaywrightGuid :: Maybe Guid
  , sessionChromiumGuid :: Maybe Guid
  , sessionFirefoxGuid :: Maybe Guid
  , sessionWebkitGuid :: Maybe Guid
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
    , sessionEventHandlers = []
    , sessionRouteHandlers = Map.empty
    , sessionRouteListening = Set.empty
    , sessionRequestOverrides = Map.empty
    , sessionContextBaseUrls = Map.empty
    , sessionBrowserContexts = Map.empty
    , sessionContextBrowsers = Map.empty
    , sessionPagesOwningContext = Set.empty
    , sessionContextsWithOwnerPage = Set.empty
    , sessionBrowserTypes = Map.empty
    , sessionSelectorEngines = []
    , sessionTimeoutDefaults = Map.empty
    , sessionMainFramePages = Map.empty
    , sessionFinishedRequests = Set.empty
    , sessionRequestEndTiming = Map.empty
    , sessionRequestFailures = Map.empty
    , sessionPendingHandlers = 0
    , sessionHandlerThreads = Set.empty
    , sessionLocatorHandlers = Map.empty
    , sessionLocatorListening = Set.empty
    , sessionPlaywrightGuid = Nothing
    , sessionChromiumGuid = Nothing
    , sessionFirefoxGuid = Nothing
    , sessionWebkitGuid = Nothing
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
