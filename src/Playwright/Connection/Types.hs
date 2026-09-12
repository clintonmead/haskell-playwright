module Playwright.Connection.Types
  ( Connection (..)
  , EventHandler (..)
  , EventWaiter (..)
  , OutgoingCall (..)
  , StoredLocatorHandler (..)
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , object
  , requiredField'
  , (.=)
  )
import Control.Concurrent
  ( ThreadId
  )
import Control.Concurrent.STM
  ( TMVar
  , TVar
  )
import Control.Concurrent.STM.TQueue
  ( TQueue
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value
  )
import Data.Map.Strict
  ( Map
  )
import Data.Sequence
  ( Seq
  )
import Data.Set
  ( Set
  )
import Data.Text
  ( Text
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Json.ChannelRef
  ( ChannelRef
  )
import Playwright.MethodName
  ( MethodName
  )
import Playwright.Protocol
  ( SelectorEngine
  )
import Playwright.RemoteObject
  ( RemoteObject
  )
import Playwright.RequestId
  ( RequestId
  )
import Playwright.RequestOverrides
  ( RequestOverrides
  )
import Playwright.TimeoutDefaults
  ( TimeoutDefaults
  )
import Playwright.Transport
  ( Transport
  )
import Playwright.Url
  ( Url
  )
import Playwright.UrlGlob
  ( UrlGlob
  )

data Connection = Connection
  { connectionTransport :: Transport
  , connectionObjects :: TVar (Map Guid RemoteObject)
  , connectionCallbacks :: TVar (Map RequestId (TMVar (Either PlaywrightError Value)))
  , connectionNextId :: TVar RequestId
  , connectionClosed :: TVar (Maybe PlaywrightError)
  , connectionEventWaiters :: TVar [EventWaiter]
  , connectionEventBuffer :: TVar (Map (Guid, MethodName) (Seq Value))
  , connectionClosedGuids :: TVar (Set Guid)
  , connectionEventHandlers :: TVar [EventHandler]
  , connectionRouteHandlers :: TVar (Map Guid [(UrlGlob, ChannelRef -> IO Bool)])
  , connectionRouteListening :: TVar (Set Guid)
  , connectionRequestOverrides :: TVar (Map Guid RequestOverrides)
  , connectionContextBaseUrls :: TVar (Map Guid Url)
  , connectionBrowserContexts :: TVar (Map Guid [Guid])
  , connectionContextBrowsers :: TVar (Map Guid Guid)
  , connectionPagesOwningContext :: TVar (Set Guid)
  , connectionSelectorEngines :: TVar [SelectorEngine]
  , connectionTimeoutDefaults :: TVar (Map Guid TimeoutDefaults)
  , connectionMainFramePages :: TVar (Map Guid Guid)
  , connectionFinishedRequests :: TVar (Set Guid)
  , connectionRequestFailures :: TVar (Map Guid Text)
  , connectionPendingHandlers :: TVar Int
  , connectionHandlerThreads :: TVar (Set ThreadId)
  , connectionHandlerQueue :: TQueue (IO ())
  , connectionLocatorHandlers :: TVar (Map Guid (Map Int StoredLocatorHandler))
  , connectionLocatorListening :: TVar (Set Guid)
  }

data StoredLocatorHandler = StoredLocatorHandler
  { storedLocatorHandlerTimes :: Maybe Int
  , storedLocatorHandlerSelector :: Text
  , storedLocatorHandlerFrame :: Guid
  , storedLocatorHandlerRun :: IO ()
  }

data EventWaiter = EventWaiter
  { eventWaiterGuid :: Guid
  , eventWaiterMethod :: MethodName
  , eventWaiterBox :: TMVar Value
  }

data EventHandler = EventHandler
  { eventHandlerGuid :: Guid
  , eventHandlerMethod :: MethodName
  , eventHandlerRun :: Value -> IO ()
  }

data OutgoingCall = OutgoingCall
  { outgoingCallId :: RequestId
  , outgoingCallGuid :: Guid
  , outgoingCallMethod :: MethodName
  , outgoingCallParams :: Value
  , outgoingCallMetadata :: Value
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec OutgoingCall)

instance HasCodec OutgoingCall where
  codec =
    object "OutgoingCall" $
      OutgoingCall
        <$> requiredField' "id" .= outgoingCallId
        <*> requiredField' "guid" .= outgoingCallGuid
        <*> requiredField' "method" .= outgoingCallMethod
        <*> requiredField' "params" .= outgoingCallParams
        <*> requiredField' "metadata" .= outgoingCallMetadata
