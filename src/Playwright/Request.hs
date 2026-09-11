module Playwright.Request
  ( Request (..)
  , requestFromRef
  , requestFrameRef
  , requestHeaders
  , requestIsNavigation
  , requestMethod
  , requestResourceType
  , requestUrl
  ) where

import Control.Exception
  ( throwIO
  )
import Playwright.Channel
  ( Channel (Channel, channelConnection, channelGuid)
  , RequestTag
  )
import Playwright.Connection
  ( Connection
  , lookupObject
  )
import Playwright.HttpHeader
  ( HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader)
  )
import Playwright.HttpMethod
  ( HttpMethod (HttpMethod)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.Protocol
  ( NameValue (nameValueName, nameValueValue)
  , RequestInitializer (requestInitializerFrame, requestInitializerHeaders, requestInitializerIsNavigationRequest, requestInitializerMethod, requestInitializerResourceType, requestInitializerUrl)
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.ResourceType
  ( ResourceType (ResourceType)
  )
import Playwright.Url
  ( Url
  )

newtype Request = Request
  { requestChannel :: Channel RequestTag
  }

requestFromRef :: Connection -> ChannelRef -> Request
requestFromRef conn ref =
  Request (Channel conn (channelRefGuid ref))

requestInitializer :: Request -> IO RequestInitializer
requestInitializer request = do
  obj <- lookupObject (channelConnection (requestChannel request)) (channelGuid (requestChannel request))
  either throwIO pure (fromJsonValue (remoteInitializer obj))

requestUrl :: Request -> IO Url
requestUrl request =
  requestInitializerUrl <$> requestInitializer request

requestMethod :: Request -> IO HttpMethod
requestMethod request =
  HttpMethod . requestInitializerMethod <$> requestInitializer request

requestResourceType :: Request -> IO ResourceType
requestResourceType request =
  ResourceType . requestInitializerResourceType <$> requestInitializer request

requestIsNavigation :: Request -> IO Bool
requestIsNavigation request =
  requestInitializerIsNavigationRequest <$> requestInitializer request

requestHeaders :: Request -> IO [HttpHeader]
requestHeaders request =
  fmap (fmap fromNameValue . requestInitializerHeaders) (requestInitializer request)

requestFrameRef :: Request -> IO (Maybe ChannelRef)
requestFrameRef request =
  requestInitializerFrame <$> requestInitializer request

fromNameValue :: NameValue -> HttpHeader
fromNameValue header =
  HttpHeader (HeaderName (nameValueName header)) (HeaderValue (nameValueValue header))
