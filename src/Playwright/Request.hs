module Playwright.Request
  ( Request (..)
  , requestFromRef
  , requestFrameRef
  , requestHeaders
  , requestIsNavigation
  , requestMethod
  , requestPostData
  , requestResourceType
  , requestResponseRef
  , requestUrl
  ) where

import Control.Exception
  ( throwIO
  )
import Data.ByteString
  ( ByteString
  )
import Playwright.Channel
  ( Channel (Channel, channelConnection, channelGuid)
  , RequestTag
  , send
  )
import Playwright.Connection
  ( Connection
  , lookupObject
  , lookupRequestOverrides
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
import Playwright.Json.ProtocolBinary
  ( protocolBinaryToByteString
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( NameValue (nameValueName, nameValueValue)
  , RequestInitializer (requestInitializerFrame, requestInitializerHeaders, requestInitializerIsNavigationRequest, requestInitializerMethod, requestInitializerPostData, requestInitializerResourceType, requestInitializerUrl)
  , RequestResponseResult (requestResponseResultResponse)
  , mkRequestResponseParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.RequestOverrides
  ( RequestOverrides (requestOverrideHeaders, requestOverrideMethod, requestOverridePostData, requestOverrideUrl)
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

requestOverrides :: Request -> IO RequestOverrides
requestOverrides request =
  lookupRequestOverrides
    (channelConnection (requestChannel request))
    (channelGuid (requestChannel request))

requestUrl :: Request -> IO Url
requestUrl request = do
  overrides <- requestOverrides request
  case requestOverrideUrl overrides of
    Just url ->
      pure url
    Nothing ->
      requestInitializerUrl <$> requestInitializer request

requestMethod :: Request -> IO HttpMethod
requestMethod request = do
  overrides <- requestOverrides request
  case requestOverrideMethod overrides of
    Just method ->
      pure method
    Nothing ->
      HttpMethod . requestInitializerMethod <$> requestInitializer request

requestResourceType :: Request -> IO ResourceType
requestResourceType request =
  ResourceType . requestInitializerResourceType <$> requestInitializer request

requestIsNavigation :: Request -> IO Bool
requestIsNavigation request =
  requestInitializerIsNavigationRequest <$> requestInitializer request

requestPostData :: Request -> IO (Maybe ByteString)
requestPostData request = do
  overrides <- requestOverrides request
  case requestOverridePostData overrides of
    Just bytes ->
      pure (Just bytes)
    Nothing ->
      fmap (fmap protocolBinaryToByteString . requestInitializerPostData) (requestInitializer request)

requestHeaders :: Request -> IO [HttpHeader]
requestHeaders request = do
  overrides <- requestOverrides request
  case requestOverrideHeaders overrides of
    Just headers ->
      pure headers
    Nothing ->
      fmap (fmap fromNameValue . requestInitializerHeaders) (requestInitializer request)

requestFrameRef :: Request -> IO (Maybe ChannelRef)
requestFrameRef request =
  requestInitializerFrame <$> requestInitializer request

requestResponseRef :: Request -> IO (Maybe ChannelRef)
requestResponseRef request = do
  result <-
    send
      (requestChannel request)
      (MethodName "response")
      mkRequestResponseParams
  pure (requestResponseResultResponse result)

fromNameValue :: NameValue -> HttpHeader
fromNameValue header =
  HttpHeader (HeaderName (nameValueName header)) (HeaderValue (nameValueValue header))
