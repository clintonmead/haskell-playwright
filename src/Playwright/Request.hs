module Playwright.Request
  ( Request (..)
  , requestFromRef
  , requestFrameRef
  , requestFailure
  , requestHeaders
  , requestIsNavigation
  , requestMethod
  , requestPostData
  , requestPostDataJSON
  , requestRedirectedFrom
  , requestRedirectedTo
  , requestResourceType
  , requestResponseRef
  , requestSizes
  , requestUrl
  , requestsEqual
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Aeson
  ( Value
  , eitherDecodeStrict
  )
import Data.ByteString
  ( ByteString
  )
import Data.Maybe
  ( listToMaybe
  , mapMaybe
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Playwright.Channel
  ( Channel (Channel, channelConnection, channelGuid)
  , RequestTag
  , ResponseTag
  , send
  )
import Playwright.Connection
  ( Connection
  , guidsOfType
  , lookupObject
  , lookupRequestFailure
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
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
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
import Playwright.NetworkSizes
  ( BodySize (BodySize)
  , HeadersSize (HeadersSize)
  , NetworkSizes (NetworkSizes)
  )
import Playwright.Protocol
  ( ChannelType (ChannelTypeRequest)
  , NameValue (nameValueName, nameValueValue)
  , RequestInitializer (requestInitializerFrame, requestInitializerHeaders, requestInitializerIsNavigationRequest, requestInitializerMethod, requestInitializerPostData, requestInitializerRedirectedFrom, requestInitializerResourceType, requestInitializerUrl)
  , RequestResponseResult (requestResponseResultResponse)
  , RequestSizes (requestSizesRequestBodySize, requestSizesRequestHeadersSize, requestSizesResponseBodySize, requestSizesResponseHeadersSize)
  , ResponseSizesResult (responseSizesResultSizes)
  , mkRequestResponseParams
  , mkResponseSizesParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteGuid, remoteInitializer)
  )
import Playwright.RequestFailure
  ( RequestFailure (RequestFailure)
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

requestsEqual :: Request -> Request -> Bool
requestsEqual left right =
  channelGuid (requestChannel left) == channelGuid (requestChannel right)

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

requestFailure :: Request -> IO (Maybe RequestFailure)
requestFailure request = do
  text <-
    lookupRequestFailure
      (channelConnection (requestChannel request))
      (channelGuid (requestChannel request))
  case text of
    Just value | value /= mempty ->
      pure (Just (RequestFailure value))
    _ ->
      pure Nothing

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

requestPostDataJSON :: Request -> IO (Maybe Value)
requestPostDataJSON request = do
  payload <- requestPostData request
  case payload of
    Nothing ->
      pure Nothing
    Just bytes ->
      case eitherDecodeStrict bytes of
        Right value ->
          pure (Just value)
        Left _ ->
          throwIO
            ( DecodeFailure
                ("POST data is not a valid JSON object: " <> decodeUtf8 bytes)
            )

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

requestRedirectedFrom :: Request -> IO (Maybe Request)
requestRedirectedFrom request = do
  initializer <- requestInitializer request
  pure
    ( fmap
        (requestFromRef (channelConnection (requestChannel request)))
        (requestInitializerRedirectedFrom initializer)
    )

requestRedirectedTo :: Request -> IO (Maybe Request)
requestRedirectedTo request = do
  let
    conn = channelConnection (requestChannel request)
    fromGuid = channelGuid (requestChannel request)
  objects <- traverse (lookupObject conn) =<< guidsOfType conn ChannelTypeRequest
  pure
    ( listToMaybe
        ( mapMaybe
            ( \obj ->
                case fromJsonValue (remoteInitializer obj) of
                  Right initializer
                    | Just ref <- requestInitializerRedirectedFrom initializer
                    , channelRefGuid ref == fromGuid ->
                        Just (requestFromRef conn (ChannelRef (remoteGuid obj)))
                  _ ->
                    Nothing
            )
            objects
        )
    )

requestResponseRef :: Request -> IO (Maybe ChannelRef)
requestResponseRef request = do
  result <-
    send
      (requestChannel request)
      (MethodName "response")
      mkRequestResponseParams
  pure (requestResponseResultResponse result)

requestSizes :: Request -> IO NetworkSizes
requestSizes request = do
  responseRef <- requestResponseRef request
  case responseRef of
    Nothing ->
      throwIO (DecodeFailure "Unable to fetch sizes for failed request")
    Just ref -> do
      let conn = channelConnection (requestChannel request)
      result <-
        send
          (Channel conn (channelRefGuid ref) :: Channel ResponseTag)
          (MethodName "sizes")
          mkResponseSizesParams
      let
        sizes = responseSizesResultSizes result
      pure
        ( NetworkSizes
            (BodySize (requestSizesRequestBodySize sizes))
            (HeadersSize (requestSizesRequestHeadersSize sizes))
            (BodySize (requestSizesResponseBodySize sizes))
            (HeadersSize (requestSizesResponseHeadersSize sizes))
        )

fromNameValue :: NameValue -> HttpHeader
fromNameValue header =
  HttpHeader (HeaderName (nameValueName header)) (HeaderValue (nameValueValue header))
