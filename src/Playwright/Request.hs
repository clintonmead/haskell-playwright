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

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
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
  ( Channel (Channel, channelGuid)
  , RequestTag
  , ResponseTag
  , send
  )
import Playwright.Connection
  ( guidsOfType
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

requestFromRef :: ChannelRef -> Request
requestFromRef ref =
  Request (Channel (channelRefGuid ref))

requestsEqual :: Request -> Request -> Bool
requestsEqual left right =
  channelGuid (requestChannel left) == channelGuid (requestChannel right)

requestInitializer :: Request -> Playwright RequestInitializer
requestInitializer request = do
  obj <- lookupObject (channelGuid (requestChannel request))
  either throwIO pure (fromJsonValue (remoteInitializer obj))

requestOverrides :: Request -> Playwright RequestOverrides
requestOverrides request =
  lookupRequestOverrides
    (channelGuid (requestChannel request))

requestUrl :: Request -> Playwright Url
requestUrl request = do
  overrides <- requestOverrides request
  case requestOverrideUrl overrides of
    Just url ->
      pure url
    Nothing ->
      requestInitializerUrl <$> requestInitializer request

requestMethod :: Request -> Playwright HttpMethod
requestMethod request = do
  overrides <- requestOverrides request
  case requestOverrideMethod overrides of
    Just method ->
      pure method
    Nothing ->
      HttpMethod . requestInitializerMethod <$> requestInitializer request

requestResourceType :: Request -> Playwright ResourceType
requestResourceType request =
  ResourceType . requestInitializerResourceType <$> requestInitializer request

requestFailure :: Request -> Playwright (Maybe RequestFailure)
requestFailure request = do
  text <-
    lookupRequestFailure
      (channelGuid (requestChannel request))
  case text of
    Just value | value /= mempty ->
      pure (Just (RequestFailure value))
    _ ->
      pure Nothing

requestIsNavigation :: Request -> Playwright Bool
requestIsNavigation request =
  requestInitializerIsNavigationRequest <$> requestInitializer request

requestPostData :: Request -> Playwright (Maybe ByteString)
requestPostData request = do
  overrides <- requestOverrides request
  case requestOverridePostData overrides of
    Just bytes ->
      pure (Just bytes)
    Nothing ->
      fmap (fmap protocolBinaryToByteString . requestInitializerPostData) (requestInitializer request)

requestPostDataJSON :: Request -> Playwright (Maybe Value)
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

requestHeaders :: Request -> Playwright [HttpHeader]
requestHeaders request = do
  overrides <- requestOverrides request
  case requestOverrideHeaders overrides of
    Just headers ->
      pure headers
    Nothing ->
      fmap (fmap fromNameValue . requestInitializerHeaders) (requestInitializer request)

requestFrameRef :: Request -> Playwright (Maybe ChannelRef)
requestFrameRef request =
  requestInitializerFrame <$> requestInitializer request

requestRedirectedFrom :: Request -> Playwright (Maybe Request)
requestRedirectedFrom request = do
  initializer <- requestInitializer request
  pure
    ( fmap
        (requestFromRef)
        (requestInitializerRedirectedFrom initializer)
    )

requestRedirectedTo :: Request -> Playwright (Maybe Request)
requestRedirectedTo request = do
  let
    fromGuid = channelGuid (requestChannel request)
  objects <- traverse (lookupObject) =<< guidsOfType ChannelTypeRequest
  pure
    ( listToMaybe
        ( mapMaybe
            ( \obj ->
                case fromJsonValue (remoteInitializer obj) of
                  Right initializer
                    | Just ref <- requestInitializerRedirectedFrom initializer
                    , channelRefGuid ref == fromGuid ->
                        Just (requestFromRef (ChannelRef (remoteGuid obj)))
                  _ ->
                    Nothing
            )
            objects
        )
    )

requestResponseRef :: Request -> Playwright (Maybe ChannelRef)
requestResponseRef request = do
  result <-
    send
      (requestChannel request)
      (MethodName "response")
      mkRequestResponseParams
  pure (requestResponseResultResponse result)

requestSizes :: Request -> Playwright NetworkSizes
requestSizes request = do
  responseRef <- requestResponseRef request
  case responseRef of
    Nothing ->
      throwIO (DecodeFailure "Unable to fetch sizes for failed request")
    Just ref -> do
      result <-
        send
          (Channel (channelRefGuid ref) :: Channel ResponseTag)
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
