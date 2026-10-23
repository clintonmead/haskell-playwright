module Playwright.Request
  ( Request (..)
  , requestFromRef
  , requestAllHeaders
  , requestFrameRef
  , requestFailure
  , requestHeaders
  , requestHeaderValue
  , requestHeadersArray
  , requestIsNavigation
  , requestMethod
  , requestPostData
  , requestPostDataJSON
  , requestRedirectedFrom
  , requestRedirectedTo
  , requestResourceType
  , requestResponseRef
  , requestServiceWorkerRef
  , requestSizes
  , requestTiming
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
  ( Value (String)
  , eitherDecodeStrict
  , object
  , (.=)
  )
import Data.Aeson.Key qualified as Key
import Data.ByteString.Char8 qualified as BS8
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Data.ByteString qualified as ByteString
import Data.Word
  ( Word8
  )
import Data.ByteString
  ( ByteString
  )
import Data.Maybe
  ( fromMaybe
  , listToMaybe
  , mapMaybe
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Data.Scientific
  ( Scientific
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
  , lookupRequestEndTiming
  , lookupRequestFailure
  , lookupRequestOverrides
  )
import Playwright.HttpHeader
  ( HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
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
  , throwDecode
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
  , RequestInitializer (requestInitializerFrame, requestInitializerHeaders, requestInitializerIsNavigationRequest, requestInitializerMethod, requestInitializerPostData, requestInitializerRedirectedFrom, requestInitializerResourceType, requestInitializerServiceWorker, requestInitializerUrl)
  , RequestRawRequestHeadersResult (requestRawRequestHeadersResultHeaders)
  , RequestResponseResult (requestResponseResultResponse)
  , RequestSizes (requestSizesRequestBodySize, requestSizesRequestHeadersSize, requestSizesResponseBodySize, requestSizesResponseHeadersSize)
  , ResponseInitializer (responseInitializerTiming)
  , ResponseSizesResult (responseSizesResultSizes)
  , mkRequestRawRequestHeadersParams
  , mkRequestResponseParams
  , mkResponseSizesParams
  )
import Playwright.Protocol qualified as Protocol
  ( ResourceTiming
  , resourceTimingConnectEnd
  , resourceTimingConnectStart
  , resourceTimingDomainLookupEnd
  , resourceTimingDomainLookupStart
  , resourceTimingRequestStart
  , resourceTimingResponseStart
  , resourceTimingSecureConnectionStart
  , resourceTimingStartTime
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
import Playwright.ResourceTiming
  ( ResourceTiming (ResourceTiming, resourceTimingConnectEnd, resourceTimingConnectStart, resourceTimingDomainLookupEnd, resourceTimingDomainLookupStart, resourceTimingRequestStart, resourceTimingResponseEnd, resourceTimingResponseStart, resourceTimingSecureConnectionStart, resourceTimingStartTime)
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
  throwDecode (remoteInitializer obj)

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
    Just bytes -> do
      headers <- requestHeaders request
      if isFormPost headers
        then pure (Just (formObject bytes))
        else
          case eitherDecodeStrict bytes of
            Right value ->
              pure (Just value)
            Left _ ->
              throwIO
                ( DecodeFailure
                    ("POST data is not a valid JSON object: " <> decodeUtf8 bytes)
                )

-- | Form posts are read as an object of their fields, as the JS client does.
isFormPost :: [HttpHeader] -> Bool
isFormPost headers =
  any
    (Text.isPrefixOf "application/x-www-form-urlencoded" . Text.toLower . headerValueToText . httpHeaderValue)
    [header | header <- headers, Text.toLower (headerNameToText (httpHeaderName header)) == "content-type"]

-- | The fields of a urlencoded body. A repeated field keeps its last value.
formObject :: ByteString -> Value
formObject bytes =
  object [Key.fromText name .= String value | (name, value) <- formFields bytes]

formFields :: ByteString -> [(Text, Text)]
formFields bytes =
  [ (decode key, decode (BS8.drop 1 rest))
  | pair <- BS8.split '&' bytes
  , not (BS8.null pair)
  , let (key, rest) = BS8.break (== '=') pair
  ]
  where
    decode = formDecode

-- | Decodes one urlencoded name or value: '+' is a space, %XX is a byte.
formDecode :: ByteString -> Text
formDecode = decodeUtf8 . ByteString.pack . decodeBytes . ByteString.unpack

decodeBytes :: [Word8] -> [Word8]
decodeBytes (43 : rest) = 32 : decodeBytes rest
decodeBytes (37 : high : low : rest)
  | Just h <- hexDigit high
  , Just l <- hexDigit low =
      (h * 16 + l) : decodeBytes rest
decodeBytes (byte : rest) = byte : decodeBytes rest
decodeBytes [] = []

hexDigit :: Word8 -> Maybe Word8
hexDigit byte
  | byte >= 48 && byte <= 57 = Just (byte - 48)
  | byte >= 65 && byte <= 70 = Just (byte - 55)
  | byte >= 97 && byte <= 102 = Just (byte - 87)
  | otherwise = Nothing

requestHeaders :: Request -> Playwright [HttpHeader]
requestHeaders request = do
  overrides <- requestOverrides request
  case requestOverrideHeaders overrides of
    Just headers ->
      pure headers
    Nothing ->
      fmap (fmap fromNameValue . requestInitializerHeaders) (requestInitializer request)

-- | The request headers as sent, one entry per line, so a repeated name
-- appears once for each line. Names are as the browser sent them.
requestHeadersArray :: Request -> Playwright [HttpHeader]
requestHeadersArray =
  requestHeaders

-- | The merged value of one request header, looked up case-insensitively.
-- Repeated lines are joined with ', ' as upstream headerValue() does.
requestHeaderValue :: Request -> HeaderName -> Playwright (Maybe Text)
requestHeaderValue request name = do
  headers <- requestHeadersArray request
  let
    wanted = Text.toLower (headerNameToText name)
    values =
      [ headerValueToText (httpHeaderValue header)
      | header <- headers
      , Text.toLower (headerNameToText (httpHeaderName header)) == wanted
      ]
  pure
    ( if null values
        then Nothing
        else Just (Text.intercalate ", " values)
    )

requestAllHeaders :: Request -> Playwright [HttpHeader]
requestAllHeaders request = do
  result <-
    send
      (requestChannel request)
      (MethodName "rawRequestHeaders")
      mkRequestRawRequestHeadersParams
  pure (fmap fromNameValue (requestRawRequestHeadersResultHeaders result))

requestFrameRef :: Request -> Playwright (Maybe ChannelRef)
requestFrameRef request =
  requestInitializerFrame <$> requestInitializer request

requestServiceWorkerRef :: Request -> Playwright (Maybe ChannelRef)
requestServiceWorkerRef request =
  requestInitializerServiceWorker <$> requestInitializer request

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

requestTiming :: Request -> Playwright ResourceTiming
requestTiming request = do
  responseRef <- requestResponseRef request
  base <-
    case responseRef of
      Nothing ->
        pure emptyTiming
      Just ref -> do
        obj <- lookupObject (channelRefGuid ref)
        initializer <- throwDecode (remoteInitializer obj)
        pure (fromProtocolTiming (responseInitializerTiming initializer))
  endTiming <- lookupRequestEndTiming (channelGuid (requestChannel request))
  pure (applyResponseEnd base (fromMaybe unsetTiming endTiming))

fromProtocolTiming :: Protocol.ResourceTiming -> ResourceTiming
fromProtocolTiming timing =
  ResourceTiming
    { resourceTimingStartTime = Protocol.resourceTimingStartTime timing
    , resourceTimingDomainLookupStart = Protocol.resourceTimingDomainLookupStart timing
    , resourceTimingDomainLookupEnd = Protocol.resourceTimingDomainLookupEnd timing
    , resourceTimingConnectStart = Protocol.resourceTimingConnectStart timing
    , resourceTimingSecureConnectionStart = Protocol.resourceTimingSecureConnectionStart timing
    , resourceTimingConnectEnd = Protocol.resourceTimingConnectEnd timing
    , resourceTimingRequestStart = Protocol.resourceTimingRequestStart timing
    , resourceTimingResponseStart = Protocol.resourceTimingResponseStart timing
    , resourceTimingResponseEnd = unsetTiming
    }

applyResponseEnd :: ResourceTiming -> Scientific -> ResourceTiming
applyResponseEnd timing end =
  timing
    { resourceTimingResponseEnd = end
    , resourceTimingResponseStart =
        if resourceTimingResponseStart timing == unsetTiming
          then end
          else resourceTimingResponseStart timing
    }

emptyTiming :: ResourceTiming
emptyTiming =
  ResourceTiming
    { resourceTimingStartTime = 0
    , resourceTimingDomainLookupStart = unsetTiming
    , resourceTimingDomainLookupEnd = unsetTiming
    , resourceTimingConnectStart = unsetTiming
    , resourceTimingSecureConnectionStart = unsetTiming
    , resourceTimingConnectEnd = unsetTiming
    , resourceTimingRequestStart = unsetTiming
    , resourceTimingResponseStart = unsetTiming
    , resourceTimingResponseEnd = unsetTiming
    }

unsetTiming :: Scientific
unsetTiming =
  -1

fromNameValue :: NameValue -> HttpHeader
fromNameValue header =
  HttpHeader (HeaderName (nameValueName header)) (HeaderValue (nameValueValue header))
