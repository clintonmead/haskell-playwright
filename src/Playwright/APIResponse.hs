module Playwright.APIResponse
  ( APIResponse (..)
  , apiResponseBody
  , apiResponseDispose
  , apiResponseFetchUid
  , apiResponseHeaders
  , apiResponseHeadersArray
  , apiResponseJson
  , apiResponseOk
  , apiResponseSecurityDetails
  , apiResponseServerAddr
  , apiResponseStatus
  , apiResponseStatusText
  , apiResponseText
  , apiResponseTiming
  , apiResponseUrl
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
import Data.List
  ( partition
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.Scientific
  ( Scientific
  )
import Data.Text
  ( Text
  , intercalate
  , pack
  , toLower
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Playwright.Channel
  ( APIRequestContextTag
  , Channel
  , send
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.HttpHeader
  ( HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  )
import Playwright.HttpStatus
  ( HttpStatus (HttpStatus)
  , httpStatusOk
  )
import Playwright.Json.ProtocolBinary
  ( protocolBinaryToByteString
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol qualified as Protocol
  ( APIRequestContextDisposeAPIResponseResult
  , APIRequestContextFetchResponseBodyResult (aPIRequestContextFetchResponseBodyResultBinary)
  , APIResponse (aPIResponseFetchUid, aPIResponseHeaders, aPIResponseResponseEndTiming, aPIResponseSecurityDetails, aPIResponseServerAddr, aPIResponseStatus, aPIResponseStatusText, aPIResponseTiming, aPIResponseUrl)
  , NameValue (nameValueName, nameValueValue)
  , ResourceTiming
  , mkAPIRequestContextDisposeAPIResponseParams
  , mkAPIRequestContextFetchResponseBodyParams
  , resourceTimingConnectEnd
  , resourceTimingConnectStart
  , resourceTimingDomainLookupEnd
  , resourceTimingDomainLookupStart
  , resourceTimingRequestStart
  , resourceTimingResponseStart
  , resourceTimingSecureConnectionStart
  , resourceTimingStartTime
  )
import Playwright.RemoteAddr
  ( RemoteAddr
  , fromProtocolRemoteAddr
  )
import Playwright.SecurityDetails
  ( SecurityDetails
  , fromProtocolSecurityDetails
  )
import Playwright.ResourceTiming
  ( ResourceTiming (ResourceTiming, resourceTimingConnectEnd, resourceTimingConnectStart, resourceTimingDomainLookupEnd, resourceTimingDomainLookupStart, resourceTimingRequestStart, resourceTimingResponseEnd, resourceTimingResponseStart, resourceTimingSecureConnectionStart, resourceTimingStartTime)
  )
import Playwright.Url
  ( Url
  )

data APIResponse = APIResponse
  { apiResponseChannel :: Channel APIRequestContextTag
  , apiResponsePayload :: Protocol.APIResponse
  }

apiResponseFetchUid :: APIResponse -> Text
apiResponseFetchUid =
  Protocol.aPIResponseFetchUid . apiResponsePayload

apiResponseUrl :: APIResponse -> Url
apiResponseUrl =
  Protocol.aPIResponseUrl . apiResponsePayload

apiResponseStatus :: APIResponse -> HttpStatus
apiResponseStatus =
  HttpStatus . Protocol.aPIResponseStatus . apiResponsePayload

apiResponseStatusText :: APIResponse -> Text
apiResponseStatusText =
  Protocol.aPIResponseStatusText . apiResponsePayload

apiResponseOk :: APIResponse -> Bool
apiResponseOk =
  httpStatusOk . apiResponseStatus

apiResponseServerAddr :: APIResponse -> Maybe RemoteAddr
apiResponseServerAddr =
  fmap fromProtocolRemoteAddr . Protocol.aPIResponseServerAddr . apiResponsePayload

apiResponseSecurityDetails :: APIResponse -> Maybe SecurityDetails
apiResponseSecurityDetails =
  fmap fromProtocolSecurityDetails . Protocol.aPIResponseSecurityDetails . apiResponsePayload

-- | Mirrors the JS client's @timing()': unset phases are -1, and the
-- response end comes from the separate @responseEndTiming@ field.
apiResponseTiming :: APIResponse -> ResourceTiming
apiResponseTiming response =
  let
    payload = apiResponsePayload response
    base =
      maybe unsetResourceTiming fromProtocolResourceTiming (Protocol.aPIResponseTiming payload)
  in
    base
      { resourceTimingResponseEnd =
          fromMaybe unsetTiming (Protocol.aPIResponseResponseEndTiming payload)
      }

unsetTiming :: Scientific
unsetTiming =
  -1

unsetResourceTiming :: ResourceTiming
unsetResourceTiming =
  ResourceTiming
    { resourceTimingStartTime = unsetTiming
    , resourceTimingDomainLookupStart = unsetTiming
    , resourceTimingDomainLookupEnd = unsetTiming
    , resourceTimingConnectStart = unsetTiming
    , resourceTimingSecureConnectionStart = unsetTiming
    , resourceTimingConnectEnd = unsetTiming
    , resourceTimingRequestStart = unsetTiming
    , resourceTimingResponseStart = unsetTiming
    , resourceTimingResponseEnd = unsetTiming
    }

fromProtocolResourceTiming :: Protocol.ResourceTiming -> ResourceTiming
fromProtocolResourceTiming timing =
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

apiResponseHeadersArray :: APIResponse -> [HttpHeader]
apiResponseHeadersArray response =
  fmap fromNameValue (Protocol.aPIResponseHeaders (apiResponsePayload response))

apiResponseHeaders :: APIResponse -> [HttpHeader]
apiResponseHeaders response =
  collapseHeaders (fmap lowercaseName (apiResponseHeadersArray response))

apiResponseBody :: APIResponse -> Playwright ByteString
apiResponseBody response = do
  result <-
    send
      (apiResponseChannel response)
      (MethodName "fetchResponseBody")
      (Protocol.mkAPIRequestContextFetchResponseBodyParams (apiResponseFetchUid response))
  case Protocol.aPIRequestContextFetchResponseBodyResultBinary result of
    Nothing ->
      throwIO (DecodeFailure "Response has been disposed")
    Just binary ->
      pure (protocolBinaryToByteString binary)

apiResponseText :: APIResponse -> Playwright Text
apiResponseText response =
  decodeUtf8 <$> apiResponseBody response

apiResponseJson :: APIResponse -> Playwright Value
apiResponseJson response = do
  bytes <- apiResponseBody response
  case eitherDecodeStrict bytes of
    Left err ->
      throwIO (DecodeFailure (pack err))
    Right value ->
      pure value

apiResponseDispose :: APIResponse -> Playwright ()
apiResponseDispose response = do
  _ :: Protocol.APIRequestContextDisposeAPIResponseResult <-
    send
      (apiResponseChannel response)
      (MethodName "disposeAPIResponse")
      (Protocol.mkAPIRequestContextDisposeAPIResponseParams (apiResponseFetchUid response))
  pure ()

fromNameValue :: Protocol.NameValue -> HttpHeader
fromNameValue header =
  HttpHeader
    (HeaderName (Protocol.nameValueName header))
    (HeaderValue (Protocol.nameValueValue header))

lowercaseName :: HttpHeader -> HttpHeader
lowercaseName header =
  HttpHeader
    (HeaderName (toLower (headerNameToText (httpHeaderName header))))
    (httpHeaderValue header)

collapseHeaders :: [HttpHeader] -> [HttpHeader]
collapseHeaders [] =
  []
collapseHeaders (header : rest) =
  let
    name = headerNameToText (httpHeaderName header)
    (same, others) = partition (\other -> headerNameToText (httpHeaderName other) == name) rest
    -- Set-Cookie values are joined with newlines, as the JS client's headers() does,
    -- so that fulfilling with this response keeps each cookie separate.
    separator :: Text
    separator =
      if name == "set-cookie"
        then "\n"
        else ", "
    mergedValue =
      intercalate
        separator
        (fmap (headerValueToText . httpHeaderValue) (header : same))
  in
    HttpHeader (httpHeaderName header) (HeaderValue mergedValue) : collapseHeaders others
