module Playwright.Response
  ( Response (..)
  , responseFromRef
  , responseAllHeaders
  , responseBody
  , responseHeaders
  , responseJSON
  , responseOk
  , responseRequest
  , responseStatus
  , responseStatusText
  , responseText
  , responseUrl
  , responseFromServiceWorker
  , requestResponse
  ) where

import Playwright.Monad
  ( Playwright
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
import Data.Text
  ( Text
  , intercalate
  , toLower
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Playwright.Channel
  ( Channel (Channel, channelGuid)
  , ResponseTag
  , send
  )
import Playwright.Connection
  ( lookupObject
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
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.Json.ProtocolBinary
  ( protocolBinaryToByteString
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( NameValue (nameValueName, nameValueValue)
  , ResponseBodyResult (responseBodyResultBinary)
  , ResponseInitializer (responseInitializerFromServiceWorker, responseInitializerHeaders, responseInitializerRequest, responseInitializerStatus, responseInitializerStatusText, responseInitializerUrl)
  , ResponseRawResponseHeadersResult (responseRawResponseHeadersResultHeaders)
  , mkResponseBodyParams
  , mkResponseRawResponseHeadersParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.Request
  ( Request
  , requestFromRef
  , requestResponseRef
  )
import Playwright.Url
  ( Url
  )
import UnliftIO.Exception
  ( throwIO
  )

newtype Response = Response
  { responseChannel :: Channel ResponseTag
  }

responseFromRef :: ChannelRef -> Response
responseFromRef ref =
  Response (Channel (channelRefGuid ref))

responseInitializer :: Response -> Playwright ResponseInitializer
responseInitializer response = do
  obj <- lookupObject (channelGuid (responseChannel response))
  throwDecode (remoteInitializer obj)

responseUrl :: Response -> Playwright Url
responseUrl response =
  responseInitializerUrl <$> responseInitializer response

responseRequest :: Response -> Playwright Request
responseRequest response = do
  initializer <- responseInitializer response
  pure (requestFromRef (responseInitializerRequest initializer))

requestResponse :: Request -> Playwright (Maybe Response)
requestResponse request = do
  ref <- requestResponseRef request
  pure (fmap (responseFromRef) ref)

responseStatus :: Response -> Playwright HttpStatus
responseStatus response =
  HttpStatus . responseInitializerStatus <$> responseInitializer response

responseStatusText :: Response -> Playwright Text
responseStatusText response =
  responseInitializerStatusText <$> responseInitializer response

responseHeaders :: Response -> Playwright [HttpHeader]
responseHeaders response =
  fmap (fmap fromNameValue . responseInitializerHeaders) (responseInitializer response)

-- | The headers the browser received, including ones the initializer leaves out
-- (such as content-encoding). Names are lowercased, and repeated values are
-- joined as upstream allHeaders() does.
responseAllHeaders :: Response -> Playwright [HttpHeader]
responseAllHeaders response = do
  result :: ResponseRawResponseHeadersResult <-
    send
      (responseChannel response)
      (MethodName "rawResponseHeaders")
      mkResponseRawResponseHeadersParams
  pure
    ( collapseHeaders
        (fmap (lowercaseHeader . fromNameValue) (responseRawResponseHeadersResultHeaders result))
    )

responseOk :: Response -> Playwright Bool
responseOk response =
  httpStatusOk <$> responseStatus response

responseFromServiceWorker :: Response -> Playwright Bool
responseFromServiceWorker response =
  responseInitializerFromServiceWorker <$> responseInitializer response

responseBody :: Response -> Playwright ByteString
responseBody response = do
  result <-
    send
      (responseChannel response)
      (MethodName "body")
      mkResponseBodyParams
  pure (protocolBinaryToByteString (responseBodyResultBinary result))

responseText :: Response -> Playwright Text
responseText response =
  decodeUtf8 <$> responseBody response

responseJSON :: Response -> Playwright Value
responseJSON response = do
  bytes <- responseBody response
  case eitherDecodeStrict bytes of
    Right value ->
      pure value
    Left _ ->
      throwIO (DecodeFailure ("response body is not valid JSON: " <> decodeUtf8 bytes))

fromNameValue :: NameValue -> HttpHeader
fromNameValue header =
  HttpHeader (HeaderName (nameValueName header)) (HeaderValue (nameValueValue header))

lowercaseHeader :: HttpHeader -> HttpHeader
lowercaseHeader header =
  HttpHeader
    (HeaderName (toLower (headerNameToText (httpHeaderName header))))
    (httpHeaderValue header)

-- | Joins the values of each name in first-seen order. Set-Cookie values are
-- joined by newlines, since they may contain commas.
collapseHeaders :: [HttpHeader] -> [HttpHeader]
collapseHeaders [] =
  []
collapseHeaders (header : rest) =
  let
    name = headerNameToText (httpHeaderName header)
    (same, others) = partition (\other -> headerNameToText (httpHeaderName other) == name) rest
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
