module Playwright.Response
  ( Response (..)
  , responseFromRef
  , responseHeaders
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
import Data.Text
  ( Text
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
import Playwright.HttpHeader
  ( HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader)
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
  , mkResponseBodyParams
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

responseOk :: Response -> Playwright Bool
responseOk response =
  httpStatusOk <$> responseStatus response

responseFromServiceWorker :: Response -> Playwright Bool
responseFromServiceWorker response =
  responseInitializerFromServiceWorker <$> responseInitializer response

responseText :: Response -> Playwright Text
responseText response = do
  result <-
    send
      (responseChannel response)
      (MethodName "body")
      mkResponseBodyParams
  pure (decodeUtf8 (protocolBinaryToByteString (responseBodyResultBinary result)))

fromNameValue :: NameValue -> HttpHeader
fromNameValue header =
  HttpHeader (HeaderName (nameValueName header)) (HeaderValue (nameValueValue header))
