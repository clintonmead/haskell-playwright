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
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Playwright.Channel
  ( Channel (Channel, channelConnection, channelGuid)
  , ResponseTag
  , send
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
import Playwright.HttpStatus
  ( HttpStatus (HttpStatus)
  , httpStatusOk
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
  , ResponseBodyResult (responseBodyResultBinary)
  , ResponseInitializer (responseInitializerHeaders, responseInitializerRequest, responseInitializerStatus, responseInitializerStatusText, responseInitializerUrl)
  , mkResponseBodyParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.Request
  ( Request
  , requestFromRef
  )
import Playwright.Url
  ( Url
  )

newtype Response = Response
  { responseChannel :: Channel ResponseTag
  }

responseFromRef :: Connection -> ChannelRef -> Response
responseFromRef conn ref =
  Response (Channel conn (channelRefGuid ref))

responseInitializer :: Response -> IO ResponseInitializer
responseInitializer response = do
  obj <- lookupObject (channelConnection (responseChannel response)) (channelGuid (responseChannel response))
  either throwIO pure (fromJsonValue (remoteInitializer obj))

responseUrl :: Response -> IO Url
responseUrl response =
  responseInitializerUrl <$> responseInitializer response

responseRequest :: Response -> IO Request
responseRequest response = do
  initializer <- responseInitializer response
  pure (requestFromRef (channelConnection (responseChannel response)) (responseInitializerRequest initializer))

responseStatus :: Response -> IO HttpStatus
responseStatus response =
  HttpStatus . responseInitializerStatus <$> responseInitializer response

responseStatusText :: Response -> IO Text
responseStatusText response =
  responseInitializerStatusText <$> responseInitializer response

responseHeaders :: Response -> IO [HttpHeader]
responseHeaders response =
  fmap (fmap fromNameValue . responseInitializerHeaders) (responseInitializer response)

responseOk :: Response -> IO Bool
responseOk response =
  httpStatusOk <$> responseStatus response

responseText :: Response -> IO Text
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
