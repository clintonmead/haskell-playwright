module Playwright.APIResponse
  ( APIResponse (..)
  , apiResponseBody
  , apiResponseFetchUid
  , apiResponseHeaders
  , apiResponseHeadersArray
  , apiResponseOk
  , apiResponseStatus
  , apiResponseStatusText
  , apiResponseText
  , apiResponseUrl
  ) where

import Control.Exception
  ( throwIO
  )
import Data.ByteString
  ( ByteString
  )
import Data.Text
  ( Text
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
  , HeaderValue (HeaderValue)
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
  ( APIRequestContextFetchResponseBodyResult (aPIRequestContextFetchResponseBodyResultBinary)
  , APIResponse (aPIResponseFetchUid, aPIResponseHeaders, aPIResponseStatus, aPIResponseStatusText, aPIResponseUrl)
  , NameValue (nameValueName, nameValueValue)
  , mkAPIRequestContextFetchResponseBodyParams
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

apiResponseHeadersArray :: APIResponse -> [HttpHeader]
apiResponseHeadersArray response =
  fmap fromNameValue (Protocol.aPIResponseHeaders (apiResponsePayload response))

apiResponseHeaders :: APIResponse -> [HttpHeader]
apiResponseHeaders response =
  fmap lowercaseName (apiResponseHeadersArray response)

apiResponseBody :: APIResponse -> IO ByteString
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

apiResponseText :: APIResponse -> IO Text
apiResponseText response =
  decodeUtf8 <$> apiResponseBody response

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
