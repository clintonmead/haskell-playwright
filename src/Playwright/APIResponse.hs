module Playwright.APIResponse
  ( APIResponse (..)
  , apiResponseBody
  , apiResponseDispose
  , apiResponseFetchUid
  , apiResponseHeaders
  , apiResponseHeadersArray
  , apiResponseJson
  , apiResponseOk
  , apiResponseStatus
  , apiResponseStatusText
  , apiResponseText
  , apiResponseUrl
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
import Data.List
  ( partition
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
  , APIResponse (aPIResponseFetchUid, aPIResponseHeaders, aPIResponseStatus, aPIResponseStatusText, aPIResponseUrl)
  , NameValue (nameValueName, nameValueValue)
  , mkAPIRequestContextDisposeAPIResponseParams
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
  collapseHeaders (fmap lowercaseName (apiResponseHeadersArray response))

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

apiResponseJson :: APIResponse -> IO Value
apiResponseJson response = do
  bytes <- apiResponseBody response
  case eitherDecodeStrict bytes of
    Left err ->
      throwIO (DecodeFailure (pack err))
    Right value ->
      pure value

apiResponseDispose :: APIResponse -> IO ()
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
    mergedValue =
      intercalate
        ", "
        (fmap (headerValueToText . httpHeaderValue) (header : same))
  in
    HttpHeader (httpHeaderName header) (HeaderValue mergedValue) : collapseHeaders others
