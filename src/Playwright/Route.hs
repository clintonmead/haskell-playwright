module Playwright.Route
  ( Route (Route)
  , abortRoute
  , continueRoute
  , continueRouteWith
  , fallbackRoute
  , fallbackRouteWith
  , fulfillRoute
  , requestFromChannel
  , routeFetch
  , routeFetchWith
  , routeRequest
  , sendContinue
  ) where

import Playwright.Monad
  ( Playwright
  )
import Control.Concurrent.STM
  ( TMVar
  , atomically
  , isEmptyTMVar
  , putTMVar
  , throwSTM
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import UnliftIO.Exception
  ( throwIO
  , try
  )
import Data.ByteString.Base64
  ( encode
  )
import Data.ByteString qualified as ByteString
import Data.Text
  ( Text
  , pack
  , toLower
  )
import Data.Text.Encoding
  ( decodeLatin1
  , encodeUtf8
  )
import Playwright.APIRequestContext
  ( apiRequestContextFor
  , fetchRequestWith
  )
import Playwright.APIResponse
  ( APIResponse
  , apiResponseFetchUid
  , apiResponseHeaders
  , apiResponseStatus
  )
import Playwright.Channel
  ( Channel (channelGuid)
  , RouteTag
  , send
  )
import Playwright.Connection
  ( lookupObject
  , lookupRequestOverrides
  , writeRequestOverrides
  )
import Playwright.ContinueOptions
  ( ContinueOptions
  , defaultContinueOptions
  )
import Playwright.Error
  ( PlaywrightError (MissingObject, RouteAlreadyHandled)
  , ignoreTargetClosed
  )
import Playwright.FetchOptions
  ( FetchOptions
  , defaultFetchOptions
  )
import Playwright.FulfillOptions
  ( ContentType (contentTypeToText)
  , FulfillBody (FulfillBytes, FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillHeaders, fulfillResponse, fulfillStatus)
  )
import Playwright.HttpHeader
  ( HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  )
import Playwright.HttpMethod
  ( HttpMethod (httpMethodToText)
  )
import Playwright.HttpStatus
  ( HttpStatus (httpStatusToInt)
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.Json.ProtocolBinary
  ( protocolBinaryFromByteString
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( NameValue
  , RouteAbortResult
  , RouteContinueParams (routeContinueParamsHeaders, routeContinueParamsMethod, routeContinueParamsPostData, routeContinueParamsUrl)
  , RouteContinueResult
  , RouteFulfillParams (routeFulfillParamsBody, routeFulfillParamsFetchResponseUid, routeFulfillParamsHeaders, routeFulfillParamsIsBase64, routeFulfillParamsStatus)
  , RouteFulfillResult
  , RouteInitializer (routeInitializerRequest)
  , mkNameValue
  , mkRouteAbortParams
  , mkRouteContinueParams
  , mkRouteFulfillParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.Request
  ( Request (requestChannel)
  , requestFromRef
  )
import Playwright.RequestOverrides
  ( RequestOverrides (requestOverrideHeaders, requestOverrideMethod, requestOverridePostData, requestOverrideUrl)
  , emptyRequestOverrides
  , overlayContinueOptions
  )

data Route = Route
  { routeChannel :: Channel RouteTag
  , routeOutcome :: TMVar Bool
  }

routeRequest :: Route -> Playwright Request
routeRequest handled =
  requestFromChannel (routeChannel handled)

requestFromChannel :: Channel RouteTag -> Playwright Request
requestFromChannel chan = do
  obj <- lookupObject (channelGuid chan)
  initializer <- throwDecode (remoteInitializer obj)
  pure (requestFromRef (routeInitializerRequest initializer))

continueRoute :: Route -> Playwright ()
continueRoute handled =
  continueRouteWith handled defaultContinueOptions

continueRouteWith :: Route -> ContinueOptions -> Playwright ()
continueRouteWith handled opts = do
  overrides <- continueOverrides handled opts
  sendContinue (routeChannel handled) False overrides
  settleRoute handled True

fallbackRoute :: Route -> Playwright ()
fallbackRoute handled =
  fallbackRouteWith handled defaultContinueOptions

fallbackRouteWith :: Route -> ContinueOptions -> Playwright ()
fallbackRouteWith handled opts = do
  _ <- continueOverrides handled opts
  settleRoute handled False

continueOverrides :: Route -> ContinueOptions -> Playwright RequestOverrides
continueOverrides handled opts = do
  result <- try (routeRequest handled)
  case result of
    Left (MissingObject _) ->
      pure emptyRequestOverrides
    Left err ->
      throwIO err
    Right request ->
      applyContinueOverrides request opts

applyContinueOverrides :: Request -> ContinueOptions -> Playwright RequestOverrides
applyContinueOverrides request opts = do
  let
    guid = channelGuid (requestChannel request)
  current <- lookupRequestOverrides guid
  let
    next = overlayContinueOptions current opts
  writeRequestOverrides guid next
  pure next

abortRoute :: Route -> Playwright ()
abortRoute handled = do
  ignoreTargetClosed $ do
    _ :: RouteAbortResult <-
      send (routeChannel handled) (MethodName "abort") mkRouteAbortParams
    pure ()
  settleRoute handled True

fulfillRoute :: Route -> FulfillOptions -> Playwright ()
fulfillRoute handled opts = do
  let
    (bodyText, isBase64, byteLength) = encodeFulfillBody (fulfillBody opts)
    fetchUid = fulfillFetchUid opts
    status = fulfillStatusCode opts
    headers = fulfillHeadersWithDefaults opts (fulfillBaseHeaders opts) byteLength
  ignoreTargetClosed $ do
    _ :: RouteFulfillResult <-
      send
        (routeChannel handled)
        (MethodName "fulfill")
        mkRouteFulfillParams
          { routeFulfillParamsBody = bodyText
          , routeFulfillParamsFetchResponseUid = fetchUid
          , routeFulfillParamsHeaders = Just headers
          , routeFulfillParamsIsBase64 = Just isBase64
          , routeFulfillParamsStatus = Just status
          }
    pure ()
  settleRoute handled True

routeFetch :: Route -> Playwright APIResponse
routeFetch handled =
  routeFetchWith handled defaultFetchOptions

routeFetchWith :: Route -> FetchOptions -> Playwright APIResponse
routeFetchWith handled opts = do
  ctx <-
    apiRequestContextFor
      (channelGuid (routeChannel handled))
  request <- routeRequest handled
  fetchRequestWith ctx request opts

settleRoute :: Route -> Bool -> Playwright ()
settleRoute handled settled =
  liftIO $
    atomically $ do
    empty <- isEmptyTMVar (routeOutcome handled)
    if empty
      then putTMVar (routeOutcome handled) settled
      else throwSTM RouteAlreadyHandled

sendContinue :: Channel RouteTag -> Bool -> RequestOverrides -> Playwright ()
sendContinue chan isFallback overrides =
  ignoreTargetClosed $ do
    _ :: RouteContinueResult <-
      send
        chan
        (MethodName "continue")
        (mkRouteContinueParams isFallback)
          { routeContinueParamsHeaders = fmap (fmap toNameValue) (requestOverrideHeaders overrides)
          , routeContinueParamsMethod = fmap httpMethodToText (requestOverrideMethod overrides)
          , routeContinueParamsPostData = fmap protocolBinaryFromByteString (requestOverridePostData overrides)
          , routeContinueParamsUrl = requestOverrideUrl overrides
          }
    pure ()

encodeFulfillBody :: Maybe FulfillBody -> (Maybe Text, Bool, Int)
encodeFulfillBody body =
  case body of
    Nothing ->
      (Nothing, False, 0)
    Just (FulfillText text) ->
      (Just text, False, ByteString.length (encodeUtf8 text))
    Just (FulfillBytes bytes) ->
      (Just (decodeLatin1 (encode bytes)), True, ByteString.length bytes)

fulfillFetchUid :: FulfillOptions -> Maybe Text
fulfillFetchUid opts =
  case (fulfillBody opts, fulfillResponse opts) of
    (Nothing, Just response) ->
      Just (apiResponseFetchUid response)
    _ ->
      Nothing

fulfillStatusCode :: FulfillOptions -> Int
fulfillStatusCode opts =
  case fulfillStatus opts of
    Just status ->
      httpStatusToInt status
    Nothing ->
      case fulfillResponse opts of
        Just response ->
          httpStatusToInt (apiResponseStatus response)
        Nothing ->
          200

fulfillBaseHeaders :: FulfillOptions -> [HttpHeader]
fulfillBaseHeaders opts =
  case fulfillHeaders opts of
    [] ->
      case fulfillResponse opts of
        Just response ->
          apiResponseHeaders response
        Nothing ->
          []
    headers ->
      headers

fulfillHeadersWithDefaults :: FulfillOptions -> [HttpHeader] -> Int -> [NameValue]
fulfillHeadersWithDefaults opts baseHeaders byteLength =
  fmap toNameValue (withLength (withContentType (fmap lowercaseHeader baseHeaders)))
  where
    withContentType headers =
      case fulfillContentType opts of
        Nothing ->
          headers
        Just contentType ->
          replaceHeader "content-type" (contentTypeToText contentType) headers
    withLength headers =
      if byteLength == 0 || hasHeader "content-length" headers
        then headers
        else replaceHeader "content-length" (pack (show byteLength)) headers

lowercaseHeader :: HttpHeader -> HttpHeader
lowercaseHeader header =
  HttpHeader
    (HeaderName (toLower (headerNameToText (httpHeaderName header))))
    (httpHeaderValue header)

replaceHeader :: Text -> Text -> [HttpHeader] -> [HttpHeader]
replaceHeader name value headers =
  HttpHeader (HeaderName name) (HeaderValue value)
    : filter (not . headerNamed name) headers

hasHeader :: Text -> [HttpHeader] -> Bool
hasHeader name =
  any (headerNamed name)

headerNamed :: Text -> HttpHeader -> Bool
headerNamed name header =
  toLower (headerNameToText (httpHeaderName header)) == name

toNameValue :: HttpHeader -> NameValue
toNameValue header =
  mkNameValue (headerNameToText (httpHeaderName header)) (headerValueToText (httpHeaderValue header))
