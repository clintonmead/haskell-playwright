module Playwright.Route
  ( Route
  , abortRoute
  , continueRoute
  , continueRouteWith
  , fallbackRoute
  , fallbackRouteWith
  , fulfillRoute
  , route
  , routeFetch
  , routeFetchWith
  , routeRequest
  , routeWith
  , unroute
  , unrouteAll
  , contextRoute
  , contextRouteWith
  , unrouteContext
  , unrouteContextAll
  ) where

import Control.Concurrent.STM
  ( STM
  , TMVar
  , TVar
  , atomically
  , isEmptyTMVar
  , newEmptyTMVarIO
  , newTVarIO
  , putTMVar
  , readTVar
  , throwSTM
  , tryReadTMVar
  , writeTVar
  )
import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( unless
  , when
  )
import Data.Aeson
  ( Value
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
import Playwright.BrowserContext
  ( BrowserContext (browserContextChannel)
  )
import Playwright.Channel
  ( Channel (Channel, channelConnection, channelGuid)
  , RouteTag
  , send
  )
import Playwright.Connection
  ( Connection
  , lookupObject
  , lookupRequestOverrides
  , pushRouteHandler
  , registerEventHandler
  , removeRouteHandlers
  , clearRouteHandlers
  , routeHandlersFor
  , writeRequestOverrides
  )
import Playwright.ContinueOptions
  ( ContinueOptions
  , defaultContinueOptions
  )
import Playwright.Error
  ( PlaywrightError (RouteAlreadyHandled)
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
import Playwright.Guid
  ( Guid
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
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.Json.ProtocolBinary
  ( protocolBinaryFromByteString
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageChannel, pageContextChannel)
  )
import Playwright.Protocol
  ( BrowserContextRouteEvent (browserContextRouteEventRoute)
  , BrowserContextSetNetworkInterceptionPatternsParamsPatterns (browserContextSetNetworkInterceptionPatternsParamsPatternsGlob)
  , BrowserContextSetNetworkInterceptionPatternsResult
  , NameValue
  , PageRouteEvent (pageRouteEventRoute)
  , PageSetNetworkInterceptionPatternsParamsPatterns (pageSetNetworkInterceptionPatternsParamsPatternsGlob)
  , PageSetNetworkInterceptionPatternsResult
  , RouteAbortResult
  , RouteContinueParams (routeContinueParamsHeaders, routeContinueParamsMethod, routeContinueParamsPostData, routeContinueParamsUrl)
  , RouteContinueResult
  , RouteFulfillParams (routeFulfillParamsBody, routeFulfillParamsFetchResponseUid, routeFulfillParamsHeaders, routeFulfillParamsIsBase64, routeFulfillParamsStatus)
  , RouteFulfillResult
  , RouteInitializer (routeInitializerRequest)
  , mkBrowserContextSetNetworkInterceptionPatternsParams
  , mkBrowserContextSetNetworkInterceptionPatternsParamsPatterns
  , mkNameValue
  , mkPageSetNetworkInterceptionPatternsParams
  , mkPageSetNetworkInterceptionPatternsParamsPatterns
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
  , requestUrl
  )
import Playwright.RequestOverrides
  ( RequestOverrides (requestOverrideHeaders, requestOverrideMethod, requestOverridePostData, requestOverrideUrl)
  , overlayContinueOptions
  )
import Playwright.RouteOptions
  ( RouteOptions (routeTimes)
  , RouteTimes (routeTimesToInt)
  , defaultRouteOptions
  )
import Playwright.UrlGlob
  ( UrlGlob (urlGlobToText)
  , matchesUrlGlob
  )

data Route = Route
  { routeChannel :: Channel RouteTag
  , routeOutcome :: TMVar Bool
  }

routeRequest :: Route -> IO Request
routeRequest handled =
  requestFromChannel (routeChannel handled)

requestFromChannel :: Channel RouteTag -> IO Request
requestFromChannel chan = do
  obj <- lookupObject (channelConnection chan) (channelGuid chan)
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  pure (requestFromRef (channelConnection chan) (routeInitializerRequest initializer))

continueRoute :: Route -> IO ()
continueRoute handled =
  continueRouteWith handled defaultContinueOptions

continueRouteWith :: Route -> ContinueOptions -> IO ()
continueRouteWith handled opts = do
  request <- routeRequest handled
  overrides <- applyContinueOverrides request opts
  sendContinue (routeChannel handled) False overrides
  settleRoute handled True

fallbackRoute :: Route -> IO ()
fallbackRoute handled =
  fallbackRouteWith handled defaultContinueOptions

fallbackRouteWith :: Route -> ContinueOptions -> IO ()
fallbackRouteWith handled opts = do
  request <- routeRequest handled
  _ <- applyContinueOverrides request opts
  settleRoute handled False

applyContinueOverrides :: Request -> ContinueOptions -> IO RequestOverrides
applyContinueOverrides request opts = do
  let
    conn = channelConnection (requestChannel request)
    guid = channelGuid (requestChannel request)
  current <- lookupRequestOverrides conn guid
  let
    next = overlayContinueOptions current opts
  writeRequestOverrides conn guid next
  pure next

abortRoute :: Route -> IO ()
abortRoute handled = do
  _ :: RouteAbortResult <-
    send (routeChannel handled) (MethodName "abort") mkRouteAbortParams
  settleRoute handled True

fulfillRoute :: Route -> FulfillOptions -> IO ()
fulfillRoute handled opts = do
  let
    (bodyText, isBase64, byteLength) = encodeFulfillBody (fulfillBody opts)
    fetchUid = fulfillFetchUid opts
    status = fulfillStatusCode opts
    headers = fulfillHeadersWithDefaults opts (fulfillBaseHeaders opts) byteLength
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
  settleRoute handled True

routeFetch :: Route -> IO APIResponse
routeFetch handled =
  routeFetchWith handled defaultFetchOptions

routeFetchWith :: Route -> FetchOptions -> IO APIResponse
routeFetchWith handled opts = do
  ctx <-
    apiRequestContextFor
      (channelConnection (routeChannel handled))
      (channelGuid (routeChannel handled))
  request <- routeRequest handled
  fetchRequestWith ctx request opts

route :: Page -> UrlGlob -> (Route -> IO ()) -> IO ()
route page glob =
  routeWith page glob defaultRouteOptions

routeWith :: Page -> UrlGlob -> RouteOptions -> (Route -> IO ()) -> IO ()
routeWith page glob opts handler = do
  let
    conn = channelConnection (pageChannel page)
    guid = channelGuid (pageChannel page)
  first <- installRouteHandler conn guid glob opts handler
  when first (registerEventHandler conn guid (MethodName "route") (dispatchPageRoute page))
  updateInterceptionPatterns page

contextRoute :: BrowserContext -> UrlGlob -> (Route -> IO ()) -> IO ()
contextRoute ctx glob =
  contextRouteWith ctx glob defaultRouteOptions

contextRouteWith :: BrowserContext -> UrlGlob -> RouteOptions -> (Route -> IO ()) -> IO ()
contextRouteWith ctx glob opts handler = do
  let
    conn = channelConnection (browserContextChannel ctx)
    guid = channelGuid (browserContextChannel ctx)
  first <- installRouteHandler conn guid glob opts handler
  when first (registerEventHandler conn guid (MethodName "route") (dispatchContextRoute ctx))
  updateContextInterceptionPatterns ctx

installRouteHandler :: Connection -> Guid -> UrlGlob -> RouteOptions -> (Route -> IO ()) -> IO Bool
installRouteHandler conn guid glob opts handler = do
  remaining <- newTVarIO (fmap routeTimesToInt (routeTimes opts))
  pushRouteHandler conn guid glob $ \ref -> do
    live <- atomically (consumeRouteTime remaining)
    if live
      then do
        box <- newEmptyTMVarIO
        handler (Route (Channel conn (channelRefGuid ref)) box)
        outcome <- atomically (tryReadTMVar box)
        case outcome of
          Nothing ->
            pure True
          Just settled ->
            pure settled
      else
        pure False

consumeRouteTime :: TVar (Maybe Int) -> STM Bool
consumeRouteTime remaining = do
  current <- readTVar remaining
  case current of
    Nothing ->
      pure True
    Just count ->
      if count <= 0
        then pure False
        else do
          writeTVar remaining (Just (count - 1))
          pure True

unroute :: Page -> UrlGlob -> IO ()
unroute page glob = do
  removeRouteHandlers
    (channelConnection (pageChannel page))
    (channelGuid (pageChannel page))
    glob
  updateInterceptionPatterns page

unrouteAll :: Page -> IO ()
unrouteAll page = do
  clearRouteHandlers
    (channelConnection (pageChannel page))
    (channelGuid (pageChannel page))
  updateInterceptionPatterns page

unrouteContext :: BrowserContext -> UrlGlob -> IO ()
unrouteContext ctx glob = do
  removeRouteHandlers
    (channelConnection (browserContextChannel ctx))
    (channelGuid (browserContextChannel ctx))
    glob
  updateContextInterceptionPatterns ctx

unrouteContextAll :: BrowserContext -> IO ()
unrouteContextAll ctx = do
  clearRouteHandlers
    (channelConnection (browserContextChannel ctx))
    (channelGuid (browserContextChannel ctx))
  updateContextInterceptionPatterns ctx

dispatchPageRoute :: Page -> Value -> IO ()
dispatchPageRoute page params = do
  event <- either throwIO pure (fromJsonValue params)
  let
    conn = channelConnection (pageChannel page)
    chan :: Channel RouteTag
    chan = Channel conn (channelRefGuid (pageRouteEventRoute event))
  request <- requestFromChannel chan
  pageHandlers <- routeHandlersFor conn (channelGuid (pageChannel page))
  contextHandlers <- routeHandlersFor conn (channelGuid (pageContextChannel page))
  chainRoutes chan request (pageHandlers <> contextHandlers)

dispatchContextRoute :: BrowserContext -> Value -> IO ()
dispatchContextRoute ctx params = do
  event <- either throwIO pure (fromJsonValue params)
  let
    conn = channelConnection (browserContextChannel ctx)
    chan :: Channel RouteTag
    chan = Channel conn (channelRefGuid (browserContextRouteEventRoute event))
  request <- requestFromChannel chan
  handlers <- routeHandlersFor conn (channelGuid (browserContextChannel ctx))
  chainRoutes chan request handlers

chainRoutes :: Channel RouteTag -> Request -> [(UrlGlob, ChannelRef -> IO Bool)] -> IO ()
chainRoutes chan request handlers =
  case handlers of
    [] -> do
      overrides <-
        lookupRequestOverrides
          (channelConnection (requestChannel request))
          (channelGuid (requestChannel request))
      sendContinue chan True overrides
    (glob, run) : rest -> do
      current <- requestUrl request
      if matchesUrlGlob glob current
        then do
          done <- run (ChannelRef (channelGuid chan))
          unless done (chainRoutes chan request rest)
        else
          chainRoutes chan request rest

settleRoute :: Route -> Bool -> IO ()
settleRoute handled settled =
  atomically $ do
    empty <- isEmptyTMVar (routeOutcome handled)
    if empty
      then putTMVar (routeOutcome handled) settled
      else throwSTM RouteAlreadyHandled

sendContinue :: Channel RouteTag -> Bool -> RequestOverrides -> IO ()
sendContinue chan isFallback overrides = do
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

updateInterceptionPatterns :: Page -> IO ()
updateInterceptionPatterns page = do
  handlers <- routeHandlersFor (channelConnection (pageChannel page)) (channelGuid (pageChannel page))
  _ :: PageSetNetworkInterceptionPatternsResult <-
    send
      (pageChannel page)
      (MethodName "setNetworkInterceptionPatterns")
      (mkPageSetNetworkInterceptionPatternsParams (fmap (globPattern . fst) handlers))
  pure ()

updateContextInterceptionPatterns :: BrowserContext -> IO ()
updateContextInterceptionPatterns ctx = do
  handlers <-
    routeHandlersFor
      (channelConnection (browserContextChannel ctx))
      (channelGuid (browserContextChannel ctx))
  _ :: BrowserContextSetNetworkInterceptionPatternsResult <-
    send
      (browserContextChannel ctx)
      (MethodName "setNetworkInterceptionPatterns")
      (mkBrowserContextSetNetworkInterceptionPatternsParams (fmap (contextGlobPattern . fst) handlers))
  pure ()

globPattern :: UrlGlob -> PageSetNetworkInterceptionPatternsParamsPatterns
globPattern glob =
  mkPageSetNetworkInterceptionPatternsParamsPatterns
    { pageSetNetworkInterceptionPatternsParamsPatternsGlob = Just (urlGlobToText glob)
    }

contextGlobPattern :: UrlGlob -> BrowserContextSetNetworkInterceptionPatternsParamsPatterns
contextGlobPattern glob =
  mkBrowserContextSetNetworkInterceptionPatternsParamsPatterns
    { browserContextSetNetworkInterceptionPatternsParamsPatternsGlob = Just (urlGlobToText glob)
    }

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
