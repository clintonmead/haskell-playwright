module Playwright.Route
  ( Route
  , abortRoute
  , continueRoute
  , continueRouteWith
  , fallbackRoute
  , fallbackRouteWith
  , fulfillRoute
  , route
  , routeRequest
  , routeWith
  , unroute
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
  , takeTMVar
  , throwSTM
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
import Playwright.Channel
  ( Channel (Channel, channelConnection, channelGuid)
  , RouteTag
  , send
  )
import Playwright.Connection
  ( lookupObject
  , lookupRequestOverrides
  , pushRouteHandler
  , registerEventHandler
  , removeRouteHandlers
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
import Playwright.FulfillOptions
  ( ContentType (contentTypeToText)
  , FulfillBody (FulfillBytes, FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillHeaders, fulfillStatus)
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
  ( Page (pageChannel)
  )
import Playwright.Protocol
  ( NameValue
  , PageRouteEvent (pageRouteEventRoute)
  , PageSetNetworkInterceptionPatternsParamsPatterns (pageSetNetworkInterceptionPatternsParamsPatternsGlob)
  , PageSetNetworkInterceptionPatternsResult
  , RouteAbortResult
  , RouteContinueParams (routeContinueParamsHeaders, routeContinueParamsMethod, routeContinueParamsPostData, routeContinueParamsUrl)
  , RouteContinueResult
  , RouteFulfillParams (routeFulfillParamsBody, routeFulfillParamsHeaders, routeFulfillParamsIsBase64, routeFulfillParamsStatus)
  , RouteFulfillResult
  , RouteInitializer (routeInitializerRequest)
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
    headers = fulfillHeadersWithDefaults opts byteLength
  _ :: RouteFulfillResult <-
    send
      (routeChannel handled)
      (MethodName "fulfill")
      mkRouteFulfillParams
        { routeFulfillParamsBody = bodyText
        , routeFulfillParamsHeaders = Just headers
        , routeFulfillParamsIsBase64 = Just isBase64
        , routeFulfillParamsStatus = Just (maybe 200 httpStatusToInt (fulfillStatus opts))
        }
  settleRoute handled True

route :: Page -> UrlGlob -> (Route -> IO ()) -> IO ()
route page glob =
  routeWith page glob defaultRouteOptions

routeWith :: Page -> UrlGlob -> RouteOptions -> (Route -> IO ()) -> IO ()
routeWith page glob opts handler = do
  let
    conn = channelConnection (pageChannel page)
    guid = channelGuid (pageChannel page)
  remaining <- newTVarIO (fmap routeTimesToInt (routeTimes opts))
  first <-
    pushRouteHandler conn guid glob $ \ref -> do
      live <- atomically (consumeRouteTime remaining)
      if live
        then do
          box <- newEmptyTMVarIO
          handler (Route (Channel conn (channelRefGuid ref)) box)
          atomically (takeTMVar box)
        else
          pure False
  when first (registerEventHandler conn guid (MethodName "route") (dispatchPageRoute page))
  updateInterceptionPatterns page

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

dispatchPageRoute :: Page -> Value -> IO ()
dispatchPageRoute page params = do
  event <- either throwIO pure (fromJsonValue params)
  let
    conn = channelConnection (pageChannel page)
    chan :: Channel RouteTag
    chan = Channel conn (channelRefGuid (pageRouteEventRoute event))
  request <- requestFromChannel chan
  handlers <- routeHandlersFor conn (channelGuid (pageChannel page))
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

globPattern :: UrlGlob -> PageSetNetworkInterceptionPatternsParamsPatterns
globPattern glob =
  mkPageSetNetworkInterceptionPatternsParamsPatterns
    { pageSetNetworkInterceptionPatternsParamsPatternsGlob = Just (urlGlobToText glob)
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

fulfillHeadersWithDefaults :: FulfillOptions -> Int -> [NameValue]
fulfillHeadersWithDefaults opts byteLength =
  fmap toNameValue (withLength (withContentType (fmap lowercaseHeader (fulfillHeaders opts))))
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
