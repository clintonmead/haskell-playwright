module Playwright.Route
  ( Route (..)
  , abortRoute
  , continueRoute
  , continueRouteWith
  , fallbackRoute
  , fulfillRoute
  , route
  , routeRequest
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Aeson
  ( Value
  )
import Data.ByteString.Base64
  ( encode
  )
import Data.ByteString qualified as ByteString
import Data.List
  ( find
  )
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
  , pushRouteHandler
  , registerEventHandler
  , routeHandlersFor
  )
import Playwright.ContinueOptions
  ( ContinueOptions (continueHeaders)
  , defaultContinueOptions
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
import Playwright.HttpStatus
  ( HttpStatus (httpStatusToInt)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( fromJsonValue
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
  , RouteContinueParams (routeContinueParamsHeaders)
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
  ( Request
  , requestFromRef
  , requestUrl
  )
import Playwright.UrlGlob
  ( UrlGlob (urlGlobToText)
  , matchesUrlGlob
  )

newtype Route = Route
  { routeChannel :: Channel RouteTag
  }

routeRequest :: Route -> IO Request
routeRequest handled = do
  obj <- lookupObject (channelConnection (routeChannel handled)) (channelGuid (routeChannel handled))
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  pure
    ( requestFromRef
        (channelConnection (routeChannel handled))
        (routeInitializerRequest initializer)
    )

continueRoute :: Route -> IO ()
continueRoute handled =
  continueRouteWith handled defaultContinueOptions

continueRouteWith :: Route -> ContinueOptions -> IO ()
continueRouteWith handled opts = do
  _ :: RouteContinueResult <-
    send
      (routeChannel handled)
      (MethodName "continue")
      (mkRouteContinueParams False)
        { routeContinueParamsHeaders = fmap (fmap toNameValue) (continueHeaders opts)
        }
  pure ()

fallbackRoute :: Route -> IO ()
fallbackRoute handled = do
  _ :: RouteContinueResult <-
    send (routeChannel handled) (MethodName "continue") (mkRouteContinueParams True)
  pure ()

abortRoute :: Route -> IO ()
abortRoute handled = do
  _ :: RouteAbortResult <-
    send (routeChannel handled) (MethodName "abort") mkRouteAbortParams
  pure ()

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
  pure ()

route :: Page -> UrlGlob -> (Route -> IO ()) -> IO ()
route page glob handler = do
  let
    conn = channelConnection (pageChannel page)
    guid = channelGuid (pageChannel page)
  first <-
    pushRouteHandler conn guid glob $ \ref ->
      handler (Route (Channel conn (channelRefGuid ref)))
  when first (registerEventHandler conn guid (MethodName "route") (dispatchPageRoute page))
  updateInterceptionPatterns page

dispatchPageRoute :: Page -> Value -> IO ()
dispatchPageRoute page params = do
  event <- either throwIO pure (fromJsonValue params)
  let
    conn = channelConnection (pageChannel page)
    handled = Route (Channel conn (channelRefGuid (pageRouteEventRoute event)))
  request <- routeRequest handled
  current <- requestUrl request
  handlers <- routeHandlersFor conn (channelGuid (pageChannel page))
  case find (\(glob, _) -> matchesUrlGlob glob current) handlers of
    Just (_, run) ->
      run (pageRouteEventRoute event)
    Nothing ->
      fallbackRoute handled

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
