module Playwright.Route.Intercept
  ( contextRoute
  , contextRouteWith
  , route
  , routeWith
  , unroute
  , unrouteAll
  , unrouteContext
  , unrouteContextAll
  ) where

import Control.Concurrent.STM
  ( STM
  , TVar
  , atomically
  , newEmptyTMVarIO
  , newTVarIO
  , readTVar
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
  , clearRouteHandlers
  , lookupRequestOverrides
  , pushRouteHandler
  , registerEventHandler
  , removeRouteHandlers
  , routeHandlersFor
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
  )
import Playwright.Json.Decode
  ( fromJsonValue
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
  , PageRouteEvent (pageRouteEventRoute)
  , PageSetNetworkInterceptionPatternsParamsPatterns (pageSetNetworkInterceptionPatternsParamsPatternsGlob)
  , PageSetNetworkInterceptionPatternsResult
  , mkBrowserContextSetNetworkInterceptionPatternsParams
  , mkBrowserContextSetNetworkInterceptionPatternsParamsPatterns
  , mkPageSetNetworkInterceptionPatternsParams
  , mkPageSetNetworkInterceptionPatternsParamsPatterns
  )
import Playwright.Request
  ( Request (requestChannel)
  , requestUrl
  )
import Playwright.Route
  ( Route (Route)
  , requestFromChannel
  , sendContinue
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
