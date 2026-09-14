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
import Control.Monad
  ( unless
  , when
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Aeson
  ( Value
  )
import Playwright.BrowserContext
  ( BrowserContext (browserContextChannel)
  )
import Playwright.Channel
  ( Channel (Channel, channelGuid)
  , RouteTag
  , send
  )
import Playwright.Connection
  ( clearRouteHandlers
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
  ( throwDecode
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
  , askConnection
  , runPlaywrightWith
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

route :: Page -> UrlGlob -> (Route -> Playwright ()) -> Playwright ()
route page glob =
  routeWith page glob defaultRouteOptions

routeWith :: Page -> UrlGlob -> RouteOptions -> (Route -> Playwright ()) -> Playwright ()
routeWith page glob opts handler = do
  let
    guid = channelGuid (pageChannel page)
  first <- installRouteHandler guid glob opts handler
  when first (registerEventHandler guid (MethodName "route") (dispatchPageRoute page))
  updateInterceptionPatterns page

contextRoute :: BrowserContext -> UrlGlob -> (Route -> Playwright ()) -> Playwright ()
contextRoute ctx glob =
  contextRouteWith ctx glob defaultRouteOptions

contextRouteWith :: BrowserContext -> UrlGlob -> RouteOptions -> (Route -> Playwright ()) -> Playwright ()
contextRouteWith ctx glob opts handler = do
  let
    guid = channelGuid (browserContextChannel ctx)
  first <- installRouteHandler guid glob opts handler
  when first (registerEventHandler guid (MethodName "route") (dispatchContextRoute ctx))
  updateContextInterceptionPatterns ctx

installRouteHandler :: Guid -> UrlGlob -> RouteOptions -> (Route -> Playwright ()) -> Playwright Bool
installRouteHandler guid glob opts handler = do
  conn <- askConnection
  remaining <- liftIO (newTVarIO (fmap routeTimesToInt (routeTimes opts)))
  pushRouteHandler guid glob $ \ref ->
    runPlaywrightWith conn $ do
      live <- liftIO (atomically (consumeRouteTime remaining))
      if live
        then do
          box <- liftIO newEmptyTMVarIO
          handler (Route (Channel (channelRefGuid ref)) box)
          outcome <- liftIO (atomically (tryReadTMVar box))
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

unroute :: Page -> UrlGlob -> Playwright ()
unroute page glob = do
  removeRouteHandlers (channelGuid (pageChannel page)) glob
  updateInterceptionPatterns page

unrouteAll :: Page -> Playwright ()
unrouteAll page = do
  clearRouteHandlers (channelGuid (pageChannel page))
  updateInterceptionPatterns page

unrouteContext :: BrowserContext -> UrlGlob -> Playwright ()
unrouteContext ctx glob = do
  removeRouteHandlers (channelGuid (browserContextChannel ctx)) glob
  updateContextInterceptionPatterns ctx

unrouteContextAll :: BrowserContext -> Playwright ()
unrouteContextAll ctx = do
  clearRouteHandlers (channelGuid (browserContextChannel ctx))
  updateContextInterceptionPatterns ctx

dispatchPageRoute :: Page -> Value -> Playwright ()
dispatchPageRoute page params = do
  event <- throwDecode params
  let
    chan :: Channel RouteTag
    chan = Channel (channelRefGuid (pageRouteEventRoute event))
  request <- requestFromChannel chan
  pageHandlers <- routeHandlersFor (channelGuid (pageChannel page))
  contextHandlers <- routeHandlersFor (channelGuid (pageContextChannel page))
  chainRoutes chan request (pageHandlers <> contextHandlers)

dispatchContextRoute :: BrowserContext -> Value -> Playwright ()
dispatchContextRoute ctx params = do
  event <- throwDecode params
  let
    chan :: Channel RouteTag
    chan = Channel (channelRefGuid (browserContextRouteEventRoute event))
  request <- requestFromChannel chan
  handlers <- routeHandlersFor (channelGuid (browserContextChannel ctx))
  chainRoutes chan request handlers

chainRoutes :: Channel RouteTag -> Request -> [(UrlGlob, ChannelRef -> IO Bool)] -> Playwright ()
chainRoutes chan request handlers =
  case handlers of
    [] -> do
      overrides <- lookupRequestOverrides (channelGuid (requestChannel request))
      sendContinue chan True overrides
    (glob, run) : rest -> do
      current <- requestUrl request
      if matchesUrlGlob glob current
        then do
          done <- liftIO (run (ChannelRef (channelGuid chan)))
          unless done (chainRoutes chan request rest)
        else
          chainRoutes chan request rest

updateInterceptionPatterns :: Page -> Playwright ()
updateInterceptionPatterns page = do
  handlers <- routeHandlersFor (channelGuid (pageChannel page))
  _ :: PageSetNetworkInterceptionPatternsResult <-
    send
      (pageChannel page)
      (MethodName "setNetworkInterceptionPatterns")
      (mkPageSetNetworkInterceptionPatternsParams (fmap (globPattern . fst) handlers))
  pure ()

updateContextInterceptionPatterns :: BrowserContext -> Playwright ()
updateContextInterceptionPatterns ctx = do
  handlers <- routeHandlersFor (channelGuid (browserContextChannel ctx))
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
