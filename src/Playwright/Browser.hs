module Playwright.Browser
  ( Browser (..)
  , browserFromRef
  , browserContexts
  , browserVersion
  , browsersEqual
  , closeBrowser
  , contextBrowser
  , newBrowserPage
  , newBrowserPageWith
  , newContext
  , newContextWith
  , onBrowserContext
  , withPage
  , withPageWith
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( when
  )
import Data.Maybe
  ( isJust
  )
import Data.Set qualified as Set
import Data.Text
  ( Text
  )
import Playwright.BrowserContext
  ( BrowserContext (browserContextChannel)
  , closeContext
  , contextFromRef
  , newPage
  )
import Playwright.Channel
  ( BrowserTag
  , Channel (Channel, channelConnection, channelGuid)
  , send
  )
import Playwright.Connection
  ( Connection
  , closedGuids
  , lookupContextBrowserGuid
  , lookupObject
  , lookupOwnedContextGuids
  , lookupSelectorEngines
  , markContextHasOwnerPage
  , markPageOwnsContext
  , registerEventHandler
  , rememberBrowserContext
  , writeContextBaseUrl
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  , ignoreTargetClosed
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
import Playwright.NewContextOptions
  ( NewContextOptions (newContextBaseURL, newContextDeviceScaleFactor, newContextIsMobile, newContextNoDefaultViewport)
  , defaultNewContextOptions
  , toBrowserNewContextParams
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Protocol
  ( BrowserCloseResult
  , BrowserContextEvent (browserContextEventContext)
  , BrowserInitializer (browserInitializerVersion)
  , BrowserNewContextParams (browserNewContextParamsSelectorEngines)
  , BrowserNewContextResult (browserNewContextResultContext)
  , mkBrowserCloseParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer, remoteParent)
  )

newtype Browser = Browser
  { browserChannel :: Channel BrowserTag
  }

browserFromRef :: Connection -> ChannelRef -> IO Browser
browserFromRef conn ref =
  pure (Browser (Channel conn (channelRefGuid ref)))

browsersEqual :: Browser -> Browser -> Bool
browsersEqual left right =
  channelGuid (browserChannel left) == channelGuid (browserChannel right)

browserVersion :: Browser -> IO Text
browserVersion browser = do
  let
    conn = channelConnection (browserChannel browser)
  obj <- lookupObject conn (channelGuid (browserChannel browser))
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  pure (browserInitializerVersion initializer)

onBrowserContext :: Browser -> (BrowserContext -> IO ()) -> IO ()
onBrowserContext browser run =
  registerEventHandler
    (channelConnection (browserChannel browser))
    (channelGuid (browserChannel browser))
    (MethodName "context")
    ( \params -> do
        event <- either throwIO pure (fromJsonValue params)
        ctx <-
          contextFromRef
            (channelConnection (browserChannel browser))
            (browserContextEventContext event)
        run ctx
    )

browserContexts :: Browser -> IO [BrowserContext]
browserContexts browser = do
  let
    conn = channelConnection (browserChannel browser)
    browserGuid = channelGuid (browserChannel browser)
  owned <- lookupOwnedContextGuids conn browserGuid
  closed <- closedGuids conn
  traverse
    (contextFromRef conn . ChannelRef)
    (filter (`Set.notMember` closed) owned)

contextBrowser :: BrowserContext -> IO Browser
contextBrowser ctx = do
  let
    conn = channelConnection (browserContextChannel ctx)
    guid = channelGuid (browserContextChannel ctx)
  owner <- lookupContextBrowserGuid conn guid
  case owner of
    Just browserGuid ->
      browserFromRef conn (ChannelRef browserGuid)
    Nothing -> do
      obj <- lookupObject conn guid
      browserFromRef conn (ChannelRef (remoteParent obj))

closeBrowser :: Browser -> IO ()
closeBrowser browser =
  ignoreTargetClosed $ do
    _ :: BrowserCloseResult <-
      send (browserChannel browser) (MethodName "close") mkBrowserCloseParams
    pure ()

newContext :: Browser -> IO BrowserContext
newContext browser =
  newContextWith browser defaultNewContextOptions

newContextWith :: Browser -> NewContextOptions -> IO BrowserContext
newContextWith browser opts = do
  validateNewContextOptions opts
  let
    conn = channelConnection (browserChannel browser)
  engines <- lookupSelectorEngines conn
  result <-
    send
      (browserChannel browser)
      (MethodName "newContext")
      ( (toBrowserNewContextParams opts)
          { browserNewContextParamsSelectorEngines =
              case engines of
                [] ->
                  Nothing
                _ ->
                  Just engines
          }
      )
  let
    ref = browserNewContextResultContext result
  rememberBrowserContext conn (channelGuid (browserChannel browser)) (channelRefGuid ref)
  case newContextBaseURL opts of
    Nothing ->
      pure ()
    Just base ->
      writeContextBaseUrl conn (channelRefGuid ref) base
  contextFromRef conn ref

validateNewContextOptions :: NewContextOptions -> IO ()
validateNewContextOptions opts = do
  let
    noViewport = newContextNoDefaultViewport opts == Just True
  when (noViewport && isJust (newContextDeviceScaleFactor opts)) $
    throwIO (DecodeFailure "\"deviceScaleFactor\" option is not supported with null \"viewport\"")
  when (noViewport && isJust (newContextIsMobile opts)) $
    throwIO (DecodeFailure "\"isMobile\" option is not supported with null \"viewport\"")

newBrowserPage :: Browser -> IO Page
newBrowserPage browser =
  newBrowserPageWith browser defaultNewContextOptions

newBrowserPageWith :: Browser -> NewContextOptions -> IO Page
newBrowserPageWith browser opts = do
  ctx <- newContextWith browser opts
  page <- newPage ctx
  let
    conn = channelConnection (browserChannel browser)
  markPageOwnsContext conn (channelGuid (pageChannel page))
  markContextHasOwnerPage conn (channelGuid (browserContextChannel ctx))
  pure page

withPage :: Browser -> (Page -> IO a) -> IO a
withPage browser =
  withPageWith browser defaultNewContextOptions

withPageWith :: Browser -> NewContextOptions -> (Page -> IO a) -> IO a
withPageWith browser opts action =
  bracket
    ( do
        ctx <- newContextWith browser opts
        page <- newPage ctx
        pure (ctx, page)
    )
    (\(ctx, _) -> closeContext ctx)
    (\(_, page) -> action page)
