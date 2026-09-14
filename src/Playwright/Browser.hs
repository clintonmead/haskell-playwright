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

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
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
  , Channel (Channel, channelGuid)
  , send
  )
import Playwright.Connection
  ( closedGuids
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
  ( throwDecode
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

browserFromRef :: ChannelRef -> Playwright Browser
browserFromRef ref =
  pure (Browser (Channel (channelRefGuid ref)))

browsersEqual :: Browser -> Browser -> Bool
browsersEqual left right =
  channelGuid (browserChannel left) == channelGuid (browserChannel right)

browserVersion :: Browser -> Playwright Text
browserVersion browser = do
  obj <- lookupObject (channelGuid (browserChannel browser))
  initializer <- throwDecode (remoteInitializer obj)
  pure (browserInitializerVersion initializer)

onBrowserContext :: Browser -> (BrowserContext -> Playwright ()) -> Playwright ()
onBrowserContext browser run =
  registerEventHandler
    (channelGuid (browserChannel browser))
    (MethodName "context")
    ( \params -> do
        event <- throwDecode params
        ctx <-
          contextFromRef
            (browserContextEventContext event)
        run ctx
    )

browserContexts :: Browser -> Playwright [BrowserContext]
browserContexts browser = do
  let
    browserGuid = channelGuid (browserChannel browser)
  owned <- lookupOwnedContextGuids browserGuid
  closed <- closedGuids
  traverse
    (contextFromRef . ChannelRef)
    (filter (`Set.notMember` closed) owned)

contextBrowser :: BrowserContext -> Playwright Browser
contextBrowser ctx = do
  let
    guid = channelGuid (browserContextChannel ctx)
  owner <- lookupContextBrowserGuid guid
  case owner of
    Just browserGuid ->
      browserFromRef (ChannelRef browserGuid)
    Nothing -> do
      obj <- lookupObject guid
      browserFromRef (ChannelRef (remoteParent obj))

closeBrowser :: Browser -> Playwright ()
closeBrowser browser =
  ignoreTargetClosed $ do
    _ :: BrowserCloseResult <-
      send (browserChannel browser) (MethodName "close") mkBrowserCloseParams
    pure ()

newContext :: Browser -> Playwright BrowserContext
newContext browser =
  newContextWith browser defaultNewContextOptions

newContextWith :: Browser -> NewContextOptions -> Playwright BrowserContext
newContextWith browser opts = do
  validateNewContextOptions opts
  engines <- lookupSelectorEngines
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
  rememberBrowserContext (channelGuid (browserChannel browser)) (channelRefGuid ref)
  case newContextBaseURL opts of
    Nothing ->
      pure ()
    Just base ->
      writeContextBaseUrl (channelRefGuid ref) base
  contextFromRef ref

validateNewContextOptions :: NewContextOptions -> Playwright ()
validateNewContextOptions opts = do
  let
    noViewport = newContextNoDefaultViewport opts == Just True
  when (noViewport && isJust (newContextDeviceScaleFactor opts)) $
    throwIO (DecodeFailure "\"deviceScaleFactor\" option is not supported with null \"viewport\"")
  when (noViewport && isJust (newContextIsMobile opts)) $
    throwIO (DecodeFailure "\"isMobile\" option is not supported with null \"viewport\"")

newBrowserPage :: Browser -> Playwright Page
newBrowserPage browser =
  newBrowserPageWith browser defaultNewContextOptions

newBrowserPageWith :: Browser -> NewContextOptions -> Playwright Page
newBrowserPageWith browser opts = do
  ctx <- newContextWith browser opts
  page <- newPage ctx
  markPageOwnsContext (channelGuid (pageChannel page))
  markContextHasOwnerPage (channelGuid (browserContextChannel ctx))
  pure page

withPage :: Browser -> (Page -> Playwright a) -> Playwright a
withPage browser =
  withPageWith browser defaultNewContextOptions

withPageWith :: Browser -> NewContextOptions -> (Page -> Playwright a) -> Playwright a
withPageWith browser opts action =
  bracket
    ( do
        ctx <- newContextWith browser opts
        page <- newPage ctx
        pure (ctx, page)
    )
    (\(ctx, _) -> closeContext ctx)
    (\(_, page) -> action page)
