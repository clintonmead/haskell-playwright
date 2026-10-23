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
  , newContextForReuseWith
  , newContextWith
  , disconnectFromReusedContext
  , onBrowserContext
  , validateNewContextOptions
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
  ( forM_
  , when
  )
import Data.Aeson
  ( Value
  , object
  , (.=)
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
  , startContextRecordHar
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
  ( NewContextOptions (newContextBaseURL, newContextDeviceScaleFactor, newContextIsMobile, newContextNoDefaultViewport, newContextRecordHar)
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
newContextWith browser =
  newContextVia browser "newContext"

-- | Internal: the driver's @newContextForReuse@, which hands back the context
-- it keeps for reuse (resetting it when the options match), as upstream's
-- @browser._newContextForReuse@ does.
newContextForReuseWith :: Browser -> NewContextOptions -> Playwright BrowserContext
newContextForReuseWith browser =
  newContextVia browser "newContextForReuse"

-- | Internal: the driver's @disconnectFromReusedContext@, which disposes the
-- context the driver is keeping for reuse, as upstream's
-- @browser._disconnectFromReusedContext@ does.
disconnectFromReusedContext :: Browser -> Text -> Playwright ()
disconnectFromReusedContext browser reason = do
  _ :: Value <-
    send
      (browserChannel browser)
      (MethodName "disconnectFromReusedContext")
      (object ["reason" .= reason])
  pure ()

newContextVia :: Browser -> Text -> NewContextOptions -> Playwright BrowserContext
newContextVia browser method opts = do
  validateNewContextOptions opts
  engines <- lookupSelectorEngines
  result <-
    send
      (browserChannel browser)
      (MethodName method)
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
  ctx <- contextFromRef ref
  forM_ (newContextRecordHar opts) (startContextRecordHar ctx)
  pure ctx

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
