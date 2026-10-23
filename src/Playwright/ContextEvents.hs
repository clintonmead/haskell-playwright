module Playwright.ContextEvents
  ( WebError (..)
  , WebErrorLocation (..)
  , onContextDialogClosed
  , onContextDownload
  , onContextFrameAttached
  , onContextFrameDetached
  , onContextFrameNavigated
  , onContextPageClose
  , onContextPageLoad
  , onContextWebError
  , onFrameAttached
  , onFrameDetached
  , onFrameNavigated
  ) where

import Data.Aeson
  ( Value
  )
import Data.Aeson.Types
  ( parseMaybe
  , withObject
  , (.:)
  )
import Data.Text
  ( Text
  )
import Playwright.BrowserContext
  ( BrowserContext (browserContextChannel)
  , contextPages
  , onContextPage
  )
import Playwright.Channel
  ( Channel (channelGuid)
  , send
  )
import Playwright.Connection
  ( registerDispatchEventHandler
  , registerEventHandler
  )
import Playwright.Dialog
  ( Dialog
  , dialogFromRef
  )
import Playwright.Download
  ( Download
  , downloadFromEvent
  )
import Playwright.Frame
  ( Frame (frameChannel)
  , frameFromRef
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Json.ChannelRef
  ( ChannelRef
  , channelRefGuid
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Page
  ( Page (pageChannel)
  , frames
  , onPageClose
  , pageFromRef
  )
import Playwright.Page.Wait
  ( onLoad
  )
import Playwright.PageError
  ( PageError
  , pageErrorFromSerialized
  )
import Playwright.Protocol
  ( BrowserContextDialogClosedEvent (browserContextDialogClosedEventDialog)
  , BrowserContextPageErrorEvent (browserContextPageErrorEventError, browserContextPageErrorEventPage)
  , BrowserContextUpdateSubscriptionParamsEvent (BrowserContextUpdateSubscriptionParamsEventDialogClosed)
  , BrowserContextUpdateSubscriptionResult
  , FrameNavigatedEvent (frameNavigatedEventError)
  , PageFrameAttachedEvent (pageFrameAttachedEventFrame)
  , PageFrameDetachedEvent (pageFrameDetachedEventFrame)
  , mkBrowserContextUpdateSubscriptionParams
  )

-- | An error thrown by page script, as upstream's 'WebError'.
data WebError = WebError
  { webErrorPage :: Page
  , webErrorError :: PageError
  , webErrorLocation :: Maybe WebErrorLocation
  }

-- | Where a 'WebError' was thrown, as upstream's @location()@.
data WebErrorLocation = WebErrorLocation
  { webErrorLocationUrl :: Text
  , webErrorLocationLine :: Int
  , webErrorLocationColumn :: Int
  }

-- | Runs the setup for each page of the context, both pages that already
-- exist and pages opened later.
eachContextPage :: BrowserContext -> (Page -> Playwright ()) -> Playwright ()
eachContextPage ctx setup = do
  existing <- contextPages ctx
  mapM_ setup existing
  onContextPage ctx setup

-- | Runs the callback with each page of the context when that page's load
-- event fires, including pages that already exist. The callback is given the
-- page, as upstream's 'pageload'.
onContextPageLoad :: BrowserContext -> (Page -> Playwright ()) -> Playwright ()
onContextPageLoad ctx callback =
  eachContextPage ctx (\page -> onLoad page (callback page))

-- | Runs the callback with each page of the context when that page closes,
-- including pages that already exist, as upstream's 'pageclose'.
onContextPageClose :: BrowserContext -> (Page -> Playwright ()) -> Playwright ()
onContextPageClose ctx callback =
  eachContextPage ctx (\page -> onPageClose page (callback page))

-- | Runs the callback with each dialog of the context once it has been closed,
-- as upstream's 'dialogclosed'.
onContextDialogClosed :: BrowserContext -> (Dialog -> Playwright ()) -> Playwright ()
onContextDialogClosed ctx run = do
  _ :: BrowserContextUpdateSubscriptionResult <-
    send
      (browserContextChannel ctx)
      (MethodName "updateSubscription")
      (mkBrowserContextUpdateSubscriptionParams True BrowserContextUpdateSubscriptionParamsEventDialogClosed)
  registerEventHandler
    (channelGuid (browserContextChannel ctx))
    (MethodName "dialogClosed")
    $ \params -> do
      event <- throwDecode params
      dialog <- dialogFromRef (browserContextDialogClosedEventDialog event)
      run dialog

-- | Runs the callback with each download started by a page of the context, as
-- upstream's 'download'.
onContextDownload :: BrowserContext -> (Download -> Playwright ()) -> Playwright ()
onContextDownload ctx run =
  eachContextPage ctx $ \page ->
    registerEventHandler
      (channelGuid (pageChannel page))
      (MethodName "download")
      $ \params -> do
        event <- throwDecode params
        run (downloadFromEvent page event)

-- | Runs the callback with each error thrown by page script in the context,
-- as upstream's 'weberror'.
onContextWebError :: BrowserContext -> (WebError -> Playwright ()) -> Playwright ()
onContextWebError ctx run =
  registerEventHandler
    (channelGuid (browserContextChannel ctx))
    (MethodName "pageError")
    $ \params -> do
      event <- throwDecode params
      page <- pageFromRef (browserContextChannel ctx) (browserContextPageErrorEventPage event)
      run
        WebError
          { webErrorPage = page
          , webErrorError = pageErrorFromSerialized (browserContextPageErrorEventError event)
          , webErrorLocation = webErrorLocationFrom params
          }

webErrorLocationFrom :: Value -> Maybe WebErrorLocation
webErrorLocationFrom =
  parseMaybe
    ( withObject "event" $ \event ->
        event .: "location" >>= withObject "location" (\location ->
          WebErrorLocation
            <$> location .: "url"
            <*> location .: "line"
            <*> location .: "column"
        )
    )

-- | Runs the callback with each frame attached to a page of the context, as
-- upstream's 'frameattached'.
onContextFrameAttached :: BrowserContext -> (Frame -> Playwright ()) -> Playwright ()
onContextFrameAttached ctx run =
  eachContextPage ctx (\page -> onFrameAttached page run)

-- | Runs the callback with each frame detached from a page of the context, as
-- upstream's 'framedetached'.
onContextFrameDetached :: BrowserContext -> (Frame -> Playwright ()) -> Playwright ()
onContextFrameDetached ctx run =
  eachContextPage ctx (\page -> onFrameDetached page run)

-- | Runs the callback with each frame detached from the page, as upstream's
-- 'framedetached'.
onFrameDetached :: Page -> (Frame -> Playwright ()) -> Playwright ()
onFrameDetached page run =
  registerEventHandler
    (channelGuid (pageChannel page))
    (MethodName "frameDetached")
    $ \params -> do
      event <- throwDecode params
      frame <- frameFromRef (pageFrameDetachedEventFrame (event :: PageFrameDetachedEvent))
      run frame

-- | Runs the callback with each frame of the context when it navigates, as
-- upstream's 'framenavigated'. Frames that are attached later are watched too.
onContextFrameNavigated :: BrowserContext -> (Frame -> Playwright ()) -> Playwright ()
onContextFrameNavigated ctx run =
  eachContextPage ctx (\page -> onFrameNavigated page run)

-- | Runs the callback with each frame of the page when it navigates, as
-- upstream's 'framenavigated'. Frames that are attached later are watched too.
onFrameNavigated :: Page -> (Frame -> Playwright ()) -> Playwright ()
onFrameNavigated page run = do
  existing <- frames page
  mapM_ (watchFrameNavigated run) existing
  watchFramesAttached page run

-- | Watches each frame attached to the page for navigation. The watch is
-- registered as the event is read, so a navigation that follows the attach
-- is not lost.
watchFramesAttached :: Page -> (Frame -> Playwright ()) -> Playwright ()
watchFramesAttached page run =
  registerDispatchEventHandler
    (channelGuid (pageChannel page))
    (MethodName "frameAttached")
    ( \params -> do
        event <- throwDecode params
        watchAttachedFrame run (pageFrameAttachedEventFrame (event :: PageFrameAttachedEvent))
    )
    (\_ -> pure ())

watchAttachedFrame :: (Frame -> Playwright ()) -> ChannelRef -> Playwright ()
watchAttachedFrame run ref =
  watchNavigatedGuid (channelRefGuid ref) (frameFromRef ref) run

-- | Runs the callback with each frame attached to the page, as upstream's
-- 'frameattached'.
onFrameAttached :: Page -> (Frame -> Playwright ()) -> Playwright ()
onFrameAttached page run =
  registerEventHandler
    (channelGuid (pageChannel page))
    (MethodName "frameAttached")
    $ \params -> do
      event <- throwDecode params
      frame <- frameFromRef (pageFrameAttachedEventFrame (event :: PageFrameAttachedEvent))
      run frame

watchFrameNavigated :: (Frame -> Playwright ()) -> Frame -> Playwright ()
watchFrameNavigated run frame =
  watchNavigatedGuid (channelGuid (frameChannel frame)) (pure frame) run

watchNavigatedGuid :: Guid -> Playwright Frame -> (Frame -> Playwright ()) -> Playwright ()
watchNavigatedGuid guid getFrame run =
  registerEventHandler
    guid
    (MethodName "navigated")
    $ \params -> do
      event <- throwDecode params
      case frameNavigatedEventError (event :: FrameNavigatedEvent) of
        Nothing ->
          getFrame >>= run
        Just _ ->
          pure ()
