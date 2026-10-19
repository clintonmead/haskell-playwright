module Playwright.Page.Wait
  ( armConsole
  , armDialog
  , onDialog
  , armFrameDetached
  , armFrameNavigated
  , armNavigatedFrame
  , armLoadEvent
  , armPopup
  , onLoad
  , onConsole
  , waitForConsole
  , waitForConsoleWhere
  , waitForDialog
  , waitForFrameDetached
  , waitForFrameNavigated
  , waitForNavigatedFrame
  , waitForFunction
  , waitForFunctionOnFrame
  , waitForFunctionPolling
  , waitForFunctionWith
  , waitForFunctionWithArg
  , waitForDomContentLoadedEvent
  , armDomContentLoadedEvent
  , waitForLoadEvent
  , waitForLoadState
  , waitForLoadStateWith
  , waitForNavigation
  , waitForNavigationWith
  , waitForNavigationOptions
  , armNavigationOptions
  , waitForPopup
  , armRequest
  , armRequestWhere
  , armRequestWith
  , waitForRequest
  , waitForRequestFailed
  , waitForRequestWhere
  , waitForRequestFailedWith
  , armRequestFailed
  , waitForRequestFinished
  , waitForRequestWith
  , armResponse
  , armResponseWhere
  , armResponseWith
  , waitForResponse
  , waitForResponseWhere
  , waitForResponseWith
  , waitForSelector
  , waitForSelectorHandle
  , waitForSelectorHandleWith
  , waitForSelectorStrict
  , waitForTimeout
  , waitForURL
  , waitForURLWith
  ) where

import Playwright.Monad
  ( Playwright
  )
import Data.Aeson
  ( Value
  , Result (Error, Success)
  , fromJSON
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.Scientific
  ( fromFloatDigits
  )
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( Channel (channelGuid)
  )
import Playwright.Connection
  ( registerEventHandler
  , registerEventWaiter
  , registerFreshEventWaiter
  , registerFutureEventWaiter
  , registerFutureEventWaiterWhere
  , waitForChannelEvent
  , waitForChannelEventUnlessClosed
  , waitForFutureChannelEvent
  , waitForWaiter
  , waitForWaiterUnlessClosed
  , waitUntilUrl
  )
import Playwright.ConsoleMessage
  ( ConsoleMessage (consoleMessagePage)
  , consoleMessageFromEvent
  )
import Playwright.Dialog
  ( Dialog (dialogPage)
  , dialogFromRef
  )
import Playwright.ElementHandle
  ( ElementHandle
  )
import Playwright.Frame
  ( Frame (frameChannel)
  , frameFromRef
  )
import Playwright.Frame.Wait
  ( waitForFunctionFrame
  , waitForLoadStateFrame
  , waitForSelectorFrame
  , waitForSelectorHandleFrame
  , waitForTimeoutFrame
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.JsHandle
  ( JsHandle
  )
import Playwright.LoadState
  ( LoadState (LoadStateCommit, LoadStateLoad)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageChannel, pageContextChannel, pageMainFrame)
  , opener
  , pageActionTimeout
  , pageFromRef
  , pageNavigationTimeout
  , pagesEqual
  , subscribeContext
  )
import Playwright.Protocol
  ( BrowserContextDialogEvent (browserContextDialogEventDialog)
  , LifecycleEvent (LifecycleEventDomcontentloaded, LifecycleEventLoad)
  , BrowserContextPageEvent (browserContextPageEventPage)
  , BrowserContextRequestEvent (browserContextRequestEventRequest)
  , BrowserContextRequestFailedEvent (browserContextRequestFailedEventRequest)
  , BrowserContextRequestFinishedEvent (browserContextRequestFinishedEventRequest)
  , BrowserContextResponseEvent (browserContextResponseEventResponse)
  , BrowserContextUpdateSubscriptionParamsEvent (BrowserContextUpdateSubscriptionParamsEventConsole, BrowserContextUpdateSubscriptionParamsEventDialog, BrowserContextUpdateSubscriptionParamsEventRequest, BrowserContextUpdateSubscriptionParamsEventRequestFailed, BrowserContextUpdateSubscriptionParamsEventRequestFinished, BrowserContextUpdateSubscriptionParamsEventResponse)
  , FrameLoadstateEvent (frameLoadstateEventAdd)
  , FrameNavigatedEvent (frameNavigatedEventNewDocument, frameNavigatedEventUrl)
  , FrameNavigatedEventNewDocument (frameNavigatedEventNewDocumentRequest)
  , LifecycleEvent (LifecycleEventLoad)
  , PageFrameDetachedEvent (pageFrameDetachedEventFrame)
  )
import Playwright.RaceTimeout
  ( waitWithTimeout
  )
import Playwright.Request
  ( Request
  , requestFromRef
  , requestResponseRef
  , requestUrl
  )
import Playwright.Response
  ( Response
  , responseFromRef
  , responseUrl
  )
import Playwright.Selector
  ( Selector
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  , undefinedArgument
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  )
import Playwright.UrlGlob
  ( UrlGlob
  , matchesUrlGlob
  )
import Playwright.WaitForNavigationOptions
  ( WaitForNavigationOptions (waitForNavigationOptionsTimeout, waitForNavigationOptionsUrl, waitForNavigationOptionsWaitUntil)
  , defaultWaitForNavigationOptions
  )
import Playwright.WaitForState
  ( WaitForState
  )

waitForSelector :: Page -> Selector -> WaitForState -> Playwright ()
waitForSelector page selector state = do
  timeout <- pageActionTimeout page
  waitForSelectorFrame (pageMainFrame page) selector state timeout Nothing

waitForSelectorHandle :: Page -> Selector -> WaitForState -> Playwright (Maybe ElementHandle)
waitForSelectorHandle page selector state = do
  timeout <- pageActionTimeout page
  waitForSelectorHandleWith page selector state timeout

waitForSelectorHandleWith :: Page -> Selector -> WaitForState -> Timeout -> Playwright (Maybe ElementHandle)
waitForSelectorHandleWith page selector state timeout =
  waitForSelectorHandleFrame (pageMainFrame page) selector state timeout Nothing

waitForSelectorStrict :: Page -> Selector -> WaitForState -> Playwright ()
waitForSelectorStrict page selector state = do
  timeout <- pageActionTimeout page
  waitForSelectorFrame (pageMainFrame page) selector state timeout (Just True)

waitForLoadState :: Page -> LoadState -> Playwright ()
waitForLoadState page =
  waitForLoadStateFrame (pageMainFrame page)

-- | Fails with a wait timeout if the load state is not reached in time.
waitForLoadStateWith :: Page -> LoadState -> Timeout -> Playwright ()
waitForLoadStateWith page state timeout =
  waitWithTimeout timeout (waitForLoadState page state)

onLoad :: Page -> Playwright () -> Playwright ()
onLoad page run =
  registerEventHandler
    (channelGuid (frameChannel (pageMainFrame page)))
    (MethodName "loadstate")
    $ \params -> do
      event <- throwDecode params
      case frameLoadstateEventAdd event of
        Just LifecycleEventLoad ->
          run
        _ ->
          pure ()

-- | Runs the handler for each dialog the page opens. The handler must accept or
-- dismiss the dialog, or the page stays blocked.
onDialog :: Page -> (Dialog -> Playwright ()) -> Playwright ()
onDialog page run = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventDialog
  registerEventHandler
    (channelGuid (pageContextChannel page))
    (MethodName "dialog")
    $ \params -> do
      event <- throwDecode params
      dialog <- dialogFromRef (browserContextDialogEventDialog event)
      if maybe False (`pagesEqual` page) (dialogPage dialog)
        then run dialog
        else pure ()

onConsole :: Page -> (ConsoleMessage -> Playwright ()) -> Playwright ()
onConsole page run = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventConsole
  registerEventHandler
    (channelGuid (pageContextChannel page))
    (MethodName "console")
    $ \params -> do
      event <- throwDecode params
      message <- consoleMessageFromEvent (pageContextChannel page) event
      case consoleMessagePage message of
        Just origin
          | pagesEqual origin page ->
              run message
        _ ->
          pure ()

waitForLoadEvent :: Page -> Playwright ()
waitForLoadEvent page = do
  timeout <- pageNavigationTimeout page
  takeLoad <- armLoadEvent page
  waitWithTimeout timeout takeLoad

waitForDomContentLoadedEvent :: Page -> Playwright ()
waitForDomContentLoadedEvent page = do
  timeout <- pageNavigationTimeout page
  takeEvent <- armDomContentLoadedEvent page
  waitWithTimeout timeout takeEvent

armLoadEvent :: Page -> Playwright (Playwright ())
armLoadEvent page =
  armLifecycleEvent page LifecycleEventLoad

armDomContentLoadedEvent :: Page -> Playwright (Playwright ())
armDomContentLoadedEvent page =
  armLifecycleEvent page LifecycleEventDomcontentloaded

-- | Registers for the next main-frame lifecycle event of the given kind, and
-- skips any other lifecycle events while it waits.
armLifecycleEvent :: Page -> LifecycleEvent -> Playwright (Playwright ())
armLifecycleEvent page lifecycle = do
  waitId <-
    registerFutureEventWaiterWhere
      (channelGuid (frameChannel (pageMainFrame page)))
      (MethodName "loadstate")
      (isLifecycleEvent lifecycle)
  pure $ do
    -- Fails when the page closes, instead of waiting for a load that never comes.
    _ <- waitForWaiterUnlessClosed waitId (channelGuid (pageChannel page))
    pure ()

-- | Whether a loadstate event reports the lifecycle event being waited for.
isLifecycleEvent :: LifecycleEvent -> Value -> Bool
isLifecycleEvent lifecycle params =
  case fromJSON params of
    Success event ->
      frameLoadstateEventAdd (event :: FrameLoadstateEvent) == Just lifecycle
    Error _ ->
      False

waitForTimeout :: Page -> Timeout -> Playwright ()
waitForTimeout page =
  waitForTimeoutFrame (pageMainFrame page)

waitForFunction :: Page -> Text -> Playwright JsHandle
waitForFunction page expression = do
  timeout <- pageActionTimeout page
  waitForFunctionWith page expression timeout

waitForFunctionWith :: Page -> Text -> Timeout -> Playwright JsHandle
waitForFunctionWith page expression timeout =
  waitForFunctionFrame (pageMainFrame page) expression undefinedArgument timeout Nothing

waitForFunctionWithArg :: Page -> Text -> SerializedArgument -> Playwright JsHandle
waitForFunctionWithArg page expression arg = do
  timeout <- pageActionTimeout page
  waitForFunctionFrame (pageMainFrame page) expression arg timeout Nothing

waitForFunctionPolling :: Page -> Text -> Timeout -> Double -> Playwright JsHandle
waitForFunctionPolling page expression timeout interval =
  waitForFunctionFrame
    (pageMainFrame page)
    expression
    undefinedArgument
    timeout
    (Just (fromFloatDigits interval))

waitForFunctionOnFrame :: Frame -> Text -> Playwright JsHandle
waitForFunctionOnFrame frame expression = do
  waitForFunctionFrame frame expression undefinedArgument defaultTimeout Nothing

waitForURL :: Page -> UrlGlob -> Playwright ()
waitForURL page glob = do
  timeout <- pageNavigationTimeout page
  waitForURLWith page glob timeout

waitForURLWith :: Page -> UrlGlob -> Timeout -> Playwright ()
waitForURLWith page glob timeout =
  waitWithTimeout timeout $ do
    waitUntilUrl
      (channelGuid (frameChannel (pageMainFrame page)))
      (matchesUrlGlob glob)
    waitForLoadState page LoadStateLoad

waitForNavigation :: Page -> Playwright (Maybe Response)
waitForNavigation page =
  waitForNavigationOptions page defaultWaitForNavigationOptions

waitForFrameNavigated :: Page -> Playwright ()
waitForFrameNavigated page =
  waitForNavigatedFrame page (pageMainFrame page)

waitForNavigatedFrame :: Page -> Frame -> Playwright ()
waitForNavigatedFrame page frame = do
  timeout <- pageNavigationTimeout page
  takeNavigated <- armNavigatedFrame frame
  waitWithTimeout timeout takeNavigated

armFrameNavigated :: Page -> Playwright (Playwright ())
armFrameNavigated page =
  armNavigatedFrame (pageMainFrame page)

armNavigatedFrame :: Frame -> Playwright (Playwright ())
armNavigatedFrame frame = do
  let
    frameGuid = channelGuid (frameChannel frame)
  waitId <- registerFutureEventWaiter frameGuid (MethodName "navigated")
  pure $ do
    _ <- waitForWaiter waitId
    pure ()

waitForFrameDetached :: Page -> Playwright Frame
waitForFrameDetached page = do
  timeout <- pageNavigationTimeout page
  takeFrame <- armFrameDetached page
  waitWithTimeout timeout takeFrame

armFrameDetached :: Page -> Playwright (Playwright Frame)
armFrameDetached page = do
  let
    guid = channelGuid (pageChannel page)
  waitId <- registerFutureEventWaiter guid (MethodName "frameDetached")
  pure $ do
    params <- waitForWaiter waitId
    event <- throwDecode params
    frameFromRef (pageFrameDetachedEventFrame event)

waitForNavigationWith :: Page -> Timeout -> Maybe UrlGlob -> Playwright (Maybe Response)
waitForNavigationWith page timeout glob =
  waitForNavigationOptions
    page
    defaultWaitForNavigationOptions
      { waitForNavigationOptionsTimeout = Just timeout
      , waitForNavigationOptionsUrl = glob
      }

waitForNavigationOptions :: Page -> WaitForNavigationOptions -> Playwright (Maybe Response)
waitForNavigationOptions page opts = do
  takeNavigation <- armNavigationOptions page opts
  takeNavigation

-- | Registers for the next navigation now, so it is not missed if it starts
-- before the returned action runs.
armNavigationOptions :: Page -> WaitForNavigationOptions -> Playwright (Playwright (Maybe Response))
armNavigationOptions page opts = do
  timeout <-
    maybe (pageNavigationTimeout page) pure (waitForNavigationOptionsTimeout opts)
  waitId <-
    registerFutureEventWaiter
      (channelGuid (frameChannel (pageMainFrame page)))
      (MethodName "navigated")
  pure $ waitWithTimeout timeout $ do
    params <- waitForWaiter waitId
    finishNavigation page opts params

collectNavigation :: Page -> WaitForNavigationOptions -> Playwright (Maybe Response)
collectNavigation page opts = do
  let
    frameGuid = channelGuid (frameChannel (pageMainFrame page))
  params <- waitForFutureChannelEvent frameGuid (MethodName "navigated")
  finishNavigation page opts params

finishNavigation :: Page -> WaitForNavigationOptions -> Value -> Playwright (Maybe Response)
finishNavigation page opts params = do
  event <- throwDecode params
  let matches = maybe True (`matchesUrlGlob` frameNavigatedEventUrl event) (waitForNavigationOptionsUrl opts)
  if matches
    then completeNavigation page opts event
    else collectNavigation page opts

completeNavigation :: Page -> WaitForNavigationOptions -> FrameNavigatedEvent -> Playwright (Maybe Response)
completeNavigation page opts event = do
  case fromMaybe LoadStateLoad (waitForNavigationOptionsWaitUntil opts) of
    LoadStateCommit ->
      pure ()
    state ->
      waitForLoadState page state
  case frameNavigatedEventNewDocument event >>= frameNavigatedEventNewDocumentRequest of
    Nothing ->
      pure Nothing
    Just requestRef -> do
      responseRef <- requestResponseRef (requestFromRef requestRef)
      pure (fmap responseFromRef responseRef)

armDialog :: Page -> Playwright (Playwright Dialog)
armDialog page = do
  timeout <- pageActionTimeout page
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventDialog
  waitId <-
    registerEventWaiter
      (channelGuid (pageContextChannel page))
      (MethodName "dialog")
  pure $ waitWithTimeout timeout $ do
    params <- waitForWaiter waitId
    event <- throwDecode params
    dialogFromRef
      (browserContextDialogEventDialog event)

waitForDialog :: Page -> Playwright Dialog
waitForDialog page = do
  takeDialog <- armDialog page
  takeDialog

armConsole :: Page -> Playwright (Playwright ConsoleMessage)
armConsole page = do
  timeout <- pageActionTimeout page
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventConsole
  waitId <-
    registerEventWaiter
      (channelGuid (pageContextChannel page))
      (MethodName "console")
  pure $ waitWithTimeout timeout $ do
    params <- waitForWaiter waitId
    event <- throwDecode params
    consoleMessageFromEvent (pageContextChannel page) event

waitForConsole :: Page -> Playwright ConsoleMessage
waitForConsole page = do
  takeMessage <- armConsole page
  takeMessage

waitForConsoleWhere :: Page -> (ConsoleMessage -> Bool) -> Playwright ConsoleMessage
waitForConsoleWhere page predicate = do
  timeout <- pageActionTimeout page
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventConsole
  waitWithTimeout timeout collect
  where
    collect = do
      params <-
        waitForChannelEventUnlessClosed
          (channelGuid (pageContextChannel page))
          (MethodName "console")
          (channelGuid (pageChannel page))
      event <- throwDecode params
      message <- consoleMessageFromEvent (pageContextChannel page) event
      case consoleMessagePage message of
        Just origin
          | pagesEqual origin page && predicate message ->
              pure message
        _ ->
          collect

waitForResponse :: Page -> UrlGlob -> Playwright Response
waitForResponse page glob = do
  timeout <- pageActionTimeout page
  waitForResponseWith page glob timeout

waitForResponseWith :: Page -> UrlGlob -> Timeout -> Playwright Response
waitForResponseWith page glob timeout = do
  takeResponse <- armResponseWith page glob timeout
  takeResponse

-- | Registers for the next response whose URL matches the glob, before the
-- action that makes it runs. See 'armRequest'.
armResponse :: Page -> UrlGlob -> Playwright (Playwright Response)
armResponse page glob = do
  timeout <- pageActionTimeout page
  armResponseWith page glob timeout

armResponseWith :: Page -> UrlGlob -> Timeout -> Playwright (Playwright Response)
armResponseWith page glob timeout =
  armResponseWhereWith page (\response -> matchesUrlGlob glob <$> responseUrl response) timeout

-- | Waits for the next response that the predicate accepts.
waitForResponseWhere :: Page -> (Response -> Playwright Bool) -> Playwright Response
waitForResponseWhere page predicate = do
  takeResponse <- armResponseWhere page predicate
  takeResponse

-- | Registers for the next response that the predicate accepts, before the
-- action that makes it runs. The predicate runs once per response event.
armResponseWhere :: Page -> (Response -> Playwright Bool) -> Playwright (Playwright Response)
armResponseWhere page predicate = do
  timeout <- pageActionTimeout page
  armResponseWhereWith page predicate timeout

armResponseWhereWith :: Page -> (Response -> Playwright Bool) -> Timeout -> Playwright (Playwright Response)
armResponseWhereWith page predicate timeout = do
  firstWaiter <- registerFreshEventWaiter guid (MethodName "response")
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventResponse
  pure (waitWithTimeout timeout (collectResponse firstWaiter))
  where
    guid = channelGuid (pageContextChannel page)
    collectResponse waitId = do
      params <- waitForWaiterUnlessClosed waitId (channelGuid (pageChannel page))
      event <- throwDecode params
      let response = responseFromRef (browserContextResponseEventResponse event)
      matched <- predicate response
      if matched
        then pure response
        else do
          nextWaiter <- registerEventWaiter guid (MethodName "response")
          collectResponse nextWaiter

waitForRequest :: Page -> UrlGlob -> Playwright Request
waitForRequest page glob = do
  timeout <- pageActionTimeout page
  waitForRequestWith page glob timeout

waitForRequestWith :: Page -> UrlGlob -> Timeout -> Playwright Request
waitForRequestWith page glob timeout = do
  takeRequest <- armRequestWith page glob timeout
  takeRequest

-- | Registers for the next request whose URL matches the glob, before the
-- action that makes it runs. The subscription is made here, not when the
-- returned action starts, so a request that fires first is not missed.
armRequest :: Page -> UrlGlob -> Playwright (Playwright Request)
armRequest page glob = do
  timeout <- pageActionTimeout page
  armRequestWith page glob timeout

armRequestWith :: Page -> UrlGlob -> Timeout -> Playwright (Playwright Request)
armRequestWith page glob timeout =
  armRequestWhereWith page (\request -> matchesUrlGlob glob <$> requestUrl request) timeout

-- | Waits for the next request that the predicate accepts.
waitForRequestWhere :: Page -> (Request -> Playwright Bool) -> Playwright Request
waitForRequestWhere page predicate = do
  takeRequest <- armRequestWhere page predicate
  takeRequest

-- | Registers for the next request that the predicate accepts, before the
-- action that makes it runs. See 'armRequest'.
armRequestWhere :: Page -> (Request -> Playwright Bool) -> Playwright (Playwright Request)
armRequestWhere page predicate = do
  timeout <- pageActionTimeout page
  armRequestWhereWith page predicate timeout

armRequestWhereWith :: Page -> (Request -> Playwright Bool) -> Timeout -> Playwright (Playwright Request)
armRequestWhereWith page predicate timeout = do
  -- Register before subscribing: events from before this call are dropped, and
  -- every event after the subscription reaches the waiter.
  firstWaiter <- registerFreshEventWaiter guid (MethodName "request")
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequest
  pure (waitWithTimeout timeout (collectRequest firstWaiter))
  where
    guid = channelGuid (pageContextChannel page)
    collectRequest waitId = do
      params <- waitForWaiterUnlessClosed waitId (channelGuid (pageChannel page))
      event <- throwDecode params
      let request = requestFromRef (browserContextRequestEventRequest event)
      matched <- predicate request
      if matched
        then pure request
        else do
          -- Events that arrive while no waiter is registered are buffered. An
          -- event that arrives while this loop evaluates the predicate is
          -- handed to any other registered consumer on this key (see TODO).
          nextWaiter <- registerEventWaiter guid (MethodName "request")
          collectRequest nextWaiter

waitForRequestFailed :: Page -> Playwright Request
waitForRequestFailed page = do
  timeout <- pageActionTimeout page
  waitForRequestFailedWith page timeout

waitForRequestFailedWith :: Page -> Timeout -> Playwright Request
waitForRequestFailedWith page timeout = do
  takeFailed <- armRequestFailedWith page timeout
  takeFailed

-- | Registers for the next failed request before the action that fails it runs.
armRequestFailed :: Page -> Playwright (Playwright Request)
armRequestFailed page = do
  timeout <- pageActionTimeout page
  armRequestFailedWith page timeout

armRequestFailedWith :: Page -> Timeout -> Playwright (Playwright Request)
armRequestFailedWith page timeout = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequestFailed
  waitId <- registerEventWaiter (channelGuid (pageContextChannel page)) (MethodName "requestFailed")
  pure $ waitWithTimeout timeout $ do
    params <- waitForWaiter waitId
    event <- throwDecode params
    pure (requestFromRef (browserContextRequestFailedEventRequest event))

waitForRequestFinished :: Page -> Playwright Request
waitForRequestFinished page = do
  timeout <- pageActionTimeout page
  waitWithTimeout timeout $ do
    subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequestFinished
    params <-
      waitForChannelEvent
        (channelGuid (pageContextChannel page))
        (MethodName "requestFinished")
    event <- throwDecode params
    pure
      ( requestFromRef
          (browserContextRequestFinishedEventRequest event)
      )

waitForPopup :: Page -> Playwright Page
waitForPopup page = do
  takePopup <- armPopup page
  takePopup

armPopup :: Page -> Playwright (Playwright Page)
armPopup page = do
  timeout <- pageActionTimeout page
  pure (waitWithTimeout timeout (collectPopup page))

collectPopup :: Page -> Playwright Page
collectPopup page = do
  params <-
    waitForChannelEvent
      (channelGuid (pageContextChannel page))
      (MethodName "page")
  event <- throwDecode params
  candidate <- pageFromRef (pageContextChannel page) (browserContextPageEventPage event)
  parent <- opener candidate
  case parent of
    Just existing
      | pagesEqual existing page ->
          pure candidate
    _ ->
      collectPopup page
