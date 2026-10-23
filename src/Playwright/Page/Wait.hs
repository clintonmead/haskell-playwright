module Playwright.Page.Wait
  ( armConsole
  , armDialog
  , onDialog
  , onPageDialogClosed
  , armFrameDetached
  , armFrameNavigated
  , armNavigatedFrame
  , armLoadEvent
  , armPopup
  , onLoad
  , onLoadEvent
  , onDomContentLoadedEvent
  , onConsole
  , removeAllConsoleListeners
  , waitForConsole
  , waitForConsoleWhere
  , waitForDialog
  , waitForFrameDetached
  , waitForFrameNavigated
  , waitForNavigatedFrame
  , waitForExpression
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
  , waitForNavigationFrame
  , waitForNavigationFrameOptions
  , armNavigationFrame
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
  , armRequestFinished
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
  , waitForURLOptions
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
import UnliftIO.Async
  ( race
  )
import UnliftIO.Exception
  ( throwIO
  )
import Playwright.Channel
  ( Channel (channelGuid)
  )
import Playwright.EventHandler
  ( ListenerRemoval
  )
import Playwright.Connection
  ( lookupContextBaseUrl
  , lookupMainFramePage
  , registerEventHandler
  , registerEventWaiter
  , registerScopedEventHandler
  , removeScopedEventHandlers
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
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (ProtocolFailure, protocolFailureMessage, protocolFailureName, protocolFailureStack)
  )
import Playwright.Frame
  ( Frame (frameChannel)
  , frameContextBaseUrl
  , frameFromRef
  , frameNavigationTimeout
  , mainFrameGuid
  )
import Playwright.Frame.Wait
  ( waitForExpressionFrame
  , waitForFunctionFrame
  , waitForLoadStateFrame
  , waitForSelectorFrame
  , waitForSelectorHandleFrame
  , waitForTimeoutFrame
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.JsHandle
  ( JsHandle
  )
import Playwright.LoadState
  ( LoadState (LoadStateCommit, LoadStateDomContentLoaded, LoadStateLoad, LoadStateNetworkIdle)
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
  ( BrowserContextDialogClosedEvent (browserContextDialogClosedEventDialog)
  , BrowserContextDialogEvent (browserContextDialogEventDialog)
  , LifecycleEvent (LifecycleEventDomcontentloaded, LifecycleEventLoad)
  , BrowserContextPageEvent (browserContextPageEventPage)
  , BrowserContextRequestEvent (browserContextRequestEventRequest)
  , BrowserContextRequestFailedEvent (browserContextRequestFailedEventRequest)
  , BrowserContextRequestFinishedEvent (browserContextRequestFinishedEventRequest)
  , BrowserContextResponseEvent (browserContextResponseEventResponse)
  , BrowserContextUpdateSubscriptionParamsEvent (BrowserContextUpdateSubscriptionParamsEventConsole, BrowserContextUpdateSubscriptionParamsEventDialog, BrowserContextUpdateSubscriptionParamsEventDialogClosed, BrowserContextUpdateSubscriptionParamsEventRequest, BrowserContextUpdateSubscriptionParamsEventRequestFailed, BrowserContextUpdateSubscriptionParamsEventRequestFinished, BrowserContextUpdateSubscriptionParamsEventResponse)
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
import Playwright.Url
  ( Url
  )
import Playwright.UrlMatch
  ( UrlMatch
  , matchesUrlMatch
  , resolveUrlMatch
  )
import Playwright.WaitForNavigationOptions
  ( WaitForNavigationOptions (waitForNavigationOptionsTimeout, waitForNavigationOptionsUrl, waitForNavigationOptionsWaitUntil)
  , defaultWaitForNavigationOptions
  )
import Playwright.WaitForURLOptions
  ( WaitForURLOptions (waitForURLOptionsTimeout, waitForURLOptionsWaitUntil)
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
  onLifecycleEvent page LifecycleEventLoad (const run)

-- | Runs the handler with the page each time its load event fires. The page is
-- passed back to the handler, as the upstream "load" listener receives it.
onLoadEvent :: Page -> (Page -> Playwright ()) -> Playwright ()
onLoadEvent page run =
  onLifecycleEvent page LifecycleEventLoad run

-- | Runs the handler with the page each time its domcontentloaded event fires.
onDomContentLoadedEvent :: Page -> (Page -> Playwright ()) -> Playwright ()
onDomContentLoadedEvent page run =
  onLifecycleEvent page LifecycleEventDomcontentloaded run

-- | Runs the handler with the page for each main-frame lifecycle event of the
-- given kind.
onLifecycleEvent :: Page -> LifecycleEvent -> (Page -> Playwright ()) -> Playwright ()
onLifecycleEvent page lifecycle run =
  registerEventHandler
    (channelGuid (frameChannel (pageMainFrame page)))
    (MethodName "loadstate")
    $ \params -> do
      event <- throwDecode params
      if frameLoadstateEventAdd event == Just lifecycle
        then run page
        else pure ()

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

onPageDialogClosed :: Page -> (Dialog -> Playwright ()) -> Playwright ()
onPageDialogClosed page run = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventDialogClosed
  registerEventHandler
    (channelGuid (pageContextChannel page))
    (MethodName "dialogClosed")
    $ \params -> do
      event <- throwDecode params
      dialog <- dialogFromRef (browserContextDialogClosedEventDialog event)
      if maybe False (`pagesEqual` page) (dialogPage dialog)
        then run dialog
        else pure ()

onConsole :: Page -> (ConsoleMessage -> Playwright ()) -> Playwright ()
onConsole page run = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventConsole
  registerScopedEventHandler
    (channelGuid (pageChannel page))
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

-- | Removes every console listener added with 'onConsole' for this page, as
-- upstream's @removeAllListeners('console')@ does. 'RemoveWaiting' waits for
-- listeners still running and rethrows the first error one of them raised.
removeAllConsoleListeners :: Page -> ListenerRemoval -> Playwright ()
removeAllConsoleListeners page =
  removeScopedEventHandlers (channelGuid (pageChannel page)) (MethodName "console")

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

-- The string form of upstream waitForFunction: the text is evaluated as an expression.
waitForExpression :: Page -> Text -> Playwright JsHandle
waitForExpression page expression = do
  timeout <- pageActionTimeout page
  waitForExpressionFrame (pageMainFrame page) expression timeout

waitForURL :: Page -> UrlMatch -> Playwright ()
waitForURL page glob = do
  timeout <- pageNavigationTimeout page
  waitForURLWith page glob timeout

waitForURLWith :: Page -> UrlMatch -> Timeout -> Playwright ()
waitForURLWith page glob timeout =
  waitForURLWithState page glob timeout LoadStateLoad

waitForURLOptions :: Page -> UrlMatch -> WaitForURLOptions -> Playwright ()
waitForURLOptions page glob opts = do
  timeout <-
    maybe (pageNavigationTimeout page) pure (waitForURLOptionsTimeout opts)
  waitForURLWithState
    page
    glob
    timeout
    (fromMaybe LoadStateLoad (waitForURLOptionsWaitUntil opts))

waitForURLWithState :: Page -> UrlMatch -> Timeout -> LoadState -> Playwright ()
waitForURLWithState page glob timeout state =
  waitWithTimeout timeout $ do
    base <- lookupContextBaseUrl (channelGuid (pageContextChannel page))
    waitUntilUrl
      (channelGuid (frameChannel (pageMainFrame page)))
      (matchesUrlMatch (resolveUrlMatch base glob))
    waitForLoadState page state

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

waitForNavigationWith :: Page -> Timeout -> Maybe UrlMatch -> Playwright (Maybe Response)
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
  base <- lookupContextBaseUrl (channelGuid (pageContextChannel page))
  if navigationMatches base opts event
    then completeNavigation page opts event
    else collectNavigation page opts

completeNavigation :: Page -> WaitForNavigationOptions -> FrameNavigatedEvent -> Playwright (Maybe Response)
completeNavigation page opts event = do
  case fromMaybe LoadStateLoad (waitForNavigationOptionsWaitUntil opts) of
    LoadStateCommit ->
      pure ()
    state ->
      waitForLoadState page state
  navigatedResponse event

-- | Frame-level counterpart of 'waitForNavigationOptions'. Fails if the frame
-- detaches before the navigation completes, as upstream does.
waitForNavigationFrame :: Frame -> Playwright (Maybe Response)
waitForNavigationFrame frame =
  waitForNavigationFrameOptions frame defaultWaitForNavigationOptions

waitForNavigationFrameOptions :: Frame -> WaitForNavigationOptions -> Playwright (Maybe Response)
waitForNavigationFrameOptions frame opts = do
  takeNavigation <- armNavigationFrame frame opts
  takeNavigation

-- | Registers for the next navigation of the frame now, so it is not missed
-- if it starts before the returned action runs. The returned action races the
-- navigation against a detach of the frame.
armNavigationFrame :: Frame -> WaitForNavigationOptions -> Playwright (Playwright (Maybe Response))
armNavigationFrame frame opts = do
  timeout <-
    maybe (frameNavigationTimeout frame) pure (waitForNavigationOptionsTimeout opts)
  let
    frameGuid = channelGuid (frameChannel frame)
  waitId <-
    registerFutureEventWaiter frameGuid (MethodName "navigated")
  mainGuid <- mainFrameGuid frame
  pageGuid <- lookupMainFramePage mainGuid
  detachId <-
    case pageGuid of
      Nothing ->
        pure Nothing
      Just page ->
        Just <$> registerFutureEventWaiterWhere page (MethodName "frameDetached") (isDetachOf frameGuid)
  pure $ do
    let
      navigation = waitWithTimeout timeout $ do
        params <- waitForWaiter waitId
        finishNavigationFrame frame opts params
    case detachId of
      Nothing ->
        navigation
      Just detached ->
        either id id <$> race navigation (waitForWaiter detached >> frameDetachedError opts)

finishNavigationFrame :: Frame -> WaitForNavigationOptions -> Value -> Playwright (Maybe Response)
finishNavigationFrame frame opts params = do
  event <- throwDecode params
  base <- frameContextBaseUrl frame
  if navigationMatches base opts event
    then completeNavigationFrame frame opts event
    else collectNavigationFrame frame opts

collectNavigationFrame :: Frame -> WaitForNavigationOptions -> Playwright (Maybe Response)
collectNavigationFrame frame opts = do
  params <- waitForFutureChannelEvent (channelGuid (frameChannel frame)) (MethodName "navigated")
  finishNavigationFrame frame opts params

completeNavigationFrame :: Frame -> WaitForNavigationOptions -> FrameNavigatedEvent -> Playwright (Maybe Response)
completeNavigationFrame frame opts event = do
  case fromMaybe LoadStateLoad (waitForNavigationOptionsWaitUntil opts) of
    LoadStateCommit ->
      pure ()
    state ->
      waitForLoadStateFrame frame state
  navigatedResponse event

navigationMatches :: Maybe Url -> WaitForNavigationOptions -> FrameNavigatedEvent -> Bool
navigationMatches base opts event =
  maybe True (\glob -> matchesUrlMatch (resolveUrlMatch base glob) (frameNavigatedEventUrl event)) (waitForNavigationOptionsUrl opts)

navigatedResponse :: FrameNavigatedEvent -> Playwright (Maybe Response)
navigatedResponse event =
  case frameNavigatedEventNewDocument event >>= frameNavigatedEventNewDocumentRequest of
    Nothing ->
      pure Nothing
    Just requestRef -> do
      responseRef <- requestResponseRef (requestFromRef requestRef)
      pure (fmap responseFromRef responseRef)

isDetachOf :: Guid -> Value -> Bool
isDetachOf frameGuid params =
  case fromJSON params of
    Success event ->
      channelRefGuid (pageFrameDetachedEventFrame event) == frameGuid
    Error _ ->
      False

-- | The error upstream raises when the frame detaches mid-navigation. The
-- wording matches Playwright's, and the call log names the requested load
-- state.
frameDetachedError :: WaitForNavigationOptions -> Playwright a
frameDetachedError opts =
  throwIO
    ( ProtocolRpc
        ProtocolFailure
          { protocolFailureName = "Error"
          , protocolFailureMessage =
              "Navigating frame was detached!\nCall log:\n  - waiting for navigation until \""
                <> loadStateName (fromMaybe LoadStateLoad (waitForNavigationOptionsWaitUntil opts))
                <> "\""
          , protocolFailureStack = Nothing
          }
    )

loadStateName :: LoadState -> Text
loadStateName state =
  case state of
    LoadStateCommit -> "commit"
    LoadStateDomContentLoaded -> "domcontentloaded"
    LoadStateLoad -> "load"
    LoadStateNetworkIdle -> "networkidle"

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

waitForResponse :: Page -> UrlMatch -> Playwright Response
waitForResponse page glob = do
  timeout <- pageActionTimeout page
  waitForResponseWith page glob timeout

waitForResponseWith :: Page -> UrlMatch -> Timeout -> Playwright Response
waitForResponseWith page glob timeout = do
  takeResponse <- armResponseWith page glob timeout
  takeResponse

-- | Registers for the next response whose URL matches the glob, before the
-- action that makes it runs. See 'armRequest'.
armResponse :: Page -> UrlMatch -> Playwright (Playwright Response)
armResponse page glob = do
  timeout <- pageActionTimeout page
  armResponseWith page glob timeout

armResponseWith :: Page -> UrlMatch -> Timeout -> Playwright (Playwright Response)
armResponseWith page glob timeout = do
  base <- lookupContextBaseUrl (channelGuid (pageContextChannel page))
  let
    resolved = resolveUrlMatch base glob
  armResponseWhereWith page (\response -> matchesUrlMatch resolved <$> responseUrl response) timeout

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

waitForRequest :: Page -> UrlMatch -> Playwright Request
waitForRequest page glob = do
  timeout <- pageActionTimeout page
  waitForRequestWith page glob timeout

waitForRequestWith :: Page -> UrlMatch -> Timeout -> Playwright Request
waitForRequestWith page glob timeout = do
  takeRequest <- armRequestWith page glob timeout
  takeRequest

-- | Registers for the next request whose URL matches the glob, before the
-- action that makes it runs. The subscription is made here, not when the
-- returned action starts, so a request that fires first is not missed.
armRequest :: Page -> UrlMatch -> Playwright (Playwright Request)
armRequest page glob = do
  timeout <- pageActionTimeout page
  armRequestWith page glob timeout

armRequestWith :: Page -> UrlMatch -> Timeout -> Playwright (Playwright Request)
armRequestWith page glob timeout = do
  base <- lookupContextBaseUrl (channelGuid (pageContextChannel page))
  let
    resolved = resolveUrlMatch base glob
  armRequestWhereWith page (\request -> matchesUrlMatch resolved <$> requestUrl request) timeout

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
  takeFinished <- armRequestFinished page
  takeFinished

-- | Registers for the next finished request before the action that finishes it
-- runs. See 'armRequestFailed'.
armRequestFinished :: Page -> Playwright (Playwright Request)
armRequestFinished page = do
  timeout <- pageActionTimeout page
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequestFinished
  waitId <- registerEventWaiter (channelGuid (pageContextChannel page)) (MethodName "requestFinished")
  pure $ waitWithTimeout timeout $ do
    params <- waitForWaiter waitId
    event <- throwDecode params
    pure (requestFromRef (browserContextRequestFinishedEventRequest event))

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
