module Playwright.Page.Wait
  ( armConsole
  , armDialog
  , armFrameDetached
  , armFrameNavigated
  , armLoadEvent
  , armPopup
  , onLoad
  , onConsole
  , waitForConsole
  , waitForConsoleWhere
  , waitForDialog
  , waitForFrameDetached
  , waitForFrameNavigated
  , waitForFunction
  , waitForFunctionOnFrame
  , waitForFunctionPolling
  , waitForFunctionWith
  , waitForFunctionWithArg
  , waitForLoadEvent
  , waitForLoadState
  , waitForNavigation
  , waitForNavigationWith
  , waitForPopup
  , waitForRequest
  , waitForRequestFailed
  , waitForRequestFailedWith
  , waitForRequestFinished
  , waitForRequestWith
  , waitForResponse
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
  , registerFutureEventWaiter
  , waitForChannelEvent
  , waitForChannelEventUnlessClosed
  , waitForFutureChannelEvent
  , waitForWaiter
  , waitUntilUrl
  )
import Playwright.ConsoleMessage
  ( ConsoleMessage (consoleMessagePage)
  , consoleMessageFromEvent
  )
import Playwright.Dialog
  ( Dialog
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
  ( LoadState (LoadStateLoad)
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

armLoadEvent :: Page -> Playwright (Playwright ())
armLoadEvent page = do
  waitId <-
    registerFutureEventWaiter
      (channelGuid (frameChannel (pageMainFrame page)))
      (MethodName "loadstate")
  pure $ do
    params <- waitForWaiter waitId
    event <- throwDecode params
    case frameLoadstateEventAdd event of
      Just LifecycleEventLoad ->
        pure ()
      _ -> do
        takeLoad <- armLoadEvent page
        takeLoad

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
waitForNavigation page = do
  timeout <- pageNavigationTimeout page
  waitForNavigationWith page timeout Nothing

waitForFrameNavigated :: Page -> Playwright ()
waitForFrameNavigated page = do
  timeout <- pageNavigationTimeout page
  takeNavigated <- armFrameNavigated page
  waitWithTimeout timeout takeNavigated

armFrameNavigated :: Page -> Playwright (Playwright ())
armFrameNavigated page = do
  let
    frameGuid = channelGuid (frameChannel (pageMainFrame page))
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
  waitWithTimeout timeout (collectNavigation page glob)

collectNavigation :: Page -> Maybe UrlGlob -> Playwright (Maybe Response)
collectNavigation page glob = do
  let
    frameGuid = channelGuid (frameChannel (pageMainFrame page))
  params <- waitForFutureChannelEvent frameGuid (MethodName "navigated")
  event <- throwDecode params
  let matches = maybe True (`matchesUrlGlob` frameNavigatedEventUrl event) glob
  if matches
    then do
      waitForLoadState page LoadStateLoad
      case frameNavigatedEventNewDocument event >>= frameNavigatedEventNewDocumentRequest of
        Nothing ->
          pure Nothing
        Just requestRef -> do
          responseRef <- requestResponseRef (requestFromRef requestRef)
          pure (fmap (responseFromRef) responseRef)
    else
      collectNavigation page glob

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
waitForResponseWith page glob timeout =
  waitWithTimeout timeout $ do
    subscribeContext page BrowserContextUpdateSubscriptionParamsEventResponse
    collectResponse
  where
    guid = channelGuid (pageContextChannel page)
    collectResponse = do
      params <-
        waitForChannelEventUnlessClosed
          guid
          (MethodName "response")
          (channelGuid (pageChannel page))
      event <- throwDecode params
      let response = responseFromRef (browserContextResponseEventResponse event)
      current <- responseUrl response
      if matchesUrlGlob glob current
        then pure response
        else collectResponse

waitForRequest :: Page -> UrlGlob -> Playwright Request
waitForRequest page glob = do
  timeout <- pageActionTimeout page
  waitForRequestWith page glob timeout

waitForRequestWith :: Page -> UrlGlob -> Timeout -> Playwright Request
waitForRequestWith page glob timeout =
  waitWithTimeout timeout $ do
    subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequest
    collectRequest
  where
    guid = channelGuid (pageContextChannel page)
    collectRequest = do
      params <-
        waitForChannelEventUnlessClosed
          guid
          (MethodName "request")
          (channelGuid (pageChannel page))
      event <- throwDecode params
      let request = requestFromRef (browserContextRequestEventRequest event)
      current <- requestUrl request
      if matchesUrlGlob glob current
        then pure request
        else collectRequest

waitForRequestFailed :: Page -> Playwright Request
waitForRequestFailed page = do
  timeout <- pageActionTimeout page
  waitForRequestFailedWith page timeout

waitForRequestFailedWith :: Page -> Timeout -> Playwright Request
waitForRequestFailedWith page timeout =
  waitWithTimeout timeout $ do
    subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequestFailed
    params <-
      waitForChannelEvent
        (channelGuid (pageContextChannel page))
        (MethodName "requestFailed")
    event <- throwDecode params
    pure
      ( requestFromRef
          (browserContextRequestFailedEventRequest event)
      )

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
