module Playwright.Page.Wait
  ( armConsole
  , armDialog
  , armFrameDetached
  , armFrameNavigated
  , armLoadEvent
  , armPopup
  , onLoad
  , waitForConsole
  , waitForDialog
  , waitForFrameDetached
  , waitForFrameNavigated
  , waitForFunction
  , waitForFunctionWith
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
import UnliftIO.Exception
  ( throwIO
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
  , waitForFutureChannelEvent
  , waitForWaiter
  , waitUntilUrl
  )
import Playwright.ConsoleMessage
  ( ConsoleMessage
  , consoleMessageFromEvent
  )
import Playwright.Dialog
  ( Dialog
  , dialogFromRef
  )
import Playwright.ElementHandle
  ( ElementHandle
  )
import Playwright.Error
  ( PlaywrightError
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
  ( fromJsonValue
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
  ( BrowserContextConsoleEvent
  , BrowserContextDialogEvent (browserContextDialogEventDialog)
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
  ( undefinedArgument
  )
import Playwright.Timeout
  ( Timeout
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
      event <- either throwIO pure (fromJsonValue params)
      case frameLoadstateEventAdd event of
        Just LifecycleEventLoad ->
          run
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
    event <- either throwIO pure (fromJsonValue params)
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
  waitForFunctionFrame (pageMainFrame page) expression undefinedArgument timeout

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
    event <- either throwIO pure (fromJsonValue params)
    frameFromRef (pageFrameDetachedEventFrame event)

waitForNavigationWith :: Page -> Timeout -> Maybe UrlGlob -> Playwright (Maybe Response)
waitForNavigationWith page timeout glob =
  waitWithTimeout timeout (collectNavigation page glob)

collectNavigation :: Page -> Maybe UrlGlob -> Playwright (Maybe Response)
collectNavigation page glob = do
  let
    frameGuid = channelGuid (frameChannel (pageMainFrame page))
  params <- waitForFutureChannelEvent frameGuid (MethodName "navigated")
  event <- either throwIO pure (fromJsonValue params)
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
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventDialog
  waitId <-
    registerEventWaiter
      (channelGuid (pageContextChannel page))
      (MethodName "dialog")
  pure $ do
    params <- waitForWaiter waitId
    event <- either throwIO pure (fromJsonValue params)
    dialogFromRef
      (browserContextDialogEventDialog event)

waitForDialog :: Page -> Playwright Dialog
waitForDialog page = do
  takeDialog <- armDialog page
  takeDialog

armConsole :: Page -> Playwright (Playwright ConsoleMessage)
armConsole page = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventConsole
  waitId <-
    registerEventWaiter
      (channelGuid (pageContextChannel page))
      (MethodName "console")
  pure $ do
    params <- waitForWaiter waitId
    event <- either throwIO pure (fromJsonValue params :: Either PlaywrightError BrowserContextConsoleEvent)
    consoleMessageFromEvent event

waitForConsole :: Page -> Playwright ConsoleMessage
waitForConsole page = do
  takeMessage <- armConsole page
  takeMessage

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
      params <- waitForChannelEvent guid (MethodName "response")
      event <- either throwIO pure (fromJsonValue params)
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
      params <- waitForChannelEvent guid (MethodName "request")
      event <- either throwIO pure (fromJsonValue params)
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
    event <- either throwIO pure (fromJsonValue params)
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
    event <- either throwIO pure (fromJsonValue params)
    pure
      ( requestFromRef
          (browserContextRequestFinishedEventRequest event)
      )

waitForPopup :: Page -> Playwright Page
waitForPopup page = do
  takePopup <- armPopup page
  takePopup

armPopup :: Page -> Playwright (Playwright Page)
armPopup page =
  pure (collectPopup page)

collectPopup :: Page -> Playwright Page
collectPopup page = do
  params <-
    waitForChannelEvent
      (channelGuid (pageContextChannel page))
      (MethodName "page")
  event <- either throwIO pure (fromJsonValue params)
  candidate <- pageFromRef (pageContextChannel page) (browserContextPageEventPage event)
  parent <- opener candidate
  case parent of
    Just existing
      | pagesEqual existing page ->
          pure candidate
    _ ->
      collectPopup page
