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

import Control.Concurrent.STM
  ( atomically
  , takeTMVar
  )
import Control.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( Channel (channelConnection, channelGuid)
  )
import Playwright.Connection
  ( registerEventHandler
  , registerEventWaiter
  , registerFutureEventWaiter
  , waitForChannelEvent
  , waitForFutureChannelEvent
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

waitForSelector :: Page -> Selector -> WaitForState -> IO ()
waitForSelector page selector state = do
  timeout <- pageActionTimeout page
  waitForSelectorFrame (pageMainFrame page) selector state timeout Nothing

waitForSelectorHandle :: Page -> Selector -> WaitForState -> IO (Maybe ElementHandle)
waitForSelectorHandle page selector state = do
  timeout <- pageActionTimeout page
  waitForSelectorHandleWith page selector state timeout

waitForSelectorHandleWith :: Page -> Selector -> WaitForState -> Timeout -> IO (Maybe ElementHandle)
waitForSelectorHandleWith page selector state timeout =
  waitForSelectorHandleFrame (pageMainFrame page) selector state timeout Nothing

waitForSelectorStrict :: Page -> Selector -> WaitForState -> IO ()
waitForSelectorStrict page selector state = do
  timeout <- pageActionTimeout page
  waitForSelectorFrame (pageMainFrame page) selector state timeout (Just True)

waitForLoadState :: Page -> LoadState -> IO ()
waitForLoadState page =
  waitForLoadStateFrame (pageMainFrame page)

onLoad :: Page -> IO () -> IO ()
onLoad page run =
  registerEventHandler
    (channelConnection (pageChannel page))
    (channelGuid (frameChannel (pageMainFrame page)))
    (MethodName "loadstate")
    $ \params -> do
      event <- either throwIO pure (fromJsonValue params)
      case frameLoadstateEventAdd event of
        Just LifecycleEventLoad ->
          run
        _ ->
          pure ()

waitForLoadEvent :: Page -> IO ()
waitForLoadEvent page = do
  timeout <- pageNavigationTimeout page
  takeLoad <- armLoadEvent page
  waitWithTimeout timeout takeLoad

armLoadEvent :: Page -> IO (IO ())
armLoadEvent page = do
  box <-
    registerFutureEventWaiter
      (channelConnection (pageChannel page))
      (channelGuid (frameChannel (pageMainFrame page)))
      (MethodName "loadstate")
  pure $ do
    params <- atomically (takeTMVar box)
    event <- either throwIO pure (fromJsonValue params)
    case frameLoadstateEventAdd event of
      Just LifecycleEventLoad ->
        pure ()
      _ -> do
        takeLoad <- armLoadEvent page
        takeLoad

waitForTimeout :: Page -> Timeout -> IO ()
waitForTimeout page =
  waitForTimeoutFrame (pageMainFrame page)

waitForFunction :: Page -> Text -> IO JsHandle
waitForFunction page expression = do
  timeout <- pageActionTimeout page
  waitForFunctionWith page expression timeout

waitForFunctionWith :: Page -> Text -> Timeout -> IO JsHandle
waitForFunctionWith page expression timeout =
  waitForFunctionFrame (pageMainFrame page) expression undefinedArgument timeout

waitForURL :: Page -> UrlGlob -> IO ()
waitForURL page glob = do
  timeout <- pageNavigationTimeout page
  waitForURLWith page glob timeout

waitForURLWith :: Page -> UrlGlob -> Timeout -> IO ()
waitForURLWith page glob timeout =
  waitWithTimeout timeout $ do
    waitUntilUrl
      (channelConnection (pageChannel page))
      (channelGuid (frameChannel (pageMainFrame page)))
      (matchesUrlGlob glob)
    waitForLoadState page LoadStateLoad

waitForNavigation :: Page -> IO (Maybe Response)
waitForNavigation page = do
  timeout <- pageNavigationTimeout page
  waitForNavigationWith page timeout Nothing

waitForFrameNavigated :: Page -> IO ()
waitForFrameNavigated page = do
  timeout <- pageNavigationTimeout page
  takeNavigated <- armFrameNavigated page
  waitWithTimeout timeout takeNavigated

armFrameNavigated :: Page -> IO (IO ())
armFrameNavigated page = do
  let
    conn = channelConnection (pageChannel page)
    frameGuid = channelGuid (frameChannel (pageMainFrame page))
  box <- registerFutureEventWaiter conn frameGuid (MethodName "navigated")
  pure $ do
    _ <- atomically (takeTMVar box)
    pure ()

waitForFrameDetached :: Page -> IO Frame
waitForFrameDetached page = do
  timeout <- pageNavigationTimeout page
  takeFrame <- armFrameDetached page
  waitWithTimeout timeout takeFrame

armFrameDetached :: Page -> IO (IO Frame)
armFrameDetached page = do
  let
    conn = channelConnection (pageChannel page)
    guid = channelGuid (pageChannel page)
  box <- registerFutureEventWaiter conn guid (MethodName "frameDetached")
  pure $ do
    params <- atomically (takeTMVar box)
    event <- either throwIO pure (fromJsonValue params)
    frameFromRef conn (pageFrameDetachedEventFrame event)

waitForNavigationWith :: Page -> Timeout -> Maybe UrlGlob -> IO (Maybe Response)
waitForNavigationWith page timeout glob =
  waitWithTimeout timeout (collectNavigation page glob)

collectNavigation :: Page -> Maybe UrlGlob -> IO (Maybe Response)
collectNavigation page glob = do
  let
    conn = channelConnection (pageChannel page)
    frameGuid = channelGuid (frameChannel (pageMainFrame page))
  params <- waitForFutureChannelEvent conn frameGuid (MethodName "navigated")
  event <- either throwIO pure (fromJsonValue params)
  let matches = maybe True (`matchesUrlGlob` frameNavigatedEventUrl event) glob
  if matches
    then do
      waitForLoadState page LoadStateLoad
      case frameNavigatedEventNewDocument event >>= frameNavigatedEventNewDocumentRequest of
        Nothing ->
          pure Nothing
        Just requestRef -> do
          responseRef <- requestResponseRef (requestFromRef conn requestRef)
          pure (fmap (responseFromRef conn) responseRef)
    else
      collectNavigation page glob

armDialog :: Page -> IO (IO Dialog)
armDialog page = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventDialog
  box <-
    registerEventWaiter
      (channelConnection (pageContextChannel page))
      (channelGuid (pageContextChannel page))
      (MethodName "dialog")
  pure $ do
    params <- atomically (takeTMVar box)
    event <- either throwIO pure (fromJsonValue params)
    dialogFromRef
      (channelConnection (pageContextChannel page))
      (browserContextDialogEventDialog event)

waitForDialog :: Page -> IO Dialog
waitForDialog page = do
  takeDialog <- armDialog page
  takeDialog

armConsole :: Page -> IO (IO ConsoleMessage)
armConsole page = do
  subscribeContext page BrowserContextUpdateSubscriptionParamsEventConsole
  box <-
    registerEventWaiter
      (channelConnection (pageContextChannel page))
      (channelGuid (pageContextChannel page))
      (MethodName "console")
  pure $ do
    params <- atomically (takeTMVar box)
    event <- either throwIO pure (fromJsonValue params :: Either PlaywrightError BrowserContextConsoleEvent)
    consoleMessageFromEvent (channelConnection (pageContextChannel page)) event

waitForConsole :: Page -> IO ConsoleMessage
waitForConsole page = do
  takeMessage <- armConsole page
  takeMessage

waitForResponse :: Page -> UrlGlob -> IO Response
waitForResponse page glob = do
  timeout <- pageActionTimeout page
  waitForResponseWith page glob timeout

waitForResponseWith :: Page -> UrlGlob -> Timeout -> IO Response
waitForResponseWith page glob timeout =
  waitWithTimeout timeout $ do
    subscribeContext page BrowserContextUpdateSubscriptionParamsEventResponse
    collectResponse
  where
    conn = channelConnection (pageContextChannel page)
    guid = channelGuid (pageContextChannel page)
    collectResponse = do
      params <- waitForChannelEvent conn guid (MethodName "response")
      event <- either throwIO pure (fromJsonValue params)
      let response = responseFromRef conn (browserContextResponseEventResponse event)
      current <- responseUrl response
      if matchesUrlGlob glob current
        then pure response
        else collectResponse

waitForRequest :: Page -> UrlGlob -> IO Request
waitForRequest page glob = do
  timeout <- pageActionTimeout page
  waitForRequestWith page glob timeout

waitForRequestWith :: Page -> UrlGlob -> Timeout -> IO Request
waitForRequestWith page glob timeout =
  waitWithTimeout timeout $ do
    subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequest
    collectRequest
  where
    conn = channelConnection (pageContextChannel page)
    guid = channelGuid (pageContextChannel page)
    collectRequest = do
      params <- waitForChannelEvent conn guid (MethodName "request")
      event <- either throwIO pure (fromJsonValue params)
      let request = requestFromRef conn (browserContextRequestEventRequest event)
      current <- requestUrl request
      if matchesUrlGlob glob current
        then pure request
        else collectRequest

waitForRequestFailed :: Page -> IO Request
waitForRequestFailed page = do
  timeout <- pageActionTimeout page
  waitForRequestFailedWith page timeout

waitForRequestFailedWith :: Page -> Timeout -> IO Request
waitForRequestFailedWith page timeout =
  waitWithTimeout timeout $ do
    subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequestFailed
    params <-
      waitForChannelEvent
        (channelConnection (pageContextChannel page))
        (channelGuid (pageContextChannel page))
        (MethodName "requestFailed")
    event <- either throwIO pure (fromJsonValue params)
    pure
      ( requestFromRef
          (channelConnection (pageContextChannel page))
          (browserContextRequestFailedEventRequest event)
      )

waitForRequestFinished :: Page -> IO Request
waitForRequestFinished page = do
  timeout <- pageActionTimeout page
  waitWithTimeout timeout $ do
    subscribeContext page BrowserContextUpdateSubscriptionParamsEventRequestFinished
    params <-
      waitForChannelEvent
        (channelConnection (pageContextChannel page))
        (channelGuid (pageContextChannel page))
        (MethodName "requestFinished")
    event <- either throwIO pure (fromJsonValue params)
    pure
      ( requestFromRef
          (channelConnection (pageContextChannel page))
          (browserContextRequestFinishedEventRequest event)
      )

waitForPopup :: Page -> IO Page
waitForPopup page = do
  takePopup <- armPopup page
  takePopup

armPopup :: Page -> IO (IO Page)
armPopup page =
  pure (collectPopup page)

collectPopup :: Page -> IO Page
collectPopup page = do
  params <-
    waitForChannelEvent
      (channelConnection (pageContextChannel page))
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
