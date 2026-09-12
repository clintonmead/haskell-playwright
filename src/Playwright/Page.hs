module Playwright.Page
  ( Page (..)
  , pageFromRef
  , bringToFront
  , closePage
  , content
  , goto
  , locator
  , locatorOnFrame
  , locatorPage
  , pdf
  , pdfWith
  , screenshot
  , setContent
  , title
  , url
  , waitForSelector
  , waitForSelectorHandle
  , waitForSelectorHandleWith
  , getByLabel
  , getByRole
  , getByTestId
  , getByText
  , goBack
  , goForward
  , isClosed
  , reload
  , requestGC
  , querySelector
  , querySelectorAll
  , querySelectorStrict
  , setViewportSize
  , frameByName
  , frameLocator
  , frames
  , mainFrame
  , addInitScript
  , addScriptTag
  , addStyleTag
  , setExtraHTTPHeaders
  , waitForFunction
  , waitForFunctionWith
  , waitForLoadState
  , waitForNavigation
  , waitForNavigationWith
  , waitForFrameNavigated
  , armFrameNavigated
  , waitForFrameDetached
  , armFrameDetached
  , waitForLoadEvent
  , armLoadEvent
  , waitForTimeout
  , waitForURL
  , waitForURLWith
  , armConsole
  , armDialog
  , waitForClose
  , waitForConsole
  , waitForDialog
  , waitForResponse
  , waitForResponseWith
  , waitForRequest
  , waitForRequestFailed
  , waitForRequestFailedWith
  , waitForRequestFinished
  , waitForRequestWith
  , waitForSelectorStrict
  , armPopup
  , opener
  , pagesEqual
  , waitForPopup
  , subscribeContext
  , onLoad
  , pageActionTimeout
  , pageNavigationTimeout
  , pageRequest
  , setDefaultTimeout
  , setDefaultNavigationTimeout
  ) where

import Control.Concurrent.STM
  ( atomically
  , takeTMVar
  )
import Control.Exception
  ( try
  , throwIO
  )
import Data.List
  ( find
  )
import Data.Set qualified as Set
import Data.ByteString
  ( ByteString
  )
import Data.Text
  ( Text
  )
import Playwright.AriaRole
  ( AriaRole
  )
import Playwright.APIRequestContext
  ( APIRequestContext
  , apiRequestContextFor
  )
import Playwright.Channel
  ( BrowserContextTag
  , Channel (Channel, channelConnection, channelGuid)
  , PageTag
  , send
  )
import Playwright.Connection
  ( closedGuids
  , lookupObject
  , lookupTimeoutDefaults
  , pageOwnsContext
  , registerEventHandler
  , registerEventWaiter
  , registerFutureEventWaiter
  , registerMainFramePage
  , setTimeoutDefault
  , setTimeoutNavigation
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
import Playwright.Frame
  ( Frame (frameChannel)
  , childFrames
  , contentFrame
  , currentName
  , currentUrl
  , frameFromRef
  , frameSend
  , gotoFrame
  , setContentFrame
  , titleFrame
  )
import Playwright.Frame.Inspect
  ( querySelectorAllFrame
  , querySelectorFrame
  )
import Playwright.Frame.Wait
  ( waitForFunctionFrame
  , waitForLoadStateFrame
  , waitForSelectorFrame
  , waitForSelectorHandleFrame
  , waitForTimeoutFrame
  )
import Playwright.ElementHandle
  ( ElementHandle
  , elementHandleFromRef
  )
import Playwright.Error
  ( PlaywrightError (MissingObject, ProtocolRpc)
  , ProtocolFailure (protocolFailureName)
  )
import Playwright.Exactness
  ( Exactness
  )
import Playwright.GetBy
  ( getByLabelSelector
  , getByRoleSelector
  , getByTestIdSelector
  , getByTextSelector
  )
import Playwright.GetByRoleOptions
  ( GetByRoleOptions
  )
import Playwright.GotoOptions
  ( GotoOptions
  )
import Playwright.HttpHeader
  ( HttpHeader
  , toNameValue
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.Json.ProtocolBinary
  ( protocolBinaryToByteString
  )
import Playwright.JsHandle
  ( JsHandle
  )
import Playwright.LoadState
  ( LoadState (LoadStateLoad)
  )
import Playwright.Locator
  ( Locator (Locator, locatorContextChannel, locatorFrame, locatorMainFrame, locatorPageChannel, locatorSelector)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( BrowserContextCloseResult
  , BrowserContextConsoleEvent
  , BrowserContextDialogEvent (browserContextDialogEventDialog)
  , BrowserContextPageEvent (browserContextPageEventPage)
  , BrowserContextRequestEvent (browserContextRequestEventRequest)
  , BrowserContextRequestFailedEvent (browserContextRequestFailedEventRequest)
  , BrowserContextRequestFinishedEvent (browserContextRequestFinishedEventRequest)
  , BrowserContextResponseEvent (browserContextResponseEventResponse)
  , BrowserContextUpdateSubscriptionParamsEvent (BrowserContextUpdateSubscriptionParamsEventConsole, BrowserContextUpdateSubscriptionParamsEventDialog, BrowserContextUpdateSubscriptionParamsEventRequest, BrowserContextUpdateSubscriptionParamsEventRequestFailed, BrowserContextUpdateSubscriptionParamsEventRequestFinished, BrowserContextUpdateSubscriptionParamsEventResponse)
  , BrowserContextUpdateSubscriptionResult
  , FrameAddScriptTagParams (frameAddScriptTagParamsContent, frameAddScriptTagParamsType, frameAddScriptTagParamsUrl)
  , FrameAddScriptTagResult (frameAddScriptTagResultElement)
  , FrameAddStyleTagParams (frameAddStyleTagParamsContent, frameAddStyleTagParamsUrl)
  , FrameAddStyleTagResult (frameAddStyleTagResultElement)
  , FrameLoadstateEvent (frameLoadstateEventAdd)
  , FrameNavigatedEvent (frameNavigatedEventNewDocument, frameNavigatedEventUrl)
  , FrameNavigatedEventNewDocument (frameNavigatedEventNewDocumentRequest)
  , LifecycleEvent (LifecycleEventLoad)
  , PageAddInitScriptResult
  , PageBringToFrontResult
  , PageCloseResult
  , PageFrameDetachedEvent (pageFrameDetachedEventFrame)
  , PageGoBackResult (pageGoBackResultResponse)
  , PageGoForwardResult (pageGoForwardResultResponse)
  , PageInitializer (pageInitializerMainFrame, pageInitializerOpener)
  , PagePdfParams (pagePdfParamsOutline, pagePdfParamsTagged)
  , PagePdfResult (pagePdfResultPdf)
  , PageReloadResult (pageReloadResultResponse)
  , PageRequestGCResult
  , PageScreenshotParams (pageScreenshotParamsFullPage)
  , PageScreenshotResult (pageScreenshotResultBinary)
  , PageSetExtraHTTPHeadersResult
  , PageSetViewportSizeResult
  , mkBrowserContextCloseParams
  , mkBrowserContextUpdateSubscriptionParams
  , mkFrameAddScriptTagParams
  , mkFrameAddStyleTagParams
  , mkPageAddInitScriptParams
  , mkPageBringToFrontParams
  , mkPageCloseParams
  , mkPageGoBackParams
  , mkPageGoForwardParams
  , mkPagePdfParams
  , mkPageReloadParams
  , mkPageRequestGCParams
  , mkPageScreenshotParams
  , mkPageSetExtraHTTPHeadersParams
  , mkPageSetViewportSizeParams
  , mkPageSetViewportSizeParamsViewportSize
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
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
import Playwright.ScriptTag
  ( ScriptTag (scriptTagContent, scriptTagType, scriptTagUrl)
  )
import Playwright.StyleTag
  ( StyleTag (styleTagContent, styleTagUrl)
  )
import Playwright.PdfOptions
  ( PdfOptions (pdfOptionsOutline, pdfOptionsTagged)
  , defaultPdfOptions
  )
import Playwright.ScreenshotOptions
  ( ScreenshotOptions (screenshotOptionsFullPage, screenshotOptionsTimeout)
  )
import Playwright.Selector
  ( Selector
  , enterFrameSelector
  , nestedSelector
  )
import Playwright.SerializedArgument
  ( undefinedArgument
  )
import Playwright.Timeout
  ( Timeout
  )
import Playwright.Url
  ( Url
  )
import Playwright.UrlGlob
  ( UrlGlob
  , matchesUrlGlob
  )
import Playwright.ViewportSize
  ( ViewportSize (viewportHeight, viewportWidth)
  , viewportHeightToInt
  , viewportWidthToInt
  )
import Playwright.RaceTimeout
  ( waitWithTimeout
  )
import Playwright.TimeoutDefaults
  ( resolveActionTimeout
  , resolveNavigationTimeout
  )
import Playwright.WaitForState
  ( WaitForState
  )

data Page = Page
  { pageChannel :: Channel PageTag
  , pageMainFrame :: Frame
  , pageContextChannel :: Channel BrowserContextTag
  }

pageFromRef :: Channel BrowserContextTag -> ChannelRef -> IO Page
pageFromRef contextChannel ref = do
  let conn = channelConnection contextChannel
  obj <- lookupObject conn (channelRefGuid ref)
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  resolvedMainFrame <- frameFromRef conn (pageInitializerMainFrame initializer)
  let
    pageGuid = channelRefGuid ref
  registerMainFramePage conn (channelGuid (frameChannel resolvedMainFrame)) pageGuid
  pure
    Page
      { pageChannel = Channel conn pageGuid
      , pageMainFrame = resolvedMainFrame
      , pageContextChannel = contextChannel
      }

pageActionTimeout :: Page -> IO Timeout
pageActionTimeout page = do
  let conn = channelConnection (pageChannel page)
  pageDefaults <- lookupTimeoutDefaults conn (channelGuid (pageChannel page))
  contextDefaults <- lookupTimeoutDefaults conn (channelGuid (pageContextChannel page))
  pure (resolveActionTimeout pageDefaults contextDefaults)

pageRequest :: Page -> IO APIRequestContext
pageRequest page =
  apiRequestContextFor
    (channelConnection (pageChannel page))
    (channelGuid (pageContextChannel page))

pageNavigationTimeout :: Page -> IO Timeout
pageNavigationTimeout page = do
  let conn = channelConnection (pageChannel page)
  pageDefaults <- lookupTimeoutDefaults conn (channelGuid (pageChannel page))
  contextDefaults <- lookupTimeoutDefaults conn (channelGuid (pageContextChannel page))
  pure (resolveNavigationTimeout pageDefaults contextDefaults)

setDefaultTimeout :: Page -> Timeout -> IO ()
setDefaultTimeout page timeout =
  setTimeoutDefault
    (channelConnection (pageChannel page))
    (channelGuid (pageChannel page))
    (Just timeout)

setDefaultNavigationTimeout :: Page -> Timeout -> IO ()
setDefaultNavigationTimeout page timeout =
  setTimeoutNavigation
    (channelConnection (pageChannel page))
    (channelGuid (pageChannel page))
    (Just timeout)

closePage :: Page -> IO ()
closePage page = do
  let
    conn = channelConnection (pageChannel page)
  ownsContext <- pageOwnsContext conn (channelGuid (pageChannel page))
  if ownsContext
    then do
      result <-
        try (send (pageContextChannel page) (MethodName "close") mkBrowserContextCloseParams)
      case result of
        Left (ProtocolRpc failure)
          | protocolFailureName failure == "TargetClosedError" ->
              pure ()
        Left err ->
          throwIO err
        Right (_ :: BrowserContextCloseResult) ->
          pure ()
    else do
      result <- try (send (pageChannel page) (MethodName "close") mkPageCloseParams)
      case result of
        Left (ProtocolRpc failure)
          | protocolFailureName failure == "TargetClosedError" ->
              pure ()
        Left err ->
          throwIO err
        Right (_ :: PageCloseResult) ->
          pure ()

isClosed :: Page -> IO Bool
isClosed page = do
  closed <- closedGuids (channelConnection (pageChannel page))
  pure (Set.member (channelGuid (pageChannel page)) closed)

waitForClose :: Page -> IO ()
waitForClose page = do
  already <- isClosed page
  if already
    then pure ()
    else do
      _ <-
        waitForChannelEvent
          (channelConnection (pageChannel page))
          (channelGuid (pageChannel page))
          (MethodName "close")
      pure ()

bringToFront :: Page -> IO ()
bringToFront page = do
  _ :: PageBringToFrontResult <-
    send (pageChannel page) (MethodName "bringToFront") mkPageBringToFrontParams
  pure ()

requestGC :: Page -> IO ()
requestGC page = do
  _ :: PageRequestGCResult <-
    send (pageChannel page) (MethodName "requestGC") mkPageRequestGCParams
  pure ()

goto :: Page -> Url -> GotoOptions -> IO (Maybe Response)
goto page = gotoFrame (pageMainFrame page)

title :: Page -> IO Text
title page = titleFrame (pageMainFrame page)

url :: Page -> IO Url
url page = currentUrl (pageMainFrame page)

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

content :: Page -> IO Text
content page = contentFrame (pageMainFrame page)

setContent :: Page -> Text -> IO ()
setContent page html = do
  timeout <- pageNavigationTimeout page
  setContentFrame (pageMainFrame page) html timeout

setViewportSize :: Page -> ViewportSize -> IO ()
setViewportSize page size = do
  _ :: PageSetViewportSizeResult <-
    send
      (pageChannel page)
      (MethodName "setViewportSize")
      ( mkPageSetViewportSizeParams
          ( mkPageSetViewportSizeParamsViewportSize
              (viewportHeightToInt (viewportHeight size))
              (viewportWidthToInt (viewportWidth size))
          )
      )
  pure ()

screenshot :: Page -> ScreenshotOptions -> IO ByteString
screenshot page opts = do
  result <-
    send
      (pageChannel page)
      (MethodName "screenshot")
      (mkPageScreenshotParams (screenshotOptionsTimeout opts))
        { pageScreenshotParamsFullPage = screenshotOptionsFullPage opts
        }
  pure (protocolBinaryToByteString (pageScreenshotResultBinary result))

pdf :: Page -> IO ByteString
pdf page =
  pdfWith page defaultPdfOptions

pdfWith :: Page -> PdfOptions -> IO ByteString
pdfWith page opts = do
  result <-
    send
      (pageChannel page)
      (MethodName "pdf")
      mkPagePdfParams
        { pagePdfParamsOutline = pdfOptionsOutline opts
        , pagePdfParamsTagged = pdfOptionsTagged opts
        }
  pure (protocolBinaryToByteString (pagePdfResultPdf result))

locator :: Page -> Selector -> Locator
locator page selector =
  Locator
    { locatorFrame = pageMainFrame page
    , locatorSelector = selector
    , locatorPageChannel = pageChannel page
    , locatorContextChannel = pageContextChannel page
    , locatorMainFrame = pageMainFrame page
    }

locatorOnFrame :: Page -> Frame -> Selector -> Locator
locatorOnFrame page frame selector =
  Locator
    { locatorFrame = frame
    , locatorSelector = selector
    , locatorPageChannel = pageChannel page
    , locatorContextChannel = pageContextChannel page
    , locatorMainFrame = pageMainFrame page
    }

locatorPage :: Locator -> Page
locatorPage loc =
  Page
    { pageChannel = locatorPageChannel loc
    , pageMainFrame = locatorMainFrame loc
    , pageContextChannel = locatorContextChannel loc
    }

frameLocator :: Page -> Selector -> Locator
frameLocator page iframeSelector =
  locator page (nestedSelector iframeSelector enterFrameSelector)

getByTestId :: Page -> Text -> Locator
getByTestId page testId =
  locator page (getByTestIdSelector testId)

getByText :: Page -> Text -> Exactness -> Locator
getByText page text exactness =
  locator page (getByTextSelector text exactness)

getByLabel :: Page -> Text -> Exactness -> Locator
getByLabel page text exactness =
  locator page (getByLabelSelector text exactness)

getByRole :: Page -> AriaRole -> GetByRoleOptions -> Locator
getByRole page role opts =
  locator page (getByRoleSelector role opts)

goBack :: Page -> IO (Maybe Response)
goBack page = do
  timeout <- pageNavigationTimeout page
  result <- send (pageChannel page) (MethodName "goBack") (mkPageGoBackParams timeout)
  pure (fmap (responseFromRef (channelConnection (pageChannel page))) (pageGoBackResultResponse result))

goForward :: Page -> IO (Maybe Response)
goForward page = do
  timeout <- pageNavigationTimeout page
  result <- send (pageChannel page) (MethodName "goForward") (mkPageGoForwardParams timeout)
  pure (fmap (responseFromRef (channelConnection (pageChannel page))) (pageGoForwardResultResponse result))

reload :: Page -> IO (Maybe Response)
reload page = do
  timeout <- pageNavigationTimeout page
  result <- send (pageChannel page) (MethodName "reload") (mkPageReloadParams timeout)
  pure (fmap (responseFromRef (channelConnection (pageChannel page))) (pageReloadResultResponse result))

querySelector :: Page -> Selector -> IO (Maybe ElementHandle)
querySelector page selector =
  querySelectorFrame (pageMainFrame page) selector Nothing

querySelectorAll :: Page -> Selector -> IO [ElementHandle]
querySelectorAll page selector =
  querySelectorAllFrame (pageMainFrame page) selector

querySelectorStrict :: Page -> Selector -> IO (Maybe ElementHandle)
querySelectorStrict page selector =
  querySelectorFrame (pageMainFrame page) selector (Just True)

waitForSelectorStrict :: Page -> Selector -> WaitForState -> IO ()
waitForSelectorStrict page selector state = do
  timeout <- pageActionTimeout page
  waitForSelectorFrame (pageMainFrame page) selector state timeout (Just True)

addInitScript :: Page -> Text -> IO ()
addInitScript page source = do
  _ :: PageAddInitScriptResult <-
    send (pageChannel page) (MethodName "addInitScript") (mkPageAddInitScriptParams source)
  pure ()

addScriptTag :: Page -> ScriptTag -> IO ElementHandle
addScriptTag page tag = do
  result <-
    frameSend
      (pageMainFrame page)
      "addScriptTag"
      mkFrameAddScriptTagParams
        { frameAddScriptTagParamsContent = scriptTagContent tag
        , frameAddScriptTagParamsType = scriptTagType tag
        , frameAddScriptTagParamsUrl = scriptTagUrl tag
        }
  pure
    ( elementHandleFromRef
        (channelConnection (frameChannel (pageMainFrame page)))
        (frameAddScriptTagResultElement result)
    )

addStyleTag :: Page -> StyleTag -> IO ElementHandle
addStyleTag page tag = do
  result <-
    frameSend
      (pageMainFrame page)
      "addStyleTag"
      mkFrameAddStyleTagParams
        { frameAddStyleTagParamsContent = styleTagContent tag
        , frameAddStyleTagParamsUrl = styleTagUrl tag
        }
  pure
    ( elementHandleFromRef
        (channelConnection (frameChannel (pageMainFrame page)))
        (frameAddStyleTagResultElement result)
    )

setExtraHTTPHeaders :: Page -> [HttpHeader] -> IO ()
setExtraHTTPHeaders page headers = do
  _ :: PageSetExtraHTTPHeadersResult <-
    send
      (pageChannel page)
      (MethodName "setExtraHTTPHeaders")
      (mkPageSetExtraHTTPHeadersParams (fmap toNameValue headers))
  pure ()

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

mainFrame :: Page -> Frame
mainFrame =
  pageMainFrame

frames :: Page -> IO [Frame]
frames page =
  collect (pageMainFrame page)
  where
    collect current = do
      children <- childFrames current
      nested <- traverse collect children
      pure (current : concat nested)

frameByName :: Page -> Text -> IO (Maybe Frame)
frameByName page name = do
  allFrames <- frames page
  named <- traverse pairName allFrames
  pure (fmap fst (find (\(_, frameName) -> frameName == name) named))
  where
    pairName frame = do
      frameName <- currentName frame
      pure (frame, frameName)

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

subscribeContext :: Page -> BrowserContextUpdateSubscriptionParamsEvent -> IO ()
subscribeContext page event = do
  _ :: BrowserContextUpdateSubscriptionResult <-
    send
      (pageContextChannel page)
      (MethodName "updateSubscription")
      (mkBrowserContextUpdateSubscriptionParams True event)
  pure ()

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

opener :: Page -> IO (Maybe Page)
opener page = do
  let
    conn = channelConnection (pageChannel page)
  obj <- lookupObject conn (channelGuid (pageChannel page))
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  case pageInitializerOpener initializer of
    Nothing ->
      pure Nothing
    Just ref -> do
      closed <- closedGuids conn
      if Set.member (channelRefGuid ref) closed
        then pure Nothing
        else do
          loaded <- try (pageFromRef (pageContextChannel page) ref)
          case loaded of
            Left (MissingObject _) ->
              pure Nothing
            Left err ->
              throwIO err
            Right parent ->
              pure (Just parent)

pagesEqual :: Page -> Page -> Bool
pagesEqual left right =
  channelGuid (pageChannel left) == channelGuid (pageChannel right)
