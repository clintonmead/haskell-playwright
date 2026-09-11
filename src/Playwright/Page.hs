module Playwright.Page
  ( Page (..)
  , pageFromRef
  , closePage
  , content
  , goto
  , locator
  , screenshot
  , setContent
  , title
  , url
  , waitForSelector
  , getByLabel
  , getByRole
  , getByTestId
  , getByText
  , goBack
  , goForward
  , reload
  , querySelector
  , querySelectorStrict
  , setViewportSize
  , frameByName
  , frames
  , mainFrame
  , addInitScript
  , addScriptTag
  , addStyleTag
  , setExtraHTTPHeaders
  , waitForFunction
  , waitForLoadState
  , waitForTimeout
  , waitForURL
  , waitForURLWith
  , armConsole
  , armDialog
  , waitForConsole
  , waitForDialog
  , waitForResponse
  , waitForResponseWith
  , waitForRequest
  , waitForRequestWith
  , waitForSelectorStrict
  , armPopup
  , opener
  , pagesEqual
  , waitForPopup
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Concurrent.Async
  ( race
  )
import Control.Concurrent.STM
  ( atomically
  , takeTMVar
  )
import Control.Exception
  ( throwIO
  )
import Data.List
  ( find
  )
import Data.ByteString
  ( ByteString
  )
import Data.Text
  ( Text
  )
import Playwright.AriaRole
  ( AriaRole
  )
import Playwright.Channel
  ( BrowserContextTag
  , Channel (Channel, channelConnection, channelGuid)
  , PageTag
  , send
  )
import Playwright.Connection
  ( lookupObject
  , registerEventWaiter
  , waitForChannelEvent
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
  ( querySelectorFrame
  )
import Playwright.Frame.Wait
  ( waitForFunctionFrame
  , waitForLoadStateFrame
  , waitForSelectorFrame
  , waitForTimeoutFrame
  )
import Playwright.ElementHandle
  ( ElementHandle
  , elementHandleFromRef
  )
import Playwright.Error
  ( PlaywrightError (WaitTimeout)
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
  ( HeaderName (headerNameToText)
  , HeaderValue (headerValueToText)
  , HttpHeader (httpHeaderName, httpHeaderValue)
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
  ( Locator (Locator, locatorFrame, locatorSelector)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( BrowserContextConsoleEvent
  , BrowserContextDialogEvent (browserContextDialogEventDialog)
  , BrowserContextPageEvent (browserContextPageEventPage)
  , BrowserContextRequestEvent (browserContextRequestEventRequest)
  , BrowserContextResponseEvent (browserContextResponseEventResponse)
  , BrowserContextUpdateSubscriptionParamsEvent (BrowserContextUpdateSubscriptionParamsEventConsole, BrowserContextUpdateSubscriptionParamsEventDialog, BrowserContextUpdateSubscriptionParamsEventRequest, BrowserContextUpdateSubscriptionParamsEventResponse)
  , BrowserContextUpdateSubscriptionResult
  , FrameAddScriptTagParams (frameAddScriptTagParamsContent, frameAddScriptTagParamsType, frameAddScriptTagParamsUrl)
  , FrameAddScriptTagResult (frameAddScriptTagResultElement)
  , FrameAddStyleTagParams (frameAddStyleTagParamsContent, frameAddStyleTagParamsUrl)
  , FrameAddStyleTagResult (frameAddStyleTagResultElement)
  , NameValue
  , PageAddInitScriptResult
  , PageCloseResult
  , PageGoBackResult (pageGoBackResultResponse)
  , PageGoForwardResult (pageGoForwardResultResponse)
  , PageInitializer (pageInitializerMainFrame, pageInitializerOpener)
  , PageReloadResult (pageReloadResultResponse)
  , PageScreenshotParams (pageScreenshotParamsFullPage)
  , PageScreenshotResult (pageScreenshotResultBinary)
  , PageSetExtraHTTPHeadersResult
  , PageSetViewportSizeResult
  , mkBrowserContextUpdateSubscriptionParams
  , mkFrameAddScriptTagParams
  , mkFrameAddStyleTagParams
  , mkNameValue
  , mkPageAddInitScriptParams
  , mkPageCloseParams
  , mkPageGoBackParams
  , mkPageGoForwardParams
  , mkPageReloadParams
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
import Playwright.ScreenshotOptions
  ( ScreenshotOptions (screenshotOptionsFullPage, screenshotOptionsTimeout)
  )
import Playwright.Selector
  ( Selector
  )
import Playwright.SerializedArgument
  ( undefinedArgument
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  , timeoutToDouble
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
  pure
    Page
      { pageChannel = Channel conn (channelRefGuid ref)
      , pageMainFrame = resolvedMainFrame
      , pageContextChannel = contextChannel
      }

closePage :: Page -> IO ()
closePage page = do
  _ :: PageCloseResult <- send (pageChannel page) (MethodName "close") mkPageCloseParams
  pure ()

goto :: Page -> Url -> GotoOptions -> IO (Maybe Response)
goto page = gotoFrame (pageMainFrame page)

title :: Page -> IO Text
title page = titleFrame (pageMainFrame page)

url :: Page -> IO Url
url page = currentUrl (pageMainFrame page)

waitForSelector :: Page -> Selector -> WaitForState -> IO ()
waitForSelector page selector state =
  waitForSelectorFrame (pageMainFrame page) selector state defaultTimeout Nothing

content :: Page -> IO Text
content page = contentFrame (pageMainFrame page)

setContent :: Page -> Text -> IO ()
setContent page html =
  setContentFrame (pageMainFrame page) html defaultTimeout

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

locator :: Page -> Selector -> Locator
locator page selector =
  Locator
    { locatorFrame = pageMainFrame page
    , locatorSelector = selector
    }

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
  result <- send (pageChannel page) (MethodName "goBack") (mkPageGoBackParams defaultTimeout)
  pure (fmap (responseFromRef (channelConnection (pageChannel page))) (pageGoBackResultResponse result))

goForward :: Page -> IO (Maybe Response)
goForward page = do
  result <- send (pageChannel page) (MethodName "goForward") (mkPageGoForwardParams defaultTimeout)
  pure (fmap (responseFromRef (channelConnection (pageChannel page))) (pageGoForwardResultResponse result))

reload :: Page -> IO (Maybe Response)
reload page = do
  result <- send (pageChannel page) (MethodName "reload") (mkPageReloadParams defaultTimeout)
  pure (fmap (responseFromRef (channelConnection (pageChannel page))) (pageReloadResultResponse result))

querySelector :: Page -> Selector -> IO (Maybe ElementHandle)
querySelector page selector =
  querySelectorFrame (pageMainFrame page) selector Nothing

querySelectorStrict :: Page -> Selector -> IO (Maybe ElementHandle)
querySelectorStrict page selector =
  querySelectorFrame (pageMainFrame page) selector (Just True)

waitForSelectorStrict :: Page -> Selector -> WaitForState -> IO ()
waitForSelectorStrict page selector state =
  waitForSelectorFrame (pageMainFrame page) selector state defaultTimeout (Just True)

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

toNameValue :: HttpHeader -> NameValue
toNameValue header =
  mkNameValue (headerNameToText (httpHeaderName header)) (headerValueToText (httpHeaderValue header))

waitForLoadState :: Page -> LoadState -> IO ()
waitForLoadState page =
  waitForLoadStateFrame (pageMainFrame page)

waitForTimeout :: Page -> Timeout -> IO ()
waitForTimeout page =
  waitForTimeoutFrame (pageMainFrame page)

waitForFunction :: Page -> Text -> IO JsHandle
waitForFunction page expression =
  waitForFunctionFrame (pageMainFrame page) expression undefinedArgument defaultTimeout

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

waitWithTimeout :: Timeout -> IO a -> IO a
waitWithTimeout timeout action = do
  result <- race (threadDelay (round (timeoutToDouble timeout * 1000))) action
  case result of
    Left () ->
      throwIO (WaitTimeout timeout)
    Right value ->
      pure value

waitForURL :: Page -> UrlGlob -> IO ()
waitForURL page glob =
  waitForURLWith page glob defaultTimeout

waitForURLWith :: Page -> UrlGlob -> Timeout -> IO ()
waitForURLWith page glob timeout =
  waitWithTimeout timeout $ do
    waitUntilUrl
      (channelConnection (pageChannel page))
      (channelGuid (frameChannel (pageMainFrame page)))
      (matchesUrlGlob glob)
    waitForLoadState page LoadStateLoad

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
waitForResponse page glob =
  waitForResponseWith page glob defaultTimeout

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
waitForRequest page glob =
  waitForRequestWith page glob defaultTimeout

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
  obj <- lookupObject (channelConnection (pageChannel page)) (channelGuid (pageChannel page))
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  case pageInitializerOpener initializer of
    Nothing ->
      pure Nothing
    Just ref ->
      Just <$> pageFromRef (pageContextChannel page) ref

pagesEqual :: Page -> Page -> Bool
pagesEqual left right =
  channelGuid (pageChannel left) == channelGuid (pageChannel right)
