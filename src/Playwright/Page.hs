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
  , waitForClose
  , opener
  , pagesEqual
  , subscribeContext
  , pageActionTimeout
  , pageNavigationTimeout
  , pageRequest
  , setDefaultTimeout
  , setDefaultNavigationTimeout
  ) where

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
  , registerMainFramePage
  , setTimeoutDefault
  , setTimeoutNavigation
  , waitForChannelEvent
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
import Playwright.ElementHandle
  ( ElementHandle
  , elementHandleFromRef
  )
import Playwright.Error
  ( PlaywrightError (MissingObject)
  , ignoreTargetClosed
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
import Playwright.Locator
  ( Locator (Locator, locatorContextChannel, locatorFrame, locatorMainFrame, locatorPageChannel, locatorSelector)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( BrowserContextCloseResult
  , BrowserContextUpdateSubscriptionParamsEvent
  , BrowserContextUpdateSubscriptionResult
  , FrameAddScriptTagParams (frameAddScriptTagParamsContent, frameAddScriptTagParamsType, frameAddScriptTagParamsUrl)
  , FrameAddScriptTagResult (frameAddScriptTagResultElement)
  , FrameAddStyleTagParams (frameAddStyleTagParamsContent, frameAddStyleTagParamsUrl)
  , FrameAddStyleTagResult (frameAddStyleTagResultElement)
  , PageAddInitScriptResult
  , PageBringToFrontResult
  , PageCloseResult
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
import Playwright.Response
  ( Response
  , responseFromRef
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
import Playwright.Timeout
  ( Timeout
  )
import Playwright.Url
  ( Url
  )
import Playwright.ViewportSize
  ( ViewportSize (viewportHeight, viewportWidth)
  , viewportHeightToInt
  , viewportWidthToInt
  )
import Playwright.TimeoutDefaults
  ( resolveActionTimeout
  , resolveNavigationTimeout
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
    then
      ignoreTargetClosed $ do
        _ :: BrowserContextCloseResult <-
          send (pageContextChannel page) (MethodName "close") mkBrowserContextCloseParams
        pure ()
    else
      ignoreTargetClosed $ do
        _ :: PageCloseResult <-
          send (pageChannel page) (MethodName "close") mkPageCloseParams
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


subscribeContext :: Page -> BrowserContextUpdateSubscriptionParamsEvent -> IO ()
subscribeContext page event = do
  _ :: BrowserContextUpdateSubscriptionResult <-
    send
      (pageContextChannel page)
      (MethodName "updateSubscription")
      (mkBrowserContextUpdateSubscriptionParams True event)
  pure ()


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
