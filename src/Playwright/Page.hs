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
  , setContentWith
  , title
  , url
  , getByAltText
  , getByAltTextRegex
  , getByLabel
  , getByPlaceholder
  , getByPlaceholderRegex
  , getByRole
  , getByTestId
  , getByTestIdAttr
  , getByTestIdAttrRegex
  , getByTestIdRegex
  , getByText
  , getByTitle
  , getByTitleRegex
  , goBack
  , goBackWith
  , goForward
  , goForwardWith
  , isClosed
  , reload
  , requestGC
  , querySelector
  , querySelectorAll
  , querySelectorStrict
  , setViewportSize
  , frameByName
  , frameByUrl
  , frameLocator
  , frames
  , mainFrame
  , addInitScript
  , addScriptTag
  , addStyleTag
  , setExtraHTTPHeaders
  , waitForClose
  , waitForCrash
  , onPageClose
  , onPageCrash
  , closePageWith
  , closePageReason
  , opener
  , pagesEqual
  , subscribeContext
  , pageActionTimeout
  , pageNavigationTimeout
  , pageRequest
  , setDefaultTimeout
  , setDefaultNavigationTimeout
  ) where

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
  ( try
  , throwIO
  )
import Control.Monad
  ( filterM
  )
import Data.Maybe
  ( listToMaybe
  )
import Data.List
  ( find
  )
import Data.Set qualified as Set
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as ByteString
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Data.Text.Encoding
  ( decodeUtf8
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
  , Channel (Channel, channelGuid)
  , PageTag
  , send
  )
import Playwright.Connection
  ( closedGuids
  , lookupObject
  , lookupTimeoutDefaults
  , pageOwnsContext
  , registerEventHandler
  , registerMainFramePage
  , setTimeoutDefault
  , setTimeoutNavigation
  , waitForChannelEvent
  )
import Playwright.SetContentOptions
  ( SetContentOptions (setContentOptionsTimeout, setContentOptionsWaitUntil)
  , defaultSetContentOptions
  )
import Playwright.Frame
  ( Frame (frameChannel)
  , childFrames
  , frameContent
  , currentName
  , currentUrl
  , frameFromRef
  , frameSend
  , gotoFrame
  , setContentFrameWith
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
  ( getByAltTextSelector
  , getByAltTextSelectorRegex
  , getByLabelSelector
  , getByPlaceholderSelector
  , getByPlaceholderSelectorRegex
  , getByRoleSelector
  , getByTestIdSelector
  , getByTestIdSelectorRegex
  , getByTextSelector
  , getByTitleSelector
  , getByTitleSelectorRegex
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
  ( throwDecode
  )
import Playwright.Json.ProtocolBinary
  ( protocolBinaryToByteString
  )
import Playwright.LoadState
  ( LoadState
  , toLifecycle
  )
import Playwright.Locator
  ( Locator (Locator, locatorContextChannel, locatorFrame, locatorMainFrame, locatorPageChannel, locatorSelector)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.RaceTimeout
  ( waitWithTimeout
  )
import Playwright.Protocol
  ( BrowserContextCloseParams (browserContextCloseParamsReason)
  , BrowserContextCloseResult
  , BrowserContextUpdateSubscriptionParamsEvent
  , BrowserContextUpdateSubscriptionResult
  , FrameAddScriptTagParams (frameAddScriptTagParamsContent, frameAddScriptTagParamsType, frameAddScriptTagParamsUrl)
  , FrameAddScriptTagResult (frameAddScriptTagResultElement)
  , FrameAddStyleTagParams (frameAddStyleTagParamsContent, frameAddStyleTagParamsUrl)
  , FrameAddStyleTagResult (frameAddStyleTagResultElement)
  , PageAddInitScriptResult
  , PageBringToFrontResult
  , PageCloseParams (pageCloseParamsReason)
  , PageCloseResult
  , PageRunBeforeUnloadResult
  , PageGoBackParams (pageGoBackParamsWaitUntil)
  , PageGoBackResult (pageGoBackResultResponse)
  , PageGoForwardParams (pageGoForwardParamsWaitUntil)
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
  , mkPageRunBeforeUnloadParams
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
  ( ScriptTag (scriptTagContent, scriptTagPath, scriptTagType, scriptTagUrl)
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
import Playwright.UrlMatch
  ( UrlMatch
  , matchesUrlMatch
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

pageFromRef :: Channel BrowserContextTag -> ChannelRef -> Playwright Page
pageFromRef contextChannel ref = do
  obj <- lookupObject (channelRefGuid ref)
  initializer <- throwDecode (remoteInitializer obj)
  resolvedMainFrame <- frameFromRef (pageInitializerMainFrame initializer)
  let
    pageGuid = channelRefGuid ref
  registerMainFramePage (channelGuid (frameChannel resolvedMainFrame)) pageGuid
  pure
    Page
      { pageChannel = Channel pageGuid
      , pageMainFrame = resolvedMainFrame
      , pageContextChannel = contextChannel
      }

pageActionTimeout :: Page -> Playwright Timeout
pageActionTimeout page = do
  pageDefaults <- lookupTimeoutDefaults (channelGuid (pageChannel page))
  contextDefaults <- lookupTimeoutDefaults (channelGuid (pageContextChannel page))
  pure (resolveActionTimeout pageDefaults contextDefaults)

pageRequest :: Page -> Playwright APIRequestContext
pageRequest page =
  apiRequestContextFor
    (channelGuid (pageContextChannel page))

pageNavigationTimeout :: Page -> Playwright Timeout
pageNavigationTimeout page = do
  pageDefaults <- lookupTimeoutDefaults (channelGuid (pageChannel page))
  contextDefaults <- lookupTimeoutDefaults (channelGuid (pageContextChannel page))
  pure (resolveNavigationTimeout pageDefaults contextDefaults)

setDefaultTimeout :: Page -> Timeout -> Playwright ()
setDefaultTimeout page timeout =
  setTimeoutDefault
    (channelGuid (pageChannel page))
    (Just timeout)

setDefaultNavigationTimeout :: Page -> Timeout -> Playwright ()
setDefaultNavigationTimeout page timeout =
  setTimeoutNavigation
    (channelGuid (pageChannel page))
    (Just timeout)

closePage :: Page -> Playwright ()
closePage page =
  closePageOpts page False Nothing

closePageWith :: Page -> Bool -> Playwright ()
closePageWith page runUnload =
  closePageOpts page runUnload Nothing

closePageReason :: Page -> Text -> Playwright ()
closePageReason page reason =
  closePageOpts page False (Just reason)

closePageOpts :: Page -> Bool -> Maybe Text -> Playwright ()
closePageOpts page runUnload reason = do
  ownsContext <- pageOwnsContext (channelGuid (pageChannel page))
  if ownsContext
    then
      ignoreTargetClosed $ do
        _ :: BrowserContextCloseResult <-
          send
            (pageContextChannel page)
            (MethodName "close")
            mkBrowserContextCloseParams {browserContextCloseParamsReason = reason}
        pure ()
    else
      if runUnload
        then do
          _ :: PageRunBeforeUnloadResult <-
            send (pageChannel page) (MethodName "runBeforeUnload") mkPageRunBeforeUnloadParams
          pure ()
        else
          ignoreTargetClosed $ do
            _ :: PageCloseResult <-
              send
                (pageChannel page)
                (MethodName "close")
                mkPageCloseParams {pageCloseParamsReason = reason}
            pure ()

isClosed :: Page -> Playwright Bool
isClosed page = do
  closed <- closedGuids
  pure (Set.member (channelGuid (pageChannel page)) closed)

waitForClose :: Page -> Playwright ()
waitForClose page = do
  already <- isClosed page
  if already
    then pure ()
    else do
      timeout <- pageActionTimeout page
      waitWithTimeout timeout $ do
        _ <-
          waitForChannelEvent
            (channelGuid (pageChannel page))
            (MethodName "close")
        pure ()

onPageClose :: Page -> Playwright () -> Playwright ()
onPageClose page run =
  registerEventHandler
    (channelGuid (pageChannel page))
    (MethodName "close")
    (\_ -> run)

waitForCrash :: Page -> Playwright ()
waitForCrash page = do
  timeout <- pageActionTimeout page
  waitWithTimeout timeout $ do
    _ <-
      waitForChannelEvent
        (channelGuid (pageChannel page))
        (MethodName "crash")
    pure ()

onPageCrash :: Page -> Playwright () -> Playwright ()
onPageCrash page run =
  registerEventHandler
    (channelGuid (pageChannel page))
    (MethodName "crash")
    (\_ -> run)

bringToFront :: Page -> Playwright ()
bringToFront page = do
  _ :: PageBringToFrontResult <-
    send (pageChannel page) (MethodName "bringToFront") mkPageBringToFrontParams
  pure ()

requestGC :: Page -> Playwright ()
requestGC page = do
  _ :: PageRequestGCResult <-
    send (pageChannel page) (MethodName "requestGC") mkPageRequestGCParams
  pure ()

goto :: Page -> Url -> GotoOptions -> Playwright (Maybe Response)
goto page = gotoFrame (pageMainFrame page)

title :: Page -> Playwright Text
title page = titleFrame (pageMainFrame page)

url :: Page -> Playwright Url
url page = currentUrl (pageMainFrame page)

content :: Page -> Playwright Text
content page = frameContent (pageMainFrame page)

setContent :: Page -> Text -> Playwright ()
setContent page html =
  setContentWith page html defaultSetContentOptions

setContentWith :: Page -> Text -> SetContentOptions -> Playwright ()
setContentWith page html opts = do
  timeout <- maybe (pageNavigationTimeout page) pure (setContentOptionsTimeout opts)
  setContentFrameWith (pageMainFrame page) html timeout (setContentOptionsWaitUntil opts)

setViewportSize :: Page -> ViewportSize -> Playwright ()
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

screenshot :: Page -> ScreenshotOptions -> Playwright ByteString
screenshot page opts = do
  result <-
    send
      (pageChannel page)
      (MethodName "screenshot")
      (mkPageScreenshotParams (screenshotOptionsTimeout opts))
        { pageScreenshotParamsFullPage = screenshotOptionsFullPage opts
        }
  pure (protocolBinaryToByteString (pageScreenshotResultBinary result))

pdf :: Page -> Playwright ByteString
pdf page =
  pdfWith page defaultPdfOptions

pdfWith :: Page -> PdfOptions -> Playwright ByteString
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
getByTestId page =
  getByTestIdAttr page "data-testid"

getByTestIdAttr :: Page -> Text -> Text -> Locator
getByTestIdAttr page attrName testId =
  locator page (getByTestIdSelector attrName testId)

getByTestIdRegex :: Page -> Text -> Locator
getByTestIdRegex page =
  getByTestIdAttrRegex page "data-testid"

getByTestIdAttrRegex :: Page -> Text -> Text -> Locator
getByTestIdAttrRegex page attrName source =
  locator page (getByTestIdSelectorRegex attrName source)

getByAltText :: Page -> Text -> Exactness -> Locator
getByAltText page text exactness =
  locator page (getByAltTextSelector text exactness)

getByAltTextRegex :: Page -> Text -> Locator
getByAltTextRegex page source =
  locator page (getByAltTextSelectorRegex source)

getByPlaceholder :: Page -> Text -> Exactness -> Locator
getByPlaceholder page text exactness =
  locator page (getByPlaceholderSelector text exactness)

getByPlaceholderRegex :: Page -> Text -> Locator
getByPlaceholderRegex page source =
  locator page (getByPlaceholderSelectorRegex source)

getByTitle :: Page -> Text -> Exactness -> Locator
getByTitle page text exactness =
  locator page (getByTitleSelector text exactness)

getByTitleRegex :: Page -> Text -> Locator
getByTitleRegex page source =
  locator page (getByTitleSelectorRegex source)

getByText :: Page -> Text -> Exactness -> Locator
getByText page text exactness =
  locator page (getByTextSelector text exactness)

getByLabel :: Page -> Text -> Exactness -> Locator
getByLabel page text exactness =
  locator page (getByLabelSelector text exactness)

getByRole :: Page -> AriaRole -> GetByRoleOptions -> Locator
getByRole page role opts =
  locator page (getByRoleSelector role opts)

goBack :: Page -> Playwright (Maybe Response)
goBack page =
  goBackWith page Nothing

goBackWith :: Page -> Maybe LoadState -> Playwright (Maybe Response)
goBackWith page waitUntil = do
  timeout <- pageNavigationTimeout page
  result <-
    send
      (pageChannel page)
      (MethodName "goBack")
      (mkPageGoBackParams timeout)
        { pageGoBackParamsWaitUntil = fmap toLifecycle waitUntil
        }
  pure (fmap (responseFromRef) (pageGoBackResultResponse result))

goForward :: Page -> Playwright (Maybe Response)
goForward page =
  goForwardWith page Nothing

goForwardWith :: Page -> Maybe LoadState -> Playwright (Maybe Response)
goForwardWith page waitUntil = do
  timeout <- pageNavigationTimeout page
  result <-
    send
      (pageChannel page)
      (MethodName "goForward")
      (mkPageGoForwardParams timeout)
        { pageGoForwardParamsWaitUntil = fmap toLifecycle waitUntil
        }
  pure (fmap (responseFromRef) (pageGoForwardResultResponse result))

reload :: Page -> Playwright (Maybe Response)
reload page = do
  timeout <- pageNavigationTimeout page
  result <- send (pageChannel page) (MethodName "reload") (mkPageReloadParams timeout)
  pure (fmap (responseFromRef) (pageReloadResultResponse result))

querySelector :: Page -> Selector -> Playwright (Maybe ElementHandle)
querySelector page selector =
  querySelectorFrame (pageMainFrame page) selector Nothing

querySelectorAll :: Page -> Selector -> Playwright [ElementHandle]
querySelectorAll page selector =
  querySelectorAllFrame (pageMainFrame page) selector

querySelectorStrict :: Page -> Selector -> Playwright (Maybe ElementHandle)
querySelectorStrict page selector =
  querySelectorFrame (pageMainFrame page) selector (Just True)

addInitScript :: Page -> Text -> Playwright ()
addInitScript page source = do
  _ :: PageAddInitScriptResult <-
    send (pageChannel page) (MethodName "addInitScript") (mkPageAddInitScriptParams source)
  pure ()

addScriptTag :: Page -> ScriptTag -> Playwright ElementHandle
addScriptTag page tag = do
  scriptContent <- scriptTagSource tag
  result <-
    frameSend
      (pageMainFrame page)
      "addScriptTag"
      mkFrameAddScriptTagParams
        { frameAddScriptTagParamsContent = scriptContent
        , frameAddScriptTagParamsType = scriptTagType tag
        , frameAddScriptTagParamsUrl = scriptTagUrl tag
        }
  pure
    ( elementHandleFromRef
        (frameAddScriptTagResultElement result)
    )

-- | The script text to send. A path is read from disk and marked with a
-- sourceURL, so stack traces name the file, as the JS client does.
scriptTagSource :: ScriptTag -> Playwright (Maybe Text)
scriptTagSource tag =
  case scriptTagPath tag of
    Nothing ->
      pure (scriptTagContent tag)
    Just path -> do
      bytes <- liftIO (ByteString.readFile path)
      pure (Just (decodeUtf8 bytes <> "\n//# sourceURL=" <> Text.pack (filter (/= '\n') path)))

addStyleTag :: Page -> StyleTag -> Playwright ElementHandle
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
        (frameAddStyleTagResultElement result)
    )

setExtraHTTPHeaders :: Page -> [HttpHeader] -> Playwright ()
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

frames :: Page -> Playwright [Frame]
frames page =
  collect (pageMainFrame page)
  where
    collect current = do
      children <- childFrames current
      nested <- traverse collect children
      pure (current : concat nested)

frameByName :: Page -> Text -> Playwright (Maybe Frame)
frameByName page name = do
  allFrames <- frames page
  named <- traverse pairName allFrames
  pure (fmap fst (find (\(_, frameName) -> frameName == name) named))
  where
    pairName frame = do
      frameName <- currentName frame
      pure (frame, frameName)


-- | The first frame whose URL matches, searching the main frame and its children.
frameByUrl :: Page -> UrlMatch -> Playwright (Maybe Frame)
frameByUrl page match = do
  allFrames <- frames page
  matching <- filterM (\frame -> matchesUrlMatch match <$> currentUrl frame) allFrames
  pure (listToMaybe matching)

subscribeContext :: Page -> BrowserContextUpdateSubscriptionParamsEvent -> Playwright ()
subscribeContext page event = do
  _ :: BrowserContextUpdateSubscriptionResult <-
    send
      (pageContextChannel page)
      (MethodName "updateSubscription")
      (mkBrowserContextUpdateSubscriptionParams True event)
  pure ()


opener :: Page -> Playwright (Maybe Page)
opener page = do
  obj <- lookupObject (channelGuid (pageChannel page))
  initializer <- throwDecode (remoteInitializer obj)
  case pageInitializerOpener initializer of
    Nothing ->
      pure Nothing
    Just ref -> do
      closed <- closedGuids
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
