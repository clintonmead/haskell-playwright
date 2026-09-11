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
  , setViewportSize
  ) where

import Control.Exception
  ( throwIO
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
  ( Channel (Channel, channelConnection)
  , PageTag
  , send
  )
import Playwright.Connection
  ( Connection
  , lookupObject
  )
import Playwright.Frame
  ( Frame
  , contentFrame
  , currentUrl
  , frameFromRef
  , gotoFrame
  , setContentFrame
  , titleFrame
  )
import Playwright.Frame.Wait
  ( waitForSelectorFrame
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
  ( Locator (Locator, locatorFrame, locatorSelector)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( PageCloseResult
  , PageGoBackResult (pageGoBackResultResponse)
  , PageGoForwardResult (pageGoForwardResultResponse)
  , PageInitializer (pageInitializerMainFrame)
  , PageReloadResult (pageReloadResultResponse)
  , PageScreenshotParams (pageScreenshotParamsFullPage)
  , PageScreenshotResult (pageScreenshotResultBinary)
  , PageSetViewportSizeResult
  , mkPageCloseParams
  , mkPageGoBackParams
  , mkPageGoForwardParams
  , mkPageReloadParams
  , mkPageScreenshotParams
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
import Playwright.ScreenshotOptions
  ( ScreenshotOptions (screenshotOptionsFullPage, screenshotOptionsTimeout)
  )
import Playwright.Selector
  ( Selector
  )
import Playwright.Timeout
  ( defaultTimeout
  )
import Playwright.Url
  ( Url
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
  }

pageFromRef :: Connection -> ChannelRef -> IO Page
pageFromRef conn ref = do
  obj <- lookupObject conn (channelRefGuid ref)
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  mainFrame <- frameFromRef conn (pageInitializerMainFrame initializer)
  pure
    Page
      { pageChannel = Channel conn (channelRefGuid ref)
      , pageMainFrame = mainFrame
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
