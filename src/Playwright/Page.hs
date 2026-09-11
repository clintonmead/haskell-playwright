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
import Playwright.Channel
  ( Channel (Channel)
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
  , PageInitializer (pageInitializerMainFrame)
  , PageScreenshotParams (pageScreenshotParamsFullPage)
  , PageScreenshotResult (pageScreenshotResultBinary)
  , mkPageCloseParams
  , mkPageScreenshotParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.Response
  ( Response
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
