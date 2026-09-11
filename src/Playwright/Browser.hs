module Playwright.Browser
  ( Browser (..)
  , browserFromRef
  , closeBrowser
  , newContext
  , withPage
  ) where

import Control.Exception
  ( bracket
  )
import Playwright.BrowserContext
  ( BrowserContext
  , closeContext
  , contextFromRef
  , newPage
  )
import Playwright.Channel
  ( BrowserTag
  , Channel (Channel, channelConnection)
  , send
  )
import Playwright.Connection
  ( Connection
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page
  )
import Playwright.Protocol
  ( BrowserCloseResult
  , BrowserNewContextResult (browserNewContextResultContext)
  , mkBrowserCloseParams
  , mkBrowserNewContextParams
  )

newtype Browser = Browser
  { browserChannel :: Channel BrowserTag
  }

browserFromRef :: Connection -> ChannelRef -> IO Browser
browserFromRef conn ref =
  pure (Browser (Channel conn (channelRefGuid ref)))

closeBrowser :: Browser -> IO ()
closeBrowser browser = do
  _ :: BrowserCloseResult <-
    send (browserChannel browser) (MethodName "close") mkBrowserCloseParams
  pure ()

newContext :: Browser -> IO BrowserContext
newContext browser = do
  result <-
    send
      (browserChannel browser)
      (MethodName "newContext")
      mkBrowserNewContextParams
  contextFromRef
    (channelConnection (browserChannel browser))
    (browserNewContextResultContext result)

withPage :: Browser -> (Page -> IO a) -> IO a
withPage browser action =
  bracket
    ( do
        ctx <- newContext browser
        page <- newPage ctx
        pure (ctx, page)
    )
    (\(ctx, _) -> closeContext ctx)
    (\(_, page) -> action page)
