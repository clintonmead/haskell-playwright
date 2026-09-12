module Playwright.Browser
  ( Browser (..)
  , browserFromRef
  , closeBrowser
  , newContext
  , newContextWith
  , withPage
  , withPageWith
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
  , writeContextBaseUrl
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.NewContextOptions
  ( NewContextOptions (newContextBaseURL)
  , defaultNewContextOptions
  , toBrowserNewContextParams
  )
import Playwright.Page
  ( Page
  )
import Playwright.Protocol
  ( BrowserCloseResult
  , BrowserNewContextResult (browserNewContextResultContext)
  , mkBrowserCloseParams
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
newContext browser =
  newContextWith browser defaultNewContextOptions

newContextWith :: Browser -> NewContextOptions -> IO BrowserContext
newContextWith browser opts = do
  result <-
    send
      (browserChannel browser)
      (MethodName "newContext")
      (toBrowserNewContextParams opts)
  let
    conn = channelConnection (browserChannel browser)
    ref = browserNewContextResultContext result
  case newContextBaseURL opts of
    Nothing ->
      pure ()
    Just base ->
      writeContextBaseUrl conn (channelRefGuid ref) base
  contextFromRef conn ref

withPage :: Browser -> (Page -> IO a) -> IO a
withPage browser =
  withPageWith browser defaultNewContextOptions

withPageWith :: Browser -> NewContextOptions -> (Page -> IO a) -> IO a
withPageWith browser opts action =
  bracket
    ( do
        ctx <- newContextWith browser opts
        page <- newPage ctx
        pure (ctx, page)
    )
    (\(ctx, _) -> closeContext ctx)
    (\(_, page) -> action page)
