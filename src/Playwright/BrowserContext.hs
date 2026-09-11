module Playwright.BrowserContext
  ( BrowserContext (..)
  , contextFromRef
  , newPage
  , closeContext
  ) where

import Playwright.Channel
  ( BrowserContextTag
  , Channel (Channel)
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
  , pageFromRef
  )
import Playwright.Protocol
  ( BrowserContextCloseResult
  , BrowserContextNewPageResult (browserContextNewPageResultPage)
  , mkBrowserContextCloseParams
  , mkBrowserContextNewPageParams
  )

newtype BrowserContext = BrowserContext
  { browserContextChannel :: Channel BrowserContextTag
  }

contextFromRef :: Connection -> ChannelRef -> IO BrowserContext
contextFromRef conn ref =
  pure (BrowserContext (Channel conn (channelRefGuid ref)))

newPage :: BrowserContext -> IO Page
newPage ctx = do
  result <-
    send
      (browserContextChannel ctx)
      (MethodName "newPage")
      mkBrowserContextNewPageParams
  pageFromRef
    (browserContextChannel ctx)
    (browserContextNewPageResultPage result)

closeContext :: BrowserContext -> IO ()
closeContext ctx = do
  _ :: BrowserContextCloseResult <-
    send (browserContextChannel ctx) (MethodName "close") mkBrowserContextCloseParams
  pure ()
