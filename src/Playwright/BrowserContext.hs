module Playwright.BrowserContext
  ( BrowserContext (..)
  , addCookies
  , clearCookies
  , clearCookiesNamed
  , contextFromRef
  , cookies
  , cookiesFor
  , newPage
  , closeContext
  , pageContext
  ) where

import Playwright.Channel
  ( BrowserContextTag
  , Channel (Channel)
  , send
  )
import Playwright.Connection
  ( Connection
  )
import Playwright.Cookie
  ( Cookie
  , CookieName (cookieNameToText)
  , CookieToSet
  , fromNetworkCookie
  , toSetNetworkCookie
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageContextChannel)
  , pageFromRef
  )
import Playwright.Protocol
  ( BrowserContextAddCookiesResult
  , BrowserContextClearCookiesParams (browserContextClearCookiesParamsName)
  , BrowserContextClearCookiesResult
  , BrowserContextCloseResult
  , BrowserContextCookiesResult (browserContextCookiesResultCookies)
  , BrowserContextNewPageResult (browserContextNewPageResultPage)
  , mkBrowserContextAddCookiesParams
  , mkBrowserContextClearCookiesParams
  , mkBrowserContextCloseParams
  , mkBrowserContextCookiesParams
  , mkBrowserContextNewPageParams
  )
import Playwright.Url
  ( Url
  , urlToText
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

pageContext :: Page -> BrowserContext
pageContext page =
  BrowserContext (pageContextChannel page)

addCookies :: BrowserContext -> [CookieToSet] -> IO ()
addCookies ctx cookiesToSet = do
  _ :: BrowserContextAddCookiesResult <-
    send
      (browserContextChannel ctx)
      (MethodName "addCookies")
      (mkBrowserContextAddCookiesParams (fmap toSetNetworkCookie cookiesToSet))
  pure ()

cookies :: BrowserContext -> IO [Cookie]
cookies ctx =
  cookiesFor ctx []

cookiesFor :: BrowserContext -> [Url] -> IO [Cookie]
cookiesFor ctx urls = do
  result <-
    send
      (browserContextChannel ctx)
      (MethodName "cookies")
      (mkBrowserContextCookiesParams (fmap urlToText urls))
  pure (fmap fromNetworkCookie (browserContextCookiesResultCookies result))

clearCookies :: BrowserContext -> IO ()
clearCookies ctx = do
  _ :: BrowserContextClearCookiesResult <-
    send
      (browserContextChannel ctx)
      (MethodName "clearCookies")
      mkBrowserContextClearCookiesParams
  pure ()

clearCookiesNamed :: BrowserContext -> CookieName -> IO ()
clearCookiesNamed ctx name = do
  _ :: BrowserContextClearCookiesResult <-
    send
      (browserContextChannel ctx)
      (MethodName "clearCookies")
      mkBrowserContextClearCookiesParams
        { browserContextClearCookiesParamsName = Just (cookieNameToText name)
        }
  pure ()
