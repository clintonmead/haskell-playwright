module Playwright.BrowserContext
  ( BrowserContext (..)
  , addCookies
  , clearCookies
  , clearCookiesNamed
  , clearGeolocation
  , clearPermissions
  , closeContext
  , contextFromRef
  , cookies
  , cookiesFor
  , grantPermissions
  , grantPermissionsFor
  , newPage
  , pageContext
  , setGeolocation
  , setOffline
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
import Playwright.Geolocation
  ( Geolocation (geolocationLatitude, geolocationLongitude)
  , Latitude (latitudeToScientific)
  , Longitude (longitudeToScientific)
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
import Playwright.PermissionName
  ( PermissionName (permissionNameToText)
  )
import Playwright.Protocol
  ( BrowserContextAddCookiesResult
  , BrowserContextClearCookiesParams (browserContextClearCookiesParamsName)
  , BrowserContextClearCookiesResult
  , BrowserContextClearPermissionsResult
  , BrowserContextCloseResult
  , BrowserContextCookiesResult (browserContextCookiesResultCookies)
  , BrowserContextGrantPermissionsParams (browserContextGrantPermissionsParamsOrigin)
  , BrowserContextGrantPermissionsResult
  , BrowserContextNewPageResult (browserContextNewPageResultPage)
  , BrowserContextSetGeolocationParams (browserContextSetGeolocationParamsGeolocation)
  , BrowserContextSetGeolocationResult
  , BrowserContextSetOfflineResult
  , mkBrowserContextAddCookiesParams
  , mkBrowserContextClearCookiesParams
  , mkBrowserContextClearPermissionsParams
  , mkBrowserContextCloseParams
  , mkBrowserContextCookiesParams
  , mkBrowserContextGrantPermissionsParams
  , mkBrowserContextNewPageParams
  , mkBrowserContextSetGeolocationParams
  , mkBrowserContextSetGeolocationParamsGeolocation
  , mkBrowserContextSetOfflineParams
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

grantPermissions :: BrowserContext -> [PermissionName] -> IO ()
grantPermissions ctx names =
  grantPermissionsForOrigin ctx names Nothing

grantPermissionsFor :: BrowserContext -> [PermissionName] -> Url -> IO ()
grantPermissionsFor ctx names origin =
  grantPermissionsForOrigin ctx names (Just origin)

grantPermissionsForOrigin :: BrowserContext -> [PermissionName] -> Maybe Url -> IO ()
grantPermissionsForOrigin ctx names origin = do
  _ :: BrowserContextGrantPermissionsResult <-
    send
      (browserContextChannel ctx)
      (MethodName "grantPermissions")
      (mkBrowserContextGrantPermissionsParams (fmap permissionNameToText names))
        { browserContextGrantPermissionsParamsOrigin = fmap urlToText origin
        }
  pure ()

clearPermissions :: BrowserContext -> IO ()
clearPermissions ctx = do
  _ :: BrowserContextClearPermissionsResult <-
    send (browserContextChannel ctx) (MethodName "clearPermissions") mkBrowserContextClearPermissionsParams
  pure ()

setGeolocation :: BrowserContext -> Geolocation -> IO ()
setGeolocation ctx location = do
  _ :: BrowserContextSetGeolocationResult <-
    send
      (browserContextChannel ctx)
      (MethodName "setGeolocation")
      mkBrowserContextSetGeolocationParams
        { browserContextSetGeolocationParamsGeolocation =
            Just
              ( mkBrowserContextSetGeolocationParamsGeolocation
                  (latitudeToScientific (geolocationLatitude location))
                  (longitudeToScientific (geolocationLongitude location))
              )
        }
  pure ()

clearGeolocation :: BrowserContext -> IO ()
clearGeolocation ctx = do
  _ :: BrowserContextSetGeolocationResult <-
    send
      (browserContextChannel ctx)
      (MethodName "setGeolocation")
      mkBrowserContextSetGeolocationParams
  pure ()

setOffline :: BrowserContext -> Bool -> IO ()
setOffline ctx offline = do
  _ :: BrowserContextSetOfflineResult <-
    send
      (browserContextChannel ctx)
      (MethodName "setOffline")
      (mkBrowserContextSetOfflineParams offline)
  pure ()
