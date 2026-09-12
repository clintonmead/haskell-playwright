module Playwright.BrowserContext
  ( BrowserContext (..)
  , addContextInitScript
  , addCookies
  , clearCookies
  , clearCookiesNamed
  , clearGeolocation
  , clearPermissions
  , closeContext
  , contextFromRef
  , contextPages
  , cookies
  , cookiesFor
  , contextsEqual
  , grantPermissions
  , grantPermissionsFor
  , newPage
  , pageContext
  , setContextExtraHTTPHeaders
  , setGeolocation
  , setHTTPCredentials
  , setOffline
  , setStorageState
  , storageState
  , waitForContextPage
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Set qualified as Set
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( BrowserContextTag
  , Channel (Channel, channelConnection, channelGuid)
  , send
  )
import Playwright.Connection
  ( Connection
  , closedGuids
  , guidsOfType
  , lookupObject
  , waitForFutureChannelEvent
  )
import Playwright.Cookie
  ( Cookie
  , CookieName (cookieNameToText)
  , CookieToSet
  , fromNetworkCookie
  , toSetNetworkCookie
  )
import Playwright.Disposable
  ( Disposable
  , disposableFromRef
  )
import Playwright.Geolocation
  ( Accuracy (accuracyToScientific)
  , Geolocation (geolocationAccuracy, geolocationLatitude, geolocationLongitude)
  , Latitude (latitudeToScientific)
  , Longitude (longitudeToScientific)
  )
import Playwright.HttpCredentials
  ( HttpCredentials (httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsUsername)
  , HttpPassword (httpPasswordToText)
  , HttpUsername (httpUsernameToText)
  )
import Playwright.HttpHeader
  ( HttpHeader
  , toNameValue
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
  )
import Playwright.Json.Decode
  ( fromJsonValue
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
  , BrowserContextAddInitScriptResult (browserContextAddInitScriptResultDisposable)
  , BrowserContextClearCookiesParams (browserContextClearCookiesParamsName)
  , BrowserContextClearCookiesResult
  , BrowserContextClearPermissionsResult
  , BrowserContextCloseResult
  , BrowserContextCookiesResult (browserContextCookiesResultCookies)
  , BrowserContextGrantPermissionsParams (browserContextGrantPermissionsParamsOrigin)
  , BrowserContextGrantPermissionsResult
  , BrowserContextNewPageResult (browserContextNewPageResultPage)
  , BrowserContextPageEvent (browserContextPageEventPage)
  , BrowserContextSetExtraHTTPHeadersResult
  , ChannelType (ChannelTypePage)
  , BrowserContextSetGeolocationParams (browserContextSetGeolocationParamsGeolocation)
  , BrowserContextSetGeolocationParamsGeolocation (browserContextSetGeolocationParamsGeolocationAccuracy)
  , BrowserContextSetGeolocationResult
  , BrowserContextSetHTTPCredentialsParams (browserContextSetHTTPCredentialsParamsHttpCredentials)
  , BrowserContextSetHTTPCredentialsParamsHttpCredentials (browserContextSetHTTPCredentialsParamsHttpCredentialsOrigin)
  , BrowserContextSetHTTPCredentialsResult
  , BrowserContextSetOfflineResult
  , BrowserContextSetStorageStateParams (browserContextSetStorageStateParamsStorageState)
  , BrowserContextSetStorageStateResult
  , BrowserContextStorageStateResult
  , mkBrowserContextAddCookiesParams
  , mkBrowserContextAddInitScriptParams
  , mkBrowserContextClearCookiesParams
  , mkBrowserContextClearPermissionsParams
  , mkBrowserContextCloseParams
  , mkBrowserContextCookiesParams
  , mkBrowserContextGrantPermissionsParams
  , mkBrowserContextNewPageParams
  , mkBrowserContextSetExtraHTTPHeadersParams
  , mkBrowserContextSetGeolocationParams
  , mkBrowserContextSetGeolocationParamsGeolocation
  , mkBrowserContextSetHTTPCredentialsParams
  , mkBrowserContextSetHTTPCredentialsParamsHttpCredentials
  , mkBrowserContextSetOfflineParams
  , mkBrowserContextSetStorageStateParams
  , mkBrowserContextStorageStateParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteGuid, remoteParent, remoteType)
  )
import Playwright.StorageState
  ( StorageState
  , fromStorageStateResult
  , toSetStorageState
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

addContextInitScript :: BrowserContext -> Text -> IO Disposable
addContextInitScript ctx source = do
  result <-
    send
      (browserContextChannel ctx)
      (MethodName "addInitScript")
      (mkBrowserContextAddInitScriptParams source)
  pure
    ( disposableFromRef
        (channelConnection (browserContextChannel ctx))
        (browserContextAddInitScriptResultDisposable result)
    )

closeContext :: BrowserContext -> IO ()
closeContext ctx = do
  _ :: BrowserContextCloseResult <-
    send (browserContextChannel ctx) (MethodName "close") mkBrowserContextCloseParams
  pure ()

pageContext :: Page -> BrowserContext
pageContext page =
  BrowserContext (pageContextChannel page)

contextsEqual :: BrowserContext -> BrowserContext -> Bool
contextsEqual left right =
  channelGuid (browserContextChannel left) == channelGuid (browserContextChannel right)

contextPages :: BrowserContext -> IO [Page]
contextPages ctx = do
  let
    conn = channelConnection (browserContextChannel ctx)
    parent = channelGuid (browserContextChannel ctx)
  closed <- closedGuids conn
  objects <- traverse (lookupObject conn) =<< guidsOfType conn ChannelTypePage
  traverse
    (pageFromRef (browserContextChannel ctx) . ChannelRef . remoteGuid)
    ( filter
        ( \obj ->
            remoteType obj == ChannelTypePage
              && remoteParent obj == parent
              && Set.notMember (remoteGuid obj) closed
        )
        objects
    )

waitForContextPage :: BrowserContext -> IO Page
waitForContextPage ctx = do
  params <-
    waitForFutureChannelEvent
      (channelConnection (browserContextChannel ctx))
      (channelGuid (browserContextChannel ctx))
      (MethodName "page")
  event <- either throwIO pure (fromJsonValue params)
  pageFromRef (browserContextChannel ctx) (browserContextPageEventPage event)

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
              ( ( mkBrowserContextSetGeolocationParamsGeolocation
                    (latitudeToScientific (geolocationLatitude location))
                    (longitudeToScientific (geolocationLongitude location))
                )
                  { browserContextSetGeolocationParamsGeolocationAccuracy =
                      fmap accuracyToScientific (geolocationAccuracy location)
                  }
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

setContextExtraHTTPHeaders :: BrowserContext -> [HttpHeader] -> IO ()
setContextExtraHTTPHeaders ctx headers = do
  _ :: BrowserContextSetExtraHTTPHeadersResult <-
    send
      (browserContextChannel ctx)
      (MethodName "setExtraHTTPHeaders")
      (mkBrowserContextSetExtraHTTPHeadersParams (fmap toNameValue headers))
  pure ()

setHTTPCredentials :: BrowserContext -> HttpCredentials -> IO ()
setHTTPCredentials ctx creds = do
  _ :: BrowserContextSetHTTPCredentialsResult <-
    send
      (browserContextChannel ctx)
      (MethodName "setHTTPCredentials")
      mkBrowserContextSetHTTPCredentialsParams
        { browserContextSetHTTPCredentialsParamsHttpCredentials =
            Just
              ( ( mkBrowserContextSetHTTPCredentialsParamsHttpCredentials
                    (httpPasswordToText (httpCredentialsPassword creds))
                    (httpUsernameToText (httpCredentialsUsername creds))
                )
                  { browserContextSetHTTPCredentialsParamsHttpCredentialsOrigin =
                      fmap urlToText (httpCredentialsOrigin creds)
                  }
              )
        }
  pure ()

storageState :: BrowserContext -> IO StorageState
storageState ctx = do
  result :: BrowserContextStorageStateResult <-
    send
      (browserContextChannel ctx)
      (MethodName "storageState")
      mkBrowserContextStorageStateParams
  pure (fromStorageStateResult result)

setStorageState :: BrowserContext -> StorageState -> IO ()
setStorageState ctx state = do
  _ :: BrowserContextSetStorageStateResult <-
    send
      (browserContextChannel ctx)
      (MethodName "setStorageState")
      mkBrowserContextSetStorageStateParams
        { browserContextSetStorageStateParamsStorageState = Just (toSetStorageState state)
        }
  pure ()
