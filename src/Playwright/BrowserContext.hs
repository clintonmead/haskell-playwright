module Playwright.BrowserContext
  ( BrowserContext (..)
  , addContextInitScript
  , addCookies
  , clearCookies
  , clearCookiesNamed
  , clearCookiesWith
  , clearGeolocation
  , clearPermissions
  , closeContext
  , closeContextWith
  , contextFromRef
  , startContextRecordHar
  , contextPages
  , cookies
  , cookiesFor
  , contextsEqual
  , contextRequest
  , isContextClosed
  , onContextClose
  , onContextPage
  , grantPermissions
  , grantPermissionsFor
  , newPage
  , pageContext
  , setContextExtraHTTPHeaders
  , setContextDefaultTimeout
  , setContextDefaultNavigationTimeout
  , setGeolocation
  , setHTTPCredentials
  , setHTTPCredentialsList
  , setOffline
  , setStorageState
  , storageState
  , waitForContextClose
  , armContextPage
  , waitForContextPage
  ) where

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Set qualified as Set
import Data.Text
  ( Text
  )
import Playwright.APIRequestContext
  ( APIRequestContext
  , apiRequestContextFor
  )
import Playwright.Channel
  ( BrowserContextTag
  , Channel (Channel, channelGuid)
  , send
  )
import Playwright.Connection
  ( closedGuids
  , contextHasOwnerPage
  , guidsOfType
  , lookupObject
  , registerEventHandler
  , registerFutureEventWaiter
  , setTimeoutDefault
  , setTimeoutNavigation
  , waitForChannelEvent
  , waitForFutureChannelEventUnlessClosed
  , waitForWaiterUnlessClosed
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  , ignoreTargetClosed
  )
import Playwright.ClearCookiesOptions
  ( ClearCookiesOptions (clearCookiesDomain, clearCookiesName, clearCookiesNameRegex, clearCookiesPath)
  , defaultClearCookiesOptions
  )
import Playwright.Cookie
  ( Cookie
  , CookieDomain (cookieDomainToText)
  , CookieName (cookieNameToText)
  , CookiePath (cookiePathToText)
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
  ( HttpCredentials
  )
import Playwright.HttpHeader
  ( HttpHeader
  , toNameValue
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
  )
import Playwright.Json.Decode
  ( throwDecode
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
  ( BrowserContextInitializer (browserContextInitializerTracing)
  )
import Playwright.Protocol
  ( BrowserContextAddCookiesResult
  , BrowserContextAddInitScriptResult (browserContextAddInitScriptResultDisposable)
  , BrowserContextClearCookiesParams (browserContextClearCookiesParamsDomain, browserContextClearCookiesParamsName, browserContextClearCookiesParamsNameRegexFlags, browserContextClearCookiesParamsNameRegexSource, browserContextClearCookiesParamsPath)
  , BrowserContextClearCookiesResult
  , BrowserContextClearPermissionsResult
  , BrowserContextCloseParams (browserContextCloseParamsReason)
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
  , mkBrowserContextSetOfflineParams
  , mkBrowserContextSetStorageStateParams
  , mkBrowserContextStorageStateParams
  )
import Playwright.RaceTimeout
  ( waitWithTimeout
  )
import Playwright.RecordHar
  ( RecordHar
  , exportRecordHar
  , startRecordHar
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteGuid, remoteInitializer, remoteParent, remoteType)
  )
import Playwright.StorageState
  ( StorageState
  , fromStorageStateResult
  , toSetStorageState
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  )
import Playwright.NewContextOptions
  ( toProtocolCredentials
  )
import Playwright.Url
  ( Url
  , urlToText
  )

newtype BrowserContext = BrowserContext
  { browserContextChannel :: Channel BrowserContextTag
  }

contextFromRef :: ChannelRef -> Playwright BrowserContext
contextFromRef ref =
  pure (BrowserContext (Channel (channelRefGuid ref)))

newPage :: BrowserContext -> Playwright Page
newPage ctx = do
  owned <-
    contextHasOwnerPage
      (channelGuid (browserContextChannel ctx))
  when owned $
    throwIO (DecodeFailure "Please use browser.newContext()")
  result <-
    send
      (browserContextChannel ctx)
      (MethodName "newPage")
      mkBrowserContextNewPageParams
  pageFromRef
    (browserContextChannel ctx)
    (browserContextNewPageResultPage result)

addContextInitScript :: BrowserContext -> Text -> Playwright Disposable
addContextInitScript ctx source = do
  result <-
    send
      (browserContextChannel ctx)
      (MethodName "addInitScript")
      (mkBrowserContextAddInitScriptParams source)
  pure
    ( disposableFromRef
        (browserContextAddInitScriptResultDisposable result)
    )

closeContext :: BrowserContext -> Playwright ()
closeContext ctx =
  closeContextWith ctx Nothing

closeContextWith :: BrowserContext -> Maybe Text -> Playwright ()
closeContextWith ctx reason =
  ignoreTargetClosed $ do
    exportRecordHar (channelGuid (browserContextChannel ctx))
    _ :: BrowserContextCloseResult <-
      send
        (browserContextChannel ctx)
        (MethodName "close")
        mkBrowserContextCloseParams {browserContextCloseParamsReason = reason}
    pure ()

-- | Starts recording the context's traffic into a HAR file, which is written
-- when the context closes.
startContextRecordHar :: BrowserContext -> RecordHar -> Playwright ()
startContextRecordHar ctx options = do
  let guid = channelGuid (browserContextChannel ctx)
  obj <- lookupObject guid
  initializer <- throwDecode (remoteInitializer obj)
  startRecordHar guid (channelRefGuid (browserContextInitializerTracing initializer)) options

pageContext :: Page -> BrowserContext
pageContext page =
  BrowserContext (pageContextChannel page)

contextRequest :: BrowserContext -> Playwright APIRequestContext
contextRequest ctx =
  apiRequestContextFor
    (channelGuid (browserContextChannel ctx))

setContextDefaultTimeout :: BrowserContext -> Timeout -> Playwright ()
setContextDefaultTimeout ctx timeout =
  setTimeoutDefault
    (channelGuid (browserContextChannel ctx))
    (Just timeout)

setContextDefaultNavigationTimeout :: BrowserContext -> Timeout -> Playwright ()
setContextDefaultNavigationTimeout ctx timeout =
  setTimeoutNavigation
    (channelGuid (browserContextChannel ctx))
    (Just timeout)

contextsEqual :: BrowserContext -> BrowserContext -> Bool
contextsEqual left right =
  channelGuid (browserContextChannel left) == channelGuid (browserContextChannel right)

isContextClosed :: BrowserContext -> Playwright Bool
isContextClosed ctx = do
  closed <- closedGuids
  pure (Set.member (channelGuid (browserContextChannel ctx)) closed)

onContextClose :: BrowserContext -> Playwright () -> Playwright ()
onContextClose ctx run =
  registerEventHandler
    (channelGuid (browserContextChannel ctx))
    (MethodName "close")
    (\_ -> run)

onContextPage :: BrowserContext -> (Page -> Playwright ()) -> Playwright ()
onContextPage ctx run =
  registerEventHandler
    (channelGuid (browserContextChannel ctx))
    (MethodName "page")
    ( \params -> do
        event <- throwDecode params
        page <- pageFromRef (browserContextChannel ctx) (browserContextPageEventPage event)
        run page
    )

waitForContextClose :: BrowserContext -> Playwright BrowserContext
waitForContextClose ctx = do
  already <- isContextClosed ctx
  if already
    then pure ctx
    else do
      _ <-
        waitForChannelEvent
          (channelGuid (browserContextChannel ctx))
          (MethodName "close")
      pure ctx

contextPages :: BrowserContext -> Playwright [Page]
contextPages ctx = do
  let
    parent = channelGuid (browserContextChannel ctx)
  closed <- closedGuids
  objects <- traverse (lookupObject) =<< guidsOfType ChannelTypePage
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

-- | Registers for the next new page before the action that opens it runs.
armContextPage :: BrowserContext -> Playwright (Playwright Page)
armContextPage ctx = do
  let contextGuid = channelGuid (browserContextChannel ctx)
  waitId <- registerFutureEventWaiter contextGuid (MethodName "page")
  pure $ waitWithTimeout defaultTimeout $ do
    params <- waitForWaiterUnlessClosed waitId contextGuid
    event <- throwDecode params
    pageFromRef (browserContextChannel ctx) (browserContextPageEventPage event)

waitForContextPage :: BrowserContext -> Playwright Page
waitForContextPage ctx =
  waitWithTimeout defaultTimeout $ do
    params <-
      waitForFutureChannelEventUnlessClosed
        (channelGuid (browserContextChannel ctx))
        (MethodName "page")
    event <- throwDecode params
    pageFromRef (browserContextChannel ctx) (browserContextPageEventPage event)

addCookies :: BrowserContext -> [CookieToSet] -> Playwright ()
addCookies ctx cookiesToSet = do
  _ :: BrowserContextAddCookiesResult <-
    send
      (browserContextChannel ctx)
      (MethodName "addCookies")
      (mkBrowserContextAddCookiesParams (fmap toSetNetworkCookie cookiesToSet))
  pure ()

cookies :: BrowserContext -> Playwright [Cookie]
cookies ctx =
  cookiesFor ctx []

cookiesFor :: BrowserContext -> [Url] -> Playwright [Cookie]
cookiesFor ctx urls = do
  result <-
    send
      (browserContextChannel ctx)
      (MethodName "cookies")
      (mkBrowserContextCookiesParams (fmap urlToText urls))
  pure (fmap fromNetworkCookie (browserContextCookiesResultCookies result))

clearCookies :: BrowserContext -> Playwright ()
clearCookies ctx =
  clearCookiesWith ctx defaultClearCookiesOptions

clearCookiesNamed :: BrowserContext -> CookieName -> Playwright ()
clearCookiesNamed ctx name =
  clearCookiesWith ctx defaultClearCookiesOptions {clearCookiesName = Just name}

clearCookiesWith :: BrowserContext -> ClearCookiesOptions -> Playwright ()
clearCookiesWith ctx opts = do
  _ :: BrowserContextClearCookiesResult <-
    send
      (browserContextChannel ctx)
      (MethodName "clearCookies")
      mkBrowserContextClearCookiesParams
        { browserContextClearCookiesParamsName = fmap cookieNameToText (clearCookiesName opts)
        , browserContextClearCookiesParamsNameRegexSource = clearCookiesNameRegex opts
        , browserContextClearCookiesParamsNameRegexFlags =
            case clearCookiesNameRegex opts of
              Nothing ->
                Nothing
              Just _ ->
                Just ("" :: Text)
        , browserContextClearCookiesParamsDomain = fmap cookieDomainToText (clearCookiesDomain opts)
        , browserContextClearCookiesParamsPath = fmap cookiePathToText (clearCookiesPath opts)
        }
  pure ()

grantPermissions :: BrowserContext -> [PermissionName] -> Playwright ()
grantPermissions ctx names =
  grantPermissionsForOrigin ctx names Nothing

grantPermissionsFor :: BrowserContext -> [PermissionName] -> Url -> Playwright ()
grantPermissionsFor ctx names origin =
  grantPermissionsForOrigin ctx names (Just origin)

grantPermissionsForOrigin :: BrowserContext -> [PermissionName] -> Maybe Url -> Playwright ()
grantPermissionsForOrigin ctx names origin = do
  _ :: BrowserContextGrantPermissionsResult <-
    send
      (browserContextChannel ctx)
      (MethodName "grantPermissions")
      (mkBrowserContextGrantPermissionsParams (fmap permissionNameToText names))
        { browserContextGrantPermissionsParamsOrigin = fmap urlToText origin
        }
  pure ()

clearPermissions :: BrowserContext -> Playwright ()
clearPermissions ctx = do
  _ :: BrowserContextClearPermissionsResult <-
    send (browserContextChannel ctx) (MethodName "clearPermissions") mkBrowserContextClearPermissionsParams
  pure ()

setGeolocation :: BrowserContext -> Geolocation -> Playwright ()
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

clearGeolocation :: BrowserContext -> Playwright ()
clearGeolocation ctx = do
  _ :: BrowserContextSetGeolocationResult <-
    send
      (browserContextChannel ctx)
      (MethodName "setGeolocation")
      mkBrowserContextSetGeolocationParams
  pure ()

setOffline :: BrowserContext -> Bool -> Playwright ()
setOffline ctx offline = do
  _ :: BrowserContextSetOfflineResult <-
    send
      (browserContextChannel ctx)
      (MethodName "setOffline")
      (mkBrowserContextSetOfflineParams offline)
  pure ()

setContextExtraHTTPHeaders :: BrowserContext -> [HttpHeader] -> Playwright ()
setContextExtraHTTPHeaders ctx headers = do
  _ :: BrowserContextSetExtraHTTPHeadersResult <-
    send
      (browserContextChannel ctx)
      (MethodName "setExtraHTTPHeaders")
      (mkBrowserContextSetExtraHTTPHeadersParams (fmap toNameValue headers))
  pure ()

setHTTPCredentials :: BrowserContext -> HttpCredentials -> Playwright ()
setHTTPCredentials ctx creds = setHTTPCredentialsList ctx [creds]

-- | Replaces the context's HTTP credentials with the given list. The driver
-- picks the first entry whose origin matches the request; an entry without an
-- origin matches every origin.
setHTTPCredentialsList :: BrowserContext -> [HttpCredentials] -> Playwright ()
setHTTPCredentialsList ctx creds = do
  _ :: BrowserContextSetHTTPCredentialsResult <-
    send
      (browserContextChannel ctx)
      (MethodName "setHTTPCredentials")
      mkBrowserContextSetHTTPCredentialsParams
        { browserContextSetHTTPCredentialsParamsHttpCredentials =
            Just (concatMap toProtocolCredentials creds)
        }
  pure ()

storageState :: BrowserContext -> Playwright StorageState
storageState ctx = do
  result :: BrowserContextStorageStateResult <-
    send
      (browserContextChannel ctx)
      (MethodName "storageState")
      mkBrowserContextStorageStateParams
  pure (fromStorageStateResult result)

setStorageState :: BrowserContext -> StorageState -> Playwright ()
setStorageState ctx state = do
  _ :: BrowserContextSetStorageStateResult <-
    send
      (browserContextChannel ctx)
      (MethodName "setStorageState")
      mkBrowserContextSetStorageStateParams
        { browserContextSetStorageStateParamsStorageState = Just (toSetStorageState state)
        }
  pure ()
