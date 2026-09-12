module Playwright.NewContextOptions
  ( NewContextOptions (..)
  , defaultNewContextOptions
  , toBrowserNewContextParams
  ) where

import Playwright.AcceptDownloads
  ( AcceptDownloads (AcceptDownloadsAccept, AcceptDownloadsDeny)
  )
import Playwright.ColorScheme
  ( ColorScheme (ColorSchemeDark, ColorSchemeLight, ColorSchemeNoOverride, ColorSchemeNoPreference)
  )
import Playwright.DeviceScaleFactor
  ( DeviceScaleFactor (deviceScaleFactorToScientific)
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
import Playwright.Locale
  ( Locale (localeToText)
  )
import Playwright.PermissionName
  ( PermissionName (permissionNameToText)
  )
import Playwright.Protocol
  ( BrowserNewContextParams (browserNewContextParamsAcceptDownloads, browserNewContextParamsBaseURL, browserNewContextParamsBypassCSP, browserNewContextParamsColorScheme, browserNewContextParamsDeviceScaleFactor, browserNewContextParamsExtraHTTPHeaders, browserNewContextParamsGeolocation, browserNewContextParamsHasTouch, browserNewContextParamsHttpCredentials, browserNewContextParamsIgnoreHTTPSErrors, browserNewContextParamsIsMobile, browserNewContextParamsJavaScriptEnabled, browserNewContextParamsLocale, browserNewContextParamsNoDefaultViewport, browserNewContextParamsOffline, browserNewContextParamsPermissions, browserNewContextParamsStorageState, browserNewContextParamsTimezoneId, browserNewContextParamsUserAgent, browserNewContextParamsViewport)
  , BrowserNewContextParamsAcceptDownloads (BrowserNewContextParamsAcceptDownloadsAccept, BrowserNewContextParamsAcceptDownloadsDeny)
  , BrowserNewContextParamsColorScheme (BrowserNewContextParamsColorSchemeDark, BrowserNewContextParamsColorSchemeLight, BrowserNewContextParamsColorSchemeNoOverride, BrowserNewContextParamsColorSchemeNoPreference)
  , BrowserNewContextParamsGeolocation (browserNewContextParamsGeolocationAccuracy)
  , BrowserNewContextParamsHttpCredentials (browserNewContextParamsHttpCredentialsOrigin)
  , BrowserNewContextParamsViewport
  , mkBrowserNewContextParams
  , mkBrowserNewContextParamsGeolocation
  , mkBrowserNewContextParamsHttpCredentials
  , mkBrowserNewContextParamsViewport
  )
import Playwright.StorageState
  ( StorageState
  , toNewContextStorageState
  )
import Playwright.TimezoneId
  ( TimezoneId (timezoneIdToText)
  )
import Playwright.Url
  ( Url
  , urlToText
  )
import Playwright.UserAgent
  ( UserAgent (userAgentToText)
  )
import Playwright.ViewportSize
  ( ViewportSize (viewportHeight, viewportWidth)
  , viewportHeightToInt
  , viewportWidthToInt
  )

data NewContextOptions = NewContextOptions
  { newContextAcceptDownloads :: Maybe AcceptDownloads
  , newContextBaseURL :: Maybe Url
  , newContextBypassCSP :: Maybe Bool
  , newContextColorScheme :: Maybe ColorScheme
  , newContextDeviceScaleFactor :: Maybe DeviceScaleFactor
  , newContextExtraHTTPHeaders :: Maybe [HttpHeader]
  , newContextGeolocation :: Maybe Geolocation
  , newContextHasTouch :: Maybe Bool
  , newContextHttpCredentials :: Maybe HttpCredentials
  , newContextIgnoreHTTPSErrors :: Maybe Bool
  , newContextIsMobile :: Maybe Bool
  , newContextJavaScriptEnabled :: Maybe Bool
  , newContextLocale :: Maybe Locale
  , newContextNoDefaultViewport :: Maybe Bool
  , newContextOffline :: Maybe Bool
  , newContextPermissions :: Maybe [PermissionName]
  , newContextStorageState :: Maybe StorageState
  , newContextTimezoneId :: Maybe TimezoneId
  , newContextUserAgent :: Maybe UserAgent
  , newContextViewport :: Maybe ViewportSize
  }
  deriving stock (Eq, Show)

defaultNewContextOptions :: NewContextOptions
defaultNewContextOptions =
    NewContextOptions
    { newContextAcceptDownloads = Nothing
    , newContextBaseURL = Nothing
    , newContextBypassCSP = Nothing
    , newContextColorScheme = Nothing
    , newContextDeviceScaleFactor = Nothing
    , newContextExtraHTTPHeaders = Nothing
    , newContextGeolocation = Nothing
    , newContextHasTouch = Nothing
    , newContextHttpCredentials = Nothing
    , newContextIgnoreHTTPSErrors = Nothing
    , newContextIsMobile = Nothing
    , newContextJavaScriptEnabled = Nothing
    , newContextLocale = Nothing
    , newContextNoDefaultViewport = Nothing
    , newContextOffline = Nothing
    , newContextPermissions = Nothing
    , newContextStorageState = Nothing
    , newContextTimezoneId = Nothing
    , newContextUserAgent = Nothing
    , newContextViewport = Nothing
    }

toBrowserNewContextParams :: NewContextOptions -> BrowserNewContextParams
toBrowserNewContextParams opts =
  mkBrowserNewContextParams
    { browserNewContextParamsAcceptDownloads = fmap toProtocolAcceptDownloads (newContextAcceptDownloads opts)
    , browserNewContextParamsBaseURL = newContextBaseURL opts
    , browserNewContextParamsBypassCSP = newContextBypassCSP opts
    , browserNewContextParamsColorScheme = fmap toProtocolColor (newContextColorScheme opts)
    , browserNewContextParamsDeviceScaleFactor =
        fmap deviceScaleFactorToScientific (newContextDeviceScaleFactor opts)
    , browserNewContextParamsExtraHTTPHeaders = fmap (fmap toNameValue) (newContextExtraHTTPHeaders opts)
    , browserNewContextParamsGeolocation = fmap toProtocolGeolocation (newContextGeolocation opts)
    , browserNewContextParamsHasTouch = newContextHasTouch opts
    , browserNewContextParamsHttpCredentials = fmap toProtocolCredentials (newContextHttpCredentials opts)
    , browserNewContextParamsIgnoreHTTPSErrors = newContextIgnoreHTTPSErrors opts
    , browserNewContextParamsIsMobile = newContextIsMobile opts
    , browserNewContextParamsJavaScriptEnabled = newContextJavaScriptEnabled opts
    , browserNewContextParamsLocale = fmap localeToText (newContextLocale opts)
    , browserNewContextParamsNoDefaultViewport = newContextNoDefaultViewport opts
    , browserNewContextParamsOffline = newContextOffline opts
    , browserNewContextParamsPermissions = fmap (fmap permissionNameToText) (newContextPermissions opts)
    , browserNewContextParamsStorageState = fmap toNewContextStorageState (newContextStorageState opts)
    , browserNewContextParamsTimezoneId = fmap timezoneIdToText (newContextTimezoneId opts)
    , browserNewContextParamsUserAgent = fmap userAgentToText (newContextUserAgent opts)
    , browserNewContextParamsViewport = fmap toProtocolViewport (newContextViewport opts)
    }

toProtocolColor :: ColorScheme -> BrowserNewContextParamsColorScheme
toProtocolColor scheme =
  case scheme of
    ColorSchemeDark ->
      BrowserNewContextParamsColorSchemeDark
    ColorSchemeLight ->
      BrowserNewContextParamsColorSchemeLight
    ColorSchemeNoPreference ->
      BrowserNewContextParamsColorSchemeNoPreference
    ColorSchemeNoOverride ->
      BrowserNewContextParamsColorSchemeNoOverride

toProtocolAcceptDownloads :: AcceptDownloads -> BrowserNewContextParamsAcceptDownloads
toProtocolAcceptDownloads mode =
  case mode of
    AcceptDownloadsAccept ->
      BrowserNewContextParamsAcceptDownloadsAccept
    AcceptDownloadsDeny ->
      BrowserNewContextParamsAcceptDownloadsDeny

toProtocolGeolocation :: Geolocation -> BrowserNewContextParamsGeolocation
toProtocolGeolocation location =
  ( mkBrowserNewContextParamsGeolocation
      (latitudeToScientific (geolocationLatitude location))
      (longitudeToScientific (geolocationLongitude location))
  )
    { browserNewContextParamsGeolocationAccuracy =
        fmap accuracyToScientific (geolocationAccuracy location)
    }

toProtocolCredentials :: HttpCredentials -> BrowserNewContextParamsHttpCredentials
toProtocolCredentials creds =
  ( mkBrowserNewContextParamsHttpCredentials
      (httpPasswordToText (httpCredentialsPassword creds))
      (httpUsernameToText (httpCredentialsUsername creds))
  )
    { browserNewContextParamsHttpCredentialsOrigin =
        fmap urlToText (httpCredentialsOrigin creds)
    }

toProtocolViewport :: ViewportSize -> BrowserNewContextParamsViewport
toProtocolViewport size =
  mkBrowserNewContextParamsViewport
    (viewportHeightToInt (viewportHeight size))
    (viewportWidthToInt (viewportWidth size))
