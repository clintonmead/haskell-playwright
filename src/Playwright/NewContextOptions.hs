module Playwright.NewContextOptions
  ( NewContextOptions (..)
  , defaultNewContextOptions
  , toBrowserNewContextParams
  ) where

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
  ( BrowserNewContextParams (browserNewContextParamsBypassCSP, browserNewContextParamsColorScheme, browserNewContextParamsDeviceScaleFactor, browserNewContextParamsExtraHTTPHeaders, browserNewContextParamsGeolocation, browserNewContextParamsHttpCredentials, browserNewContextParamsIgnoreHTTPSErrors, browserNewContextParamsJavaScriptEnabled, browserNewContextParamsLocale, browserNewContextParamsOffline, browserNewContextParamsPermissions, browserNewContextParamsTimezoneId, browserNewContextParamsUserAgent, browserNewContextParamsViewport)
  , BrowserNewContextParamsColorScheme (BrowserNewContextParamsColorSchemeDark, BrowserNewContextParamsColorSchemeLight, BrowserNewContextParamsColorSchemeNoOverride, BrowserNewContextParamsColorSchemeNoPreference)
  , BrowserNewContextParamsGeolocation (browserNewContextParamsGeolocationAccuracy)
  , BrowserNewContextParamsHttpCredentials (browserNewContextParamsHttpCredentialsOrigin)
  , BrowserNewContextParamsViewport
  , mkBrowserNewContextParams
  , mkBrowserNewContextParamsGeolocation
  , mkBrowserNewContextParamsHttpCredentials
  , mkBrowserNewContextParamsViewport
  )
import Playwright.TimezoneId
  ( TimezoneId (timezoneIdToText)
  )
import Playwright.Url
  ( urlToText
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
  { newContextBypassCSP :: Maybe Bool
  , newContextColorScheme :: Maybe ColorScheme
  , newContextDeviceScaleFactor :: Maybe DeviceScaleFactor
  , newContextExtraHTTPHeaders :: Maybe [HttpHeader]
  , newContextGeolocation :: Maybe Geolocation
  , newContextHttpCredentials :: Maybe HttpCredentials
  , newContextIgnoreHTTPSErrors :: Maybe Bool
  , newContextJavaScriptEnabled :: Maybe Bool
  , newContextLocale :: Maybe Locale
  , newContextOffline :: Maybe Bool
  , newContextPermissions :: Maybe [PermissionName]
  , newContextTimezoneId :: Maybe TimezoneId
  , newContextUserAgent :: Maybe UserAgent
  , newContextViewport :: Maybe ViewportSize
  }
  deriving stock (Eq, Show)

defaultNewContextOptions :: NewContextOptions
defaultNewContextOptions =
  NewContextOptions
    { newContextBypassCSP = Nothing
    , newContextColorScheme = Nothing
    , newContextDeviceScaleFactor = Nothing
    , newContextExtraHTTPHeaders = Nothing
    , newContextGeolocation = Nothing
    , newContextHttpCredentials = Nothing
    , newContextIgnoreHTTPSErrors = Nothing
    , newContextJavaScriptEnabled = Nothing
    , newContextLocale = Nothing
    , newContextOffline = Nothing
    , newContextPermissions = Nothing
    , newContextTimezoneId = Nothing
    , newContextUserAgent = Nothing
    , newContextViewport = Nothing
    }

toBrowserNewContextParams :: NewContextOptions -> BrowserNewContextParams
toBrowserNewContextParams opts =
  mkBrowserNewContextParams
    { browserNewContextParamsBypassCSP = newContextBypassCSP opts
    , browserNewContextParamsColorScheme = fmap toProtocolColor (newContextColorScheme opts)
    , browserNewContextParamsDeviceScaleFactor =
        fmap deviceScaleFactorToScientific (newContextDeviceScaleFactor opts)
    , browserNewContextParamsExtraHTTPHeaders = fmap (fmap toNameValue) (newContextExtraHTTPHeaders opts)
    , browserNewContextParamsGeolocation = fmap toProtocolGeolocation (newContextGeolocation opts)
    , browserNewContextParamsHttpCredentials = fmap toProtocolCredentials (newContextHttpCredentials opts)
    , browserNewContextParamsIgnoreHTTPSErrors = newContextIgnoreHTTPSErrors opts
    , browserNewContextParamsJavaScriptEnabled = newContextJavaScriptEnabled opts
    , browserNewContextParamsLocale = fmap localeToText (newContextLocale opts)
    , browserNewContextParamsOffline = newContextOffline opts
    , browserNewContextParamsPermissions = fmap (fmap permissionNameToText) (newContextPermissions opts)
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
