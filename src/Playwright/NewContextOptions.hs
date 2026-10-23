module Playwright.NewContextOptions
  ( NewContextOptions (..)
  , defaultNewContextOptions
  , toBrowserNewContextParams
  , toProtocolCredentials
  , toProtocolCredentialsList
  ) where

import Data.Maybe
  ( maybeToList
  )
import Data.Text
  ( pack
  )
import Playwright.AcceptDownloads
  ( AcceptDownloads (AcceptDownloadsAccept, AcceptDownloadsDeny)
  )
import Playwright.ClientCertificate
  ( ClientCertificate
  )
import Playwright.ColorScheme
  ( ColorScheme (ColorSchemeDark, ColorSchemeLight, ColorSchemeNoOverride, ColorSchemeNoPreference)
  )
import Playwright.Contrast
  ( Contrast (ContrastMore, ContrastNoOverride, ContrastNoPreference)
  )
import Playwright.DeviceScaleFactor
  ( DeviceScaleFactor (deviceScaleFactorToScientific)
  )
import Playwright.ForcedColors
  ( ForcedColors (ForcedColorsActive, ForcedColorsNoOverride, ForcedColorsNone)
  )
import Playwright.Geolocation
  ( Accuracy (accuracyToScientific)
  , Geolocation (geolocationAccuracy, geolocationLatitude, geolocationLongitude)
  , Latitude (latitudeToScientific)
  , Longitude (longitudeToScientific)
  )
import Playwright.HttpCredentials
  ( HttpCredentials (httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsSend, httpCredentialsUsername)
  , HttpPassword (httpPasswordToText)
  , HttpUsername (httpUsernameToText)
  )
import Playwright.HttpCredentialsSend
  ( HttpCredentialsSend (HttpCredentialsSendAlways, HttpCredentialsSendUnauthorized)
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
import Playwright.ProxySettings
  ( ProxyHost (proxyHostToText)
  , ProxySettings (proxySettingsBypass, proxySettingsPassword, proxySettingsServer, proxySettingsUsername)
  )
import Playwright.Protocol qualified as Protocol
import Playwright.Protocol
  ( BrowserNewContextParams (browserNewContextParamsAcceptDownloads, browserNewContextParamsBaseURL, browserNewContextParamsBypassCSP, browserNewContextParamsColorScheme, browserNewContextParamsContrast, browserNewContextParamsDeviceScaleFactor, browserNewContextParamsExtraHTTPHeaders, browserNewContextParamsForcedColors, browserNewContextParamsGeolocation, browserNewContextParamsHasTouch, browserNewContextParamsHttpCredentials, browserNewContextParamsIgnoreHTTPSErrors, browserNewContextParamsIsMobile, browserNewContextParamsJavaScriptEnabled, browserNewContextParamsLocale, browserNewContextParamsNoDefaultViewport, browserNewContextParamsOffline, browserNewContextParamsPermissions, browserNewContextParamsProxy, browserNewContextParamsRecordVideo, browserNewContextParamsReducedMotion, browserNewContextParamsScreen, browserNewContextParamsServiceWorkers, browserNewContextParamsStorageState, browserNewContextParamsStrictSelectors, browserNewContextParamsTimezoneId, browserNewContextParamsUserAgent, browserNewContextParamsViewport)
  , BrowserNewContextParamsAcceptDownloads (BrowserNewContextParamsAcceptDownloadsAccept, BrowserNewContextParamsAcceptDownloadsDeny)
  , BrowserNewContextParamsRecordVideo (browserNewContextParamsRecordVideoDir, browserNewContextParamsRecordVideoSize)
  , BrowserNewContextParamsRecordVideoSize
  , mkBrowserNewContextParamsRecordVideo
  , mkBrowserNewContextParamsRecordVideoSize
  , BrowserNewContextParamsColorScheme (BrowserNewContextParamsColorSchemeDark, BrowserNewContextParamsColorSchemeLight, BrowserNewContextParamsColorSchemeNoOverride, BrowserNewContextParamsColorSchemeNoPreference)
  , BrowserNewContextParamsContrast (BrowserNewContextParamsContrastMore, BrowserNewContextParamsContrastNoOverride, BrowserNewContextParamsContrastNoPreference)
  , BrowserNewContextParamsForcedColors (BrowserNewContextParamsForcedColorsActive, BrowserNewContextParamsForcedColorsNoOverride, BrowserNewContextParamsForcedColorsNone)
  , BrowserNewContextParamsGeolocation (browserNewContextParamsGeolocationAccuracy)
  , BrowserNewContextParamsProxy (browserNewContextParamsProxyBypass, browserNewContextParamsProxyPassword, browserNewContextParamsProxyUsername)
  , BrowserNewContextParamsReducedMotion (BrowserNewContextParamsReducedMotionNoOverride, BrowserNewContextParamsReducedMotionNoPreference, BrowserNewContextParamsReducedMotionReduce)
  , BrowserNewContextParamsScreen
  , BrowserNewContextParamsServiceWorkers (BrowserNewContextParamsServiceWorkersAllow, BrowserNewContextParamsServiceWorkersBlock)
  , BrowserNewContextParamsViewport
  , mkBrowserNewContextParams
  , mkBrowserNewContextParamsGeolocation
  , mkBrowserNewContextParamsProxy
  , mkBrowserNewContextParamsScreen
  , mkBrowserNewContextParamsViewport
  )
import Playwright.RecordHar
  ( RecordHar
  )
import Playwright.RecordVideo
  ( RecordVideo (recordVideoDir, recordVideoSize)
  )
import Playwright.ReducedMotion
  ( ReducedMotion (ReducedMotionNoOverride, ReducedMotionNoPreference, ReducedMotionReduce)
  )
import Playwright.ServiceWorkersPolicy
  ( ServiceWorkersPolicy (ServiceWorkersAllow, ServiceWorkersBlock)
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
  , newContextClientCertificates :: Maybe [ClientCertificate]
  , newContextColorScheme :: Maybe ColorScheme
  , newContextContrast :: Maybe Contrast
  , newContextDeviceScaleFactor :: Maybe DeviceScaleFactor
  , newContextExtraHTTPHeaders :: Maybe [HttpHeader]
  , newContextForcedColors :: Maybe ForcedColors
  , newContextGeolocation :: Maybe Geolocation
  , newContextHasTouch :: Maybe Bool
  , newContextHttpCredentials :: Maybe HttpCredentials
  , newContextHttpCredentialsList :: [HttpCredentials]
  , newContextIgnoreHTTPSErrors :: Maybe Bool
  , newContextIsMobile :: Maybe Bool
  , newContextJavaScriptEnabled :: Maybe Bool
  , newContextLocale :: Maybe Locale
  , newContextNoDefaultViewport :: Maybe Bool
  , newContextOffline :: Maybe Bool
  , newContextPermissions :: Maybe [PermissionName]
  , newContextProxy :: Maybe ProxySettings
  , newContextReducedMotion :: Maybe ReducedMotion
  , newContextScreen :: Maybe ViewportSize
  , newContextServiceWorkers :: Maybe ServiceWorkersPolicy
  , newContextStorageState :: Maybe StorageState
  , newContextStrictSelectors :: Maybe Bool
  , newContextTimezoneId :: Maybe TimezoneId
  , newContextUserAgent :: Maybe UserAgent
  , newContextRecordHar :: Maybe RecordHar
  , newContextRecordVideo :: Maybe RecordVideo
  , newContextViewport :: Maybe ViewportSize
  }
  deriving stock (Eq, Show)

defaultNewContextOptions :: NewContextOptions
defaultNewContextOptions =
    NewContextOptions
    { newContextAcceptDownloads = Nothing
    , newContextBaseURL = Nothing
    , newContextBypassCSP = Nothing
    , newContextClientCertificates = Nothing
    , newContextColorScheme = Nothing
    , newContextContrast = Nothing
    , newContextDeviceScaleFactor = Nothing
    , newContextExtraHTTPHeaders = Nothing
    , newContextForcedColors = Nothing
    , newContextGeolocation = Nothing
    , newContextHasTouch = Nothing
    , newContextHttpCredentials = Nothing
    , newContextHttpCredentialsList = []
    , newContextIgnoreHTTPSErrors = Nothing
    , newContextIsMobile = Nothing
    , newContextJavaScriptEnabled = Nothing
    , newContextLocale = Nothing
    , newContextNoDefaultViewport = Nothing
    , newContextOffline = Nothing
    , newContextPermissions = Nothing
    , newContextProxy = Nothing
    , newContextReducedMotion = Nothing
    , newContextScreen = Nothing
    , newContextServiceWorkers = Nothing
    , newContextStorageState = Nothing
    , newContextStrictSelectors = Nothing
    , newContextTimezoneId = Nothing
    , newContextUserAgent = Nothing
    , newContextRecordHar = Nothing
    , newContextRecordVideo = Nothing
    , newContextViewport = Nothing
    }

toBrowserNewContextParams :: NewContextOptions -> BrowserNewContextParams
toBrowserNewContextParams opts =
  mkBrowserNewContextParams
    { browserNewContextParamsAcceptDownloads = fmap toProtocolAcceptDownloads (newContextAcceptDownloads opts)
    , browserNewContextParamsBaseURL = newContextBaseURL opts
    , browserNewContextParamsBypassCSP = newContextBypassCSP opts
    , browserNewContextParamsColorScheme = fmap toProtocolColor (newContextColorScheme opts)
    , browserNewContextParamsContrast = fmap toProtocolContrast (newContextContrast opts)
    , browserNewContextParamsDeviceScaleFactor =
        fmap deviceScaleFactorToScientific (newContextDeviceScaleFactor opts)
    , browserNewContextParamsExtraHTTPHeaders = fmap (fmap toNameValue) (newContextExtraHTTPHeaders opts)
    , browserNewContextParamsForcedColors = fmap toProtocolForcedColors (newContextForcedColors opts)
    , browserNewContextParamsGeolocation = fmap toProtocolGeolocation (newContextGeolocation opts)
    , browserNewContextParamsHasTouch = newContextHasTouch opts
    , browserNewContextParamsHttpCredentials = toProtocolCredentialsList opts
    , browserNewContextParamsIgnoreHTTPSErrors = newContextIgnoreHTTPSErrors opts
    , browserNewContextParamsIsMobile = newContextIsMobile opts
    , browserNewContextParamsJavaScriptEnabled = newContextJavaScriptEnabled opts
    , browserNewContextParamsLocale = fmap localeToText (newContextLocale opts)
    , browserNewContextParamsNoDefaultViewport = newContextNoDefaultViewport opts
    , browserNewContextParamsOffline = newContextOffline opts
    , browserNewContextParamsPermissions = fmap (fmap permissionNameToText) (newContextPermissions opts)
    , browserNewContextParamsProxy = fmap toProtocolProxy (newContextProxy opts)
    , browserNewContextParamsReducedMotion = fmap toProtocolReducedMotion (newContextReducedMotion opts)
    , browserNewContextParamsScreen = fmap toProtocolScreen (newContextScreen opts)
    , browserNewContextParamsServiceWorkers = fmap toProtocolServiceWorkers (newContextServiceWorkers opts)
    , browserNewContextParamsStorageState = fmap toNewContextStorageState (newContextStorageState opts)
    , browserNewContextParamsStrictSelectors = newContextStrictSelectors opts
    , browserNewContextParamsTimezoneId = fmap timezoneIdToText (newContextTimezoneId opts)
    , browserNewContextParamsUserAgent = fmap userAgentToText (newContextUserAgent opts)
    , browserNewContextParamsRecordVideo = fmap toProtocolRecordVideo (newContextRecordVideo opts)
    , browserNewContextParamsViewport = fmap toProtocolViewport (newContextViewport opts)
    }

toProtocolRecordVideo :: RecordVideo -> BrowserNewContextParamsRecordVideo
toProtocolRecordVideo video =
  mkBrowserNewContextParamsRecordVideo
    { browserNewContextParamsRecordVideoDir = Just (pack (recordVideoDir video))
    , browserNewContextParamsRecordVideoSize = fmap toProtocolVideoSize (recordVideoSize video)
    }

toProtocolVideoSize :: ViewportSize -> BrowserNewContextParamsRecordVideoSize
toProtocolVideoSize size =
  mkBrowserNewContextParamsRecordVideoSize
    (viewportHeightToInt (viewportHeight size))
    (viewportWidthToInt (viewportWidth size))

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

toProtocolReducedMotion :: ReducedMotion -> BrowserNewContextParamsReducedMotion
toProtocolReducedMotion motion =
  case motion of
    ReducedMotionReduce ->
      BrowserNewContextParamsReducedMotionReduce
    ReducedMotionNoPreference ->
      BrowserNewContextParamsReducedMotionNoPreference
    ReducedMotionNoOverride ->
      BrowserNewContextParamsReducedMotionNoOverride

toProtocolForcedColors :: ForcedColors -> BrowserNewContextParamsForcedColors
toProtocolForcedColors colors =
  case colors of
    ForcedColorsActive ->
      BrowserNewContextParamsForcedColorsActive
    ForcedColorsNone ->
      BrowserNewContextParamsForcedColorsNone
    ForcedColorsNoOverride ->
      BrowserNewContextParamsForcedColorsNoOverride

toProtocolContrast :: Contrast -> BrowserNewContextParamsContrast
toProtocolContrast contrast =
  case contrast of
    ContrastNoPreference ->
      BrowserNewContextParamsContrastNoPreference
    ContrastMore ->
      BrowserNewContextParamsContrastMore
    ContrastNoOverride ->
      BrowserNewContextParamsContrastNoOverride

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

-- | The single 'newContextHttpCredentials' comes first, then every entry of
-- 'newContextHttpCredentialsList'. Nothing is sent when neither is set.
toProtocolCredentialsList :: NewContextOptions -> Maybe [Protocol.HttpCredentials]
toProtocolCredentialsList opts =
  case concatMap toProtocolCredentials (maybeToList (newContextHttpCredentials opts) <> newContextHttpCredentialsList opts) of
    [] -> Nothing
    creds -> Just creds

toProtocolCredentials :: HttpCredentials -> [Protocol.HttpCredentials]
toProtocolCredentials creds =
  [ Protocol.HttpCredentials
      { Protocol.httpCredentialsOrigin = fmap urlToText (httpCredentialsOrigin creds)
      , Protocol.httpCredentialsPassword = httpPasswordToText (httpCredentialsPassword creds)
      , Protocol.httpCredentialsSend = fmap toProtocolCredentialsSend (httpCredentialsSend creds)
      , Protocol.httpCredentialsUsername = httpUsernameToText (httpCredentialsUsername creds)
      }
  ]

toProtocolCredentialsSend :: HttpCredentialsSend -> Protocol.HttpCredentialsSend
toProtocolCredentialsSend send =
  case send of
    HttpCredentialsSendAlways ->
      Protocol.HttpCredentialsSendAlways
    HttpCredentialsSendUnauthorized ->
      Protocol.HttpCredentialsSendUnauthorized

toProtocolViewport :: ViewportSize -> BrowserNewContextParamsViewport
toProtocolViewport size =
  mkBrowserNewContextParamsViewport
    (viewportHeightToInt (viewportHeight size))
    (viewportWidthToInt (viewportWidth size))

toProtocolScreen :: ViewportSize -> BrowserNewContextParamsScreen
toProtocolScreen size =
  mkBrowserNewContextParamsScreen
    (viewportHeightToInt (viewportHeight size))
    (viewportWidthToInt (viewportWidth size))

toProtocolServiceWorkers :: ServiceWorkersPolicy -> BrowserNewContextParamsServiceWorkers
toProtocolServiceWorkers policy =
  case policy of
    ServiceWorkersAllow ->
      BrowserNewContextParamsServiceWorkersAllow
    ServiceWorkersBlock ->
      BrowserNewContextParamsServiceWorkersBlock

toProtocolProxy :: ProxySettings -> BrowserNewContextParamsProxy
toProtocolProxy settings =
  ( mkBrowserNewContextParamsProxy (proxyHostToText (proxySettingsServer settings))
  )
    { browserNewContextParamsProxyBypass = proxySettingsBypass settings
    , browserNewContextParamsProxyUsername = fmap httpUsernameToText (proxySettingsUsername settings)
    , browserNewContextParamsProxyPassword = fmap httpPasswordToText (proxySettingsPassword settings)
    }
