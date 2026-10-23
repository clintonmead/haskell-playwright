module Playwright.LaunchPersistent
  ( toLaunchPersistentParams
  ) where

import Data.Scientific
  ( fromFloatDigits
  )
import Data.Text
  ( pack
  )
import Playwright.AcceptDownloads
  ( AcceptDownloads (AcceptDownloadsAccept, AcceptDownloadsDeny)
  )
import Playwright.ClientCertificate
  ( LoadedClientCertificate
  , toLaunchPersistentClientCertificates
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
  ( HttpPassword (httpPasswordToText)
  , HttpUsername (httpUsernameToText)
  )
import Playwright.HttpHeader
  ( toNameValue
  )
import Playwright.LaunchOptions
  ( LaunchOptions (launchOptionsArgs, launchOptionsChromiumSandbox, launchOptionsDownloadsPath, launchOptionsExecutablePath, launchOptionsHeadless, launchOptionsIgnoreDefaultArgs, launchOptionsProxy, launchOptionsSlowMo)
  )
import Playwright.Locale
  ( Locale (localeToText)
  )
import Playwright.NewContextOptions
  ( toProtocolCredentialsList
  , NewContextOptions (newContextAcceptDownloads, newContextBaseURL, newContextBypassCSP, newContextColorScheme, newContextContrast, newContextDeviceScaleFactor, newContextExtraHTTPHeaders, newContextForcedColors, newContextGeolocation, newContextHasTouch, newContextIgnoreHTTPSErrors, newContextIsMobile, newContextJavaScriptEnabled, newContextLocale, newContextNoDefaultViewport, newContextOffline, newContextPermissions, newContextRecordVideo, newContextReducedMotion, newContextScreen, newContextServiceWorkers, newContextStrictSelectors, newContextTimezoneId, newContextUserAgent, newContextViewport)
  )
import Playwright.PermissionName
  ( PermissionName (permissionNameToText)
  )
import Playwright.ProxySettings
  ( ProxyHost (proxyHostToText)
  , ProxySettings (proxySettingsBypass, proxySettingsPassword, proxySettingsServer, proxySettingsUsername)
  )
import Playwright.Protocol
  ( BrowserTypeLaunchPersistentContextParams (browserTypeLaunchPersistentContextParamsAcceptDownloads, browserTypeLaunchPersistentContextParamsClientCertificates, browserTypeLaunchPersistentContextParamsArgs, browserTypeLaunchPersistentContextParamsBaseURL, browserTypeLaunchPersistentContextParamsBypassCSP, browserTypeLaunchPersistentContextParamsChromiumSandbox, browserTypeLaunchPersistentContextParamsColorScheme, browserTypeLaunchPersistentContextParamsContrast, browserTypeLaunchPersistentContextParamsDeviceScaleFactor, browserTypeLaunchPersistentContextParamsDownloadsPath, browserTypeLaunchPersistentContextParamsExecutablePath, browserTypeLaunchPersistentContextParamsExtraHTTPHeaders, browserTypeLaunchPersistentContextParamsForcedColors, browserTypeLaunchPersistentContextParamsGeolocation, browserTypeLaunchPersistentContextParamsHasTouch, browserTypeLaunchPersistentContextParamsHeadless, browserTypeLaunchPersistentContextParamsHttpCredentials, browserTypeLaunchPersistentContextParamsIgnoreDefaultArgs, browserTypeLaunchPersistentContextParamsIgnoreHTTPSErrors, browserTypeLaunchPersistentContextParamsIsMobile, browserTypeLaunchPersistentContextParamsJavaScriptEnabled, browserTypeLaunchPersistentContextParamsLocale, browserTypeLaunchPersistentContextParamsNoDefaultViewport, browserTypeLaunchPersistentContextParamsOffline, browserTypeLaunchPersistentContextParamsPermissions, browserTypeLaunchPersistentContextParamsProxy, browserTypeLaunchPersistentContextParamsRecordVideo, browserTypeLaunchPersistentContextParamsReducedMotion, browserTypeLaunchPersistentContextParamsScreen, browserTypeLaunchPersistentContextParamsSelectorEngines, browserTypeLaunchPersistentContextParamsServiceWorkers, browserTypeLaunchPersistentContextParamsSlowMo, browserTypeLaunchPersistentContextParamsStrictSelectors, browserTypeLaunchPersistentContextParamsTimezoneId, browserTypeLaunchPersistentContextParamsUserAgent, browserTypeLaunchPersistentContextParamsViewport)
  , BrowserTypeLaunchPersistentContextParamsAcceptDownloads (BrowserTypeLaunchPersistentContextParamsAcceptDownloadsAccept, BrowserTypeLaunchPersistentContextParamsAcceptDownloadsDeny)
  , BrowserTypeLaunchPersistentContextParamsColorScheme (BrowserTypeLaunchPersistentContextParamsColorSchemeDark, BrowserTypeLaunchPersistentContextParamsColorSchemeLight, BrowserTypeLaunchPersistentContextParamsColorSchemeNoOverride, BrowserTypeLaunchPersistentContextParamsColorSchemeNoPreference)
  , BrowserTypeLaunchPersistentContextParamsContrast (BrowserTypeLaunchPersistentContextParamsContrastMore, BrowserTypeLaunchPersistentContextParamsContrastNoOverride, BrowserTypeLaunchPersistentContextParamsContrastNoPreference)
  , BrowserTypeLaunchPersistentContextParamsForcedColors (BrowserTypeLaunchPersistentContextParamsForcedColorsActive, BrowserTypeLaunchPersistentContextParamsForcedColorsNoOverride, BrowserTypeLaunchPersistentContextParamsForcedColorsNone)
  , BrowserTypeLaunchPersistentContextParamsGeolocation (browserTypeLaunchPersistentContextParamsGeolocationAccuracy)
  , BrowserTypeLaunchPersistentContextParamsProxy (browserTypeLaunchPersistentContextParamsProxyBypass, browserTypeLaunchPersistentContextParamsProxyPassword, browserTypeLaunchPersistentContextParamsProxyUsername)
  , BrowserTypeLaunchPersistentContextParamsReducedMotion (BrowserTypeLaunchPersistentContextParamsReducedMotionNoOverride, BrowserTypeLaunchPersistentContextParamsReducedMotionNoPreference, BrowserTypeLaunchPersistentContextParamsReducedMotionReduce)
  , BrowserTypeLaunchPersistentContextParamsServiceWorkers (BrowserTypeLaunchPersistentContextParamsServiceWorkersAllow, BrowserTypeLaunchPersistentContextParamsServiceWorkersBlock)
  , SelectorEngine
  , mkBrowserTypeLaunchPersistentContextParams
  , mkBrowserTypeLaunchPersistentContextParamsGeolocation
  , mkBrowserTypeLaunchPersistentContextParamsProxy
  , mkBrowserTypeLaunchPersistentContextParamsScreen
  , mkBrowserTypeLaunchPersistentContextParamsViewport
  , BrowserTypeLaunchPersistentContextParamsRecordVideo (browserTypeLaunchPersistentContextParamsRecordVideoDir, browserTypeLaunchPersistentContextParamsRecordVideoSize)
  , BrowserTypeLaunchPersistentContextParamsRecordVideoSize
  , BrowserTypeLaunchPersistentContextParamsScreen
  , BrowserTypeLaunchPersistentContextParamsViewport
  , mkBrowserTypeLaunchPersistentContextParamsRecordVideo
  , mkBrowserTypeLaunchPersistentContextParamsRecordVideoSize
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
import Playwright.Timeout
  ( timeoutToDouble
  )
import Playwright.TimezoneId
  ( TimezoneId (timezoneIdToText)
  )
import Playwright.UserAgent
  ( UserAgent (userAgentToText)
  )
import Playwright.ViewportSize
  ( ViewportSize (viewportHeight, viewportWidth)
  , viewportHeightToInt
  , viewportWidthToInt
  )

toLaunchPersistentParams :: FilePath -> LaunchOptions -> NewContextOptions -> [SelectorEngine] -> [LoadedClientCertificate] -> BrowserTypeLaunchPersistentContextParams
toLaunchPersistentParams userDataDir launch opts engines certs =
  ( mkBrowserTypeLaunchPersistentContextParams (pack userDataDir)
  )
    { browserTypeLaunchPersistentContextParamsAcceptDownloads = fmap toAccept (newContextAcceptDownloads opts)
    , browserTypeLaunchPersistentContextParamsArgs = launchOptionsArgs launch
    , browserTypeLaunchPersistentContextParamsBaseURL = newContextBaseURL opts
    , browserTypeLaunchPersistentContextParamsBypassCSP = newContextBypassCSP opts
    , browserTypeLaunchPersistentContextParamsChromiumSandbox = launchOptionsChromiumSandbox launch
    , browserTypeLaunchPersistentContextParamsClientCertificates = toLaunchPersistentClientCertificates certs
    , browserTypeLaunchPersistentContextParamsColorScheme = fmap toColor (newContextColorScheme opts)
    , browserTypeLaunchPersistentContextParamsContrast = fmap toContrast (newContextContrast opts)
    , browserTypeLaunchPersistentContextParamsDeviceScaleFactor =
        fmap deviceScaleFactorToScientific (newContextDeviceScaleFactor opts)
    , browserTypeLaunchPersistentContextParamsDownloadsPath = fmap pack (launchOptionsDownloadsPath launch)
    , browserTypeLaunchPersistentContextParamsExecutablePath = fmap pack (launchOptionsExecutablePath launch)
    , browserTypeLaunchPersistentContextParamsExtraHTTPHeaders = fmap (fmap toNameValue) (newContextExtraHTTPHeaders opts)
    , browserTypeLaunchPersistentContextParamsForcedColors = fmap toForced (newContextForcedColors opts)
    , browserTypeLaunchPersistentContextParamsGeolocation = fmap toGeo (newContextGeolocation opts)
    , browserTypeLaunchPersistentContextParamsHasTouch = newContextHasTouch opts
    , browserTypeLaunchPersistentContextParamsHeadless = launchOptionsHeadless launch
    , browserTypeLaunchPersistentContextParamsHttpCredentials = toProtocolCredentialsList opts
    , browserTypeLaunchPersistentContextParamsIgnoreDefaultArgs = launchOptionsIgnoreDefaultArgs launch
    , browserTypeLaunchPersistentContextParamsIgnoreHTTPSErrors = newContextIgnoreHTTPSErrors opts
    , browserTypeLaunchPersistentContextParamsIsMobile = newContextIsMobile opts
    , browserTypeLaunchPersistentContextParamsJavaScriptEnabled = newContextJavaScriptEnabled opts
    , browserTypeLaunchPersistentContextParamsLocale = fmap localeToText (newContextLocale opts)
    , browserTypeLaunchPersistentContextParamsNoDefaultViewport = newContextNoDefaultViewport opts
    , browserTypeLaunchPersistentContextParamsOffline = newContextOffline opts
    , browserTypeLaunchPersistentContextParamsPermissions = fmap (fmap permissionNameToText) (newContextPermissions opts)
    , browserTypeLaunchPersistentContextParamsProxy = fmap toLaunchProxy (launchOptionsProxy launch)
    , browserTypeLaunchPersistentContextParamsRecordVideo = fmap toRecordVideo (newContextRecordVideo opts)
    , browserTypeLaunchPersistentContextParamsReducedMotion = fmap toMotion (newContextReducedMotion opts)
    , browserTypeLaunchPersistentContextParamsScreen = fmap toScreen (newContextScreen opts)
    , browserTypeLaunchPersistentContextParamsSelectorEngines = listToMaybeEngines engines
    , browserTypeLaunchPersistentContextParamsServiceWorkers = fmap toWorkers (newContextServiceWorkers opts)
    , browserTypeLaunchPersistentContextParamsSlowMo =
        fmap (fromFloatDigits . timeoutToDouble) (launchOptionsSlowMo launch)
    , browserTypeLaunchPersistentContextParamsStrictSelectors = newContextStrictSelectors opts
    , browserTypeLaunchPersistentContextParamsTimezoneId = fmap timezoneIdToText (newContextTimezoneId opts)
    , browserTypeLaunchPersistentContextParamsUserAgent = fmap userAgentToText (newContextUserAgent opts)
    , browserTypeLaunchPersistentContextParamsViewport = fmap toViewport (newContextViewport opts)
    }

listToMaybeEngines :: [SelectorEngine] -> Maybe [SelectorEngine]
listToMaybeEngines engines =
  case engines of
    [] ->
      Nothing
    _ ->
      Just engines

toAccept :: AcceptDownloads -> BrowserTypeLaunchPersistentContextParamsAcceptDownloads
toAccept mode =
  case mode of
    AcceptDownloadsAccept ->
      BrowserTypeLaunchPersistentContextParamsAcceptDownloadsAccept
    AcceptDownloadsDeny ->
      BrowserTypeLaunchPersistentContextParamsAcceptDownloadsDeny

toColor :: ColorScheme -> BrowserTypeLaunchPersistentContextParamsColorScheme
toColor scheme =
  case scheme of
    ColorSchemeDark ->
      BrowserTypeLaunchPersistentContextParamsColorSchemeDark
    ColorSchemeLight ->
      BrowserTypeLaunchPersistentContextParamsColorSchemeLight
    ColorSchemeNoPreference ->
      BrowserTypeLaunchPersistentContextParamsColorSchemeNoPreference
    ColorSchemeNoOverride ->
      BrowserTypeLaunchPersistentContextParamsColorSchemeNoOverride

toMotion :: ReducedMotion -> BrowserTypeLaunchPersistentContextParamsReducedMotion
toMotion motion =
  case motion of
    ReducedMotionReduce ->
      BrowserTypeLaunchPersistentContextParamsReducedMotionReduce
    ReducedMotionNoPreference ->
      BrowserTypeLaunchPersistentContextParamsReducedMotionNoPreference
    ReducedMotionNoOverride ->
      BrowserTypeLaunchPersistentContextParamsReducedMotionNoOverride

toForced :: ForcedColors -> BrowserTypeLaunchPersistentContextParamsForcedColors
toForced colors =
  case colors of
    ForcedColorsActive ->
      BrowserTypeLaunchPersistentContextParamsForcedColorsActive
    ForcedColorsNone ->
      BrowserTypeLaunchPersistentContextParamsForcedColorsNone
    ForcedColorsNoOverride ->
      BrowserTypeLaunchPersistentContextParamsForcedColorsNoOverride

toContrast :: Contrast -> BrowserTypeLaunchPersistentContextParamsContrast
toContrast contrast =
  case contrast of
    ContrastNoPreference ->
      BrowserTypeLaunchPersistentContextParamsContrastNoPreference
    ContrastMore ->
      BrowserTypeLaunchPersistentContextParamsContrastMore
    ContrastNoOverride ->
      BrowserTypeLaunchPersistentContextParamsContrastNoOverride

toGeo :: Geolocation -> BrowserTypeLaunchPersistentContextParamsGeolocation
toGeo location =
  ( mkBrowserTypeLaunchPersistentContextParamsGeolocation
      (latitudeToScientific (geolocationLatitude location))
      (longitudeToScientific (geolocationLongitude location))
  )
    { browserTypeLaunchPersistentContextParamsGeolocationAccuracy =
        fmap accuracyToScientific (geolocationAccuracy location)
    }

toRecordVideo :: RecordVideo -> BrowserTypeLaunchPersistentContextParamsRecordVideo
toRecordVideo video =
  mkBrowserTypeLaunchPersistentContextParamsRecordVideo
    { browserTypeLaunchPersistentContextParamsRecordVideoDir = Just (pack (recordVideoDir video))
    , browserTypeLaunchPersistentContextParamsRecordVideoSize = fmap toVideoSize (recordVideoSize video)
    }

toVideoSize :: ViewportSize -> BrowserTypeLaunchPersistentContextParamsRecordVideoSize
toVideoSize size =
  mkBrowserTypeLaunchPersistentContextParamsRecordVideoSize
    (viewportHeightToInt (viewportHeight size))
    (viewportWidthToInt (viewportWidth size))

toViewport :: ViewportSize -> BrowserTypeLaunchPersistentContextParamsViewport
toViewport size =
  mkBrowserTypeLaunchPersistentContextParamsViewport
    (viewportHeightToInt (viewportHeight size))
    (viewportWidthToInt (viewportWidth size))

toScreen :: ViewportSize -> BrowserTypeLaunchPersistentContextParamsScreen
toScreen size =
  mkBrowserTypeLaunchPersistentContextParamsScreen
    (viewportHeightToInt (viewportHeight size))
    (viewportWidthToInt (viewportWidth size))

toWorkers :: ServiceWorkersPolicy -> BrowserTypeLaunchPersistentContextParamsServiceWorkers
toWorkers policy =
  case policy of
    ServiceWorkersAllow ->
      BrowserTypeLaunchPersistentContextParamsServiceWorkersAllow
    ServiceWorkersBlock ->
      BrowserTypeLaunchPersistentContextParamsServiceWorkersBlock

toLaunchProxy :: ProxySettings -> BrowserTypeLaunchPersistentContextParamsProxy
toLaunchProxy settings =
  ( mkBrowserTypeLaunchPersistentContextParamsProxy (proxyHostToText (proxySettingsServer settings))
  )
    { browserTypeLaunchPersistentContextParamsProxyBypass = proxySettingsBypass settings
    , browserTypeLaunchPersistentContextParamsProxyUsername = fmap httpUsernameToText (proxySettingsUsername settings)
    , browserTypeLaunchPersistentContextParamsProxyPassword = fmap httpPasswordToText (proxySettingsPassword settings)
    }
