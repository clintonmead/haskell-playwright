module Playwright
  ( Browser (..)
  , BrowserContext (..)
  , BrowserName (..)
  , BrowserType (..)
  , Accuracy (..)
  , APIRequestContext
  , APIResponse
  , AcceptDownloads (..)
  , AddLocatorHandlerOptions (..)
  , AriaRole (..)
  , BoundingBox (..)
  , BodySize (..)
  , BindingName (..)
  , CheckOptions (..)
  , ClickOptions (..)
  , ClockMillis (..)
  , ClockTicks (..)
  , ClockTime (..)
  , ColorScheme (..)
  , ConsoleMessage (..)
  , ConsoleType (..)
  , ContentType (..)
  , ContinueOptions (..)
  , Cookie (..)
  , CookieDomain (..)
  , CookieExpires (..)
  , CookieHttpOnly (..)
  , CookieName (..)
  , CookiePath (..)
  , CookieSameSite (..)
  , CookieSecure (..)
  , CookieToSet (..)
  , CookieValue (..)
  , CssPixel (..)
  , DeviceScaleFactor (..)
  , Dialog (..)
  , DialogType (..)
  , Disposable (..)
  , Download
  , DomEventType (..)
  , DropData (..)
  , DropFile (..)
  , DropPayload (..)
  , ElementHandle (..)
  , ElementState (..)
  , EmulateMedia (..)
  , Exactness (..)
  , FileChooser (..)
  , FetchOptions (..)
  , FetchPostData (..)
  , FetchFormField (..)
  , FetchMultipartField (..)
  , FetchMultipartFile (..)
  , FetchQueryParam (..)
  , Frame (..)
  , FulfillBody (..)
  , FulfillOptions (..)
  , Geolocation (..)
  , GetByRoleOptions (..)
  , GotoOptions (..)
  , HeaderName (..)
  , HeaderValue (..)
  , HeadersSize (..)
  , HttpCredentials (..)
  , HttpCredentialsSend (..)
  , HttpHeader (..)
  , HttpMethod (..)
  , HttpPassword (..)
  , HttpStatus (..)
  , HttpUsername (..)
  , InputFile (..)
  , InputFileName (..)
  , JsHandle (..)
  , Key (..)
  , Latitude (..)
  , LaunchOptions (..)
  , LoadState (..)
  , Locale (..)
  , Locator (..)
  , LocatorHandlerTimes (..)
  , Longitude (..)
  , MediaType (..)
  , MimeType (..)
  , NewContextOptions (..)
  , NetworkSizes (..)
  , OriginStorageState (..)
  , Page (..)
  , PageError (..)
  , PdfOptions (..)
  , PermissionName (..)
  , Playwright (..)
  , Point (..)
  , PropertyName (..)
  , RegisterSelectorOptions (..)
  , Request (..)
  , RequestFailure (..)
  , ResourceType (..)
  , Response (..)
  , Route
  , RouteOptions (..)
  , RouteTimes (..)
  , ScreenshotOptions (..)
  , ScriptTag (..)
  , SelectOption (..)
  , Selector
  , SpecialJsValue (..)
  , SuggestedFilename (..)
  , StorageItem (..)
  , StorageKind (..)
  , StorageName (..)
  , StorageState (..)
  , StorageValue (..)
  , StyleTag (..)
  , Timeout
  , TimezoneId (..)
  , Url
  , UrlGlob (..)
  , UserAgent (..)
  , ViewportHeight (..)
  , ViewportSize (..)
  , ViewportWidth (..)
  , WaitForState (..)
  , WebStorage (..)
  , WheelDelta (..)
  , Worker (..)
  , abortRoute
  , acceptDialog
  , acceptDialogWith
  , addContextInitScript
  , addCookies
  , addInitScript
  , addLocatorHandler
  , addLocatorHandlerWith
  , addScriptTag
  , addStyleTag
  , allInnerTexts
  , allLocators
  , allTextContents
  , apiResponseBody
  , apiResponseDispose
  , apiResponseHeaders
  , apiResponseHeadersArray
  , apiResponseJson
  , apiResponseOk
  , apiResponseStatus
  , apiResponseStatusText
  , apiResponseText
  , apiResponseUrl
  , armConsole
  , armDialog
  , armDownload
  , armFileChooser
  , armFrameDetached
  , armFrameNavigated
  , armLoadEvent
  , armPopup
  , asElement
  , boolArgument
  , blur
  , boundingBox
  , bringToFront
  , browserFromRef
  , browserTypeFromRef
  , check
  , checkHandle
  , checkWith
  , channelArgument
  , chainLocator
  , childLocator
  , childFrameLocator
  , childFrames
  , chromium
  , clear
  , clearCookies
  , clearCookiesNamed
  , clearGeolocation
  , clearPageErrors
  , clearPermissions
  , click
  , clickHandle
  , clickHandleWith
  , clickWith
  , clockFastForward
  , clockInstall
  , clockInstallAt
  , clockMillisFromInt
  , clockPauseAt
  , clockResume
  , clockRunFor
  , clockSetFixedTime
  , clockSetSystemTime
  , closeBrowser
  , closeContext
  , closeContextWith
  , closePage
  , content
  , contentFrameHandle
  , contextExposeFunction
  , contextFromRef
  , contextPages
  , contextRequest
  , contextRoute
  , contextRouteWith
  , continueRoute
  , continueRouteWith
  , cookieForUrl
  , cookies
  , cookiesFor
  , contextsEqual
  , cssPixelFromInt
  , currentName
  , currentUrl
  , dblclick
  , defaultAddLocatorHandlerOptions
  , defaultCheckOptions
  , defaultClickOptions
  , defaultContinueOptions
  , defaultEmulateMedia
  , defaultFetchOptions
  , defaultFulfillOptions
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , defaultPdfOptions
  , defaultRegisterSelectorOptions
  , defaultRouteOptions
  , defaultScreenshotOptions
  , defaultTimeout
  , describeLocator
  , deviceScaleFactorFromInt
  , dispatchEvent
  , dispose
  , disposeHandle
  , downloadPath
  , downloadPage
  , downloadSuggestedFilename
  , downloadUrl
  , dragAndDrop
  , dropOnto
  , dismissDialog
  , elementArgument
  , elementHandle
  , elementHandles
  , emptyScriptTag
  , emptyStyleTag
  , emptyDropPayload
  , emulateMedia
  , emptyStorageState
  , enterFrameSelector
  , evalOnSelectorAll
  , evalOnSelectorAllBool
  , evalOnSelectorAllHandle
  , evalOnSelectorAllHandleNumber
  , evalOnSelectorAllHandleTexts
  , evalOnSelectorAllNumber
  , evalOnSelectorAllTexts
  , evalOnSelectorAllWith
  , evalOnSelectorBool
  , evalOnSelectorHandle
  , evalOnSelectorHandleText
  , evalOnSelector
  , evalOnSelectorText
  , evalOnSelectorTextWith
  , evalOnSelectorWith
  , evaluate
  , evaluateBool
  , evaluateBoolWith
  , evaluateHandle
  , evaluateHandleExpression
  , evaluateHandleOnElement
  , evaluateHandleOnFrame
  , evaluateHandleOnLocator
  , evaluateHandleWith
  , evaluateNumber
  , evaluateNumberWith
  , evaluateNumbersWith
  , evaluateOnElement
  , evaluateOnElementBoolWith
  , evaluateOnElementNumber
  , evaluateOnElementText
  , evaluateOnElementWith
  , evaluateOnFrame
  , evaluateOnFrameText
  , evaluateOnHandle
  , evaluateOnHandleExpression
  , evaluateOnHandleExpressionNumbers
  , evaluateOnHandleNumbers
  , evaluateOnLocator
  , evaluateOnLocatorAllTexts
  , evaluateOnLocatorBool
  , evaluateOnLocatorNumber
  , evaluateOnLocatorText
  , evaluateOnLocatorWith
  , evaluateOnWorker
  , evaluateOnWorkerText
  , evaluateSpecial
  , evaluateSpecialWith
  , evaluateText
  , evaluateTextWith
  , evaluateTexts
  , evaluateWith
  , exposeFunction
  , fallbackRoute
  , fallbackRouteWith
  , fetchDelete
  , fetchDeleteWith
  , fetchFromUrl
  , fetchFromUrlWith
  , fetchGet
  , fetchGetWith
  , fetchHead
  , fetchHeadWith
  , fetchPatch
  , fetchPatchWith
  , fetchPost
  , fetchPostWith
  , fetchPut
  , fetchPutWith
  , fetchRequest
  , fetchRequestWith
  , apiRequestStorageState
  , disposeAPIRequestContext
  , fill
  , fillHandle
  , filterVisible
  , firefox
  , first
  , focus
  , focusHandle
  , frameByName
  , frameElement
  , frameLocator
  , frames
  , framesEqual
  , fulfillRoute
  , getAttribute
  , getAttributeHandle
  , getByLabel
  , getByRole
  , getByTestId
  , getByText
  , getProperty
  , getPropertyList
  , goBack
  , goForward
  , goto
  , gotoFrame
  , grantPermissions
  , grantPermissionsFor
  , hover
  , highlight
  , hideHighlight
  , hoverHandle
  , httpStatusOk
  , initializePlaywright
  , innerHtml
  , innerHtmlHandle
  , innerText
  , innerTextHandle
  , innerTextWith
  , inputValue
  , inputValueHandle
  , isChecked
  , isClosed
  , isDisabled
  , isEditable
  , isEditableHandle
  , isEnabled
  , isHidden
  , isVisible
  , jsHandleArgument
  , jsHandleToString
  , jsonValue
  , jsonValueNumber
  , jsonValueSpecial
  , jsonValueText
  , jsonValueTexts
  , keyboardDown
  , keyboardInsertText
  , keyboardPress
  , keyboardType
  , keyboardUp
  , lastLocator
  , latitudeFromInt
  , launch
  , localStorage
  , locator
  , locatorBoundingBox
  , locatorCount
  , locatorDescription
  , locatorOnFrame
  , locatorPage
  , locatorScrollIntoView
  , locatorSelectText
  , longitudeFromInt
  , mainFrame
  , milliseconds
  , mouseClick
  , mouseDown
  , mouseMove
  , mouseUp
  , mouseWheel
  , nestedSelector
  , newContext
  , newContextWith
  , newPage
  , nth
  , numberArgument
  , numbersArgument
  , objectNumberArgument
  , objectSpecialArgument
  , objectTextArgument
  , onLoad
  , onRequest
  , onRequestFailed
  , onRequestFinished
  , onResponse
  , opener
  , ownerFrameHandle
  , pageContext
  , pageErrors
  , pageFromRef
  , pageGetAttribute
  , pageInnerHtml
  , pageInnerText
  , pageInputValue
  , pageIsChecked
  , pageIsDisabled
  , pageIsEditable
  , pageIsEnabled
  , pageIsHidden
  , pageIsVisible
  , pagesEqual
  , pageTextContent
  , pageRequest
  , pdf
  , pdfWith
  , press
  , pressSequentially
  , querySelector
  , querySelectorAll
  , querySelectorAllHandle
  , querySelectorHandle
  , querySelectorStrict
  , reload
  , removeLocatorHandler
  , registerSelector
  , registerSelectorWith
  , requestFrame
  , requestFailure
  , requestGC
  , requestHeaders
  , requestIsNavigation
  , requestMethod
  , requestPostData
  , requestPostDataJSON
  , requestRedirectedFrom
  , requestRedirectedTo
  , requestResourceType
  , requestResponse
  , requestSizes
  , requestUrl
  , requests
  , requestsEqual
  , responseFrame
  , responseFinished
  , responseHeaders
  , responseOk
  , responseRequest
  , responseStatus
  , responseStatusText
  , responseText
  , responseUrl
  , route
  , routeFetch
  , routeFetchWith
  , routeRequest
  , routeWith
  , screenshot
  , screenshotHandle
  , scrollIntoViewIfNeeded
  , scrollIntoViewIfNeededWith
  , selectOption
  , selectOptionHandle
  , selectText
  , selectTextWith
  , selectorFromText
  , selectorToText
  , sessionExpires
  , sessionStorage
  , setChecked
  , setCheckedHandle
  , setContent
  , setContextDefaultNavigationTimeout
  , setContextDefaultTimeout
  , setContextExtraHTTPHeaders
  , setDefaultNavigationTimeout
  , setDefaultTimeout
  , setExtraHTTPHeaders
  , setGeolocation
  , setHTTPCredentials
  , setInputFiles
  , setInputFilesHandle
  , setOffline
  , setStorageState
  , setViewportSize
  , specialArgument
  , storageClear
  , storageGetItem
  , storageItems
  , storageRemoveItem
  , storageSetItem
  , storageState
  , tap
  , tapHandle
  , textArgument
  , textContent
  , textContentHandle
  , timeoutToDouble
  , title
  , typeHandle
  , typeText
  , uncheck
  , uncheckHandle
  , uncheckWith
  , unroute
  , unrouteAll
  , unrouteContext
  , unrouteContextAll
  , url
  , urlFromText
  , urlToText
  , waitFor
  , waitForClose
  , waitForConsole
  , waitForContextPage
  , waitForDialog
  , waitForDownload
  , waitForElementState
  , waitForElementStateWith
  , waitForFileChooser
  , waitForFileChooserWith
  , waitForFunction
  , waitForFunctionWith
  , waitForLoadEvent
  , waitForLoadState
  , waitForNavigation
  , waitForNavigationWith
  , waitForFrameNavigated
  , waitForFrameDetached
  , waitForPageError
  , waitForPopup
  , waitUntilJsHandlePreview
  , waitForRequest
  , waitForRequestFailed
  , waitForRequestFailedWith
  , waitForRequestFinished
  , waitForRequestWith
  , waitForResponse
  , waitForResponseWith
  , waitForSelector
  , waitForSelectorHandle
  , waitForSelectorHandleOnFrame
  , waitForSelectorHandleWith
  , waitForSelectorOnHandle
  , waitForSelectorOnHandleWith
  , waitForSelectorStrict
  , waitForTimeout
  , waitForURL
  , waitForURLWith
  , waitForWorker
  , waitForWith
  , webkit
  , wheelDeltaFromInt
  , withBrowser
  , withPage
  , withPageWith
  , withPlaywright
  , workerUrl
  , workers
  ) where

import Playwright.AcceptDownloads
  ( AcceptDownloads (AcceptDownloadsAccept, AcceptDownloadsDeny)
  )
import Playwright.APIRequestContext
  ( APIRequestContext
  , apiRequestStorageState
  , disposeAPIRequestContext
  , fetchDelete
  , fetchDeleteWith
  , fetchFromUrl
  , fetchFromUrlWith
  , fetchGet
  , fetchGetWith
  , fetchHead
  , fetchHeadWith
  , fetchPatch
  , fetchPatchWith
  , fetchPost
  , fetchPostWith
  , fetchPut
  , fetchPutWith
  , fetchRequest
  , fetchRequestWith
  )
import Playwright.APIResponse
  ( APIResponse
  , apiResponseBody
  , apiResponseDispose
  , apiResponseHeaders
  , apiResponseHeadersArray
  , apiResponseJson
  , apiResponseOk
  , apiResponseStatus
  , apiResponseStatusText
  , apiResponseText
  , apiResponseUrl
  )
import Playwright.AriaRole
  ( AriaRole (AriaRole, ariaRoleToText)
  )
import Playwright.BindingName
  ( BindingName (BindingName, bindingNameToText)
  )
import Playwright.BoundingBox
  ( BoundingBox (BoundingBox, boundingBoxHeight, boundingBoxWidth, boundingBoxX, boundingBoxY)
  )
import Playwright.Browser
  ( Browser (Browser, browserChannel)
  , browserFromRef
  , closeBrowser
  , newContext
  , newContextWith
  , withPage
  , withPageWith
  )
import Playwright.BrowserContext
  ( BrowserContext (BrowserContext, browserContextChannel)
  , addContextInitScript
  , addCookies
  , clearCookies
  , clearCookiesNamed
  , clearGeolocation
  , clearPermissions
  , closeContext
  , closeContextWith
  , contextFromRef
  , contextPages
  , cookies
  , cookiesFor
  , contextsEqual
  , contextRequest
  , grantPermissions
  , grantPermissionsFor
  , newPage
  , pageContext
  , setContextDefaultNavigationTimeout
  , setContextDefaultTimeout
  , setContextExtraHTTPHeaders
  , setGeolocation
  , setHTTPCredentials
  , setOffline
  , setStorageState
  , storageState
  , waitForContextPage
  )
import Playwright.BrowserName
  ( BrowserName (Chromium, Firefox, WebKit)
  )
import Playwright.BrowserType
  ( BrowserType (BrowserType, browserTypeChannel, browserTypeName)
  , browserTypeFromRef
  , launch
  , withBrowser
  )
import Playwright.CheckOptions
  ( CheckOptions (CheckOptions, checkOptionsForce, checkOptionsTimeout, checkOptionsTrial)
  , defaultCheckOptions
  )
import Playwright.ClickOptions
  ( ClickOptions (ClickOptions, clickOptionsForce, clickOptionsNoWaitAfter, clickOptionsPosition, clickOptionsTimeout)
  , defaultClickOptions
  )
import Playwright.Clock
  ( ClockMillis (ClockMillis, clockMillisToScientific)
  , ClockTicks (ClockTicksMillis, ClockTicksString)
  , ClockTime (ClockTimeMillis, ClockTimeString)
  , clockFastForward
  , clockInstall
  , clockInstallAt
  , clockMillisFromInt
  , clockPauseAt
  , clockResume
  , clockRunFor
  , clockSetFixedTime
  , clockSetSystemTime
  )
import Playwright.ColorScheme
  ( ColorScheme (ColorSchemeDark, ColorSchemeLight, ColorSchemeNoOverride, ColorSchemeNoPreference)
  )
import Playwright.ConsoleMessage
  ( ConsoleMessage (ConsoleMessage, consoleMessageArgs, consoleMessageText, consoleMessageType)
  )
import Playwright.ConsoleType
  ( ConsoleType (ConsoleAssert, ConsoleClear, ConsoleCount, ConsoleDebug, ConsoleDir, ConsoleDirXml, ConsoleError, ConsoleInfo, ConsoleLog, ConsoleProfile, ConsoleProfileEnd, ConsoleTable, ConsoleTimeEnd, ConsoleTrace, ConsoleVerbose, ConsoleWarning)
  )
import Playwright.ContinueOptions
  ( ContinueOptions (ContinueOptions, continueHeaders, continueMethod, continuePostData, continueUrl)
  , defaultContinueOptions
  )
import Playwright.Cookie
  ( Cookie (Cookie, cookieDomain, cookieExpires, cookieHttpOnly, cookieName, cookiePath, cookieSameSite, cookieSecure, cookieValue)
  , CookieDomain (CookieDomain, cookieDomainToText)
  , CookieExpires (CookieExpires, cookieExpiresToScientific)
  , CookieHttpOnly (CookieHttpOnly, cookieHttpOnlyToBool)
  , CookieName (CookieName, cookieNameToText)
  , CookiePath (CookiePath, cookiePathToText)
  , CookieSecure (CookieSecure, cookieSecureToBool)
  , CookieToSet (CookieToSet, cookieToSetDomain, cookieToSetExpires, cookieToSetHttpOnly, cookieToSetName, cookieToSetPath, cookieToSetSameSite, cookieToSetSecure, cookieToSetUrl, cookieToSetValue)
  , CookieValue (CookieValue, cookieValueToText)
  , cookieForUrl
  , sessionExpires
  )
import Playwright.CookieSameSite
  ( CookieSameSite (CookieSameSiteLax, CookieSameSiteNone, CookieSameSiteStrict)
  )
import Playwright.CssPixel
  ( CssPixel (CssPixel, cssPixelToScientific)
  , cssPixelFromInt
  )
import Playwright.DeviceScaleFactor
  ( DeviceScaleFactor (DeviceScaleFactor, deviceScaleFactorToScientific)
  , deviceScaleFactorFromInt
  )
import Playwright.Dialog
  ( Dialog (Dialog, dialogChannel, dialogDefaultValue, dialogMessage, dialogType)
  , acceptDialog
  , acceptDialogWith
  , dismissDialog
  )
import Playwright.DialogType
  ( DialogType (DialogAlert, DialogBeforeUnload, DialogConfirm, DialogPrompt)
  )
import Playwright.Download
  ( Download
  , SuggestedFilename (SuggestedFilename, suggestedFilenameToText)
  , armDownload
  , downloadPage
  , downloadPath
  , downloadSuggestedFilename
  , downloadUrl
  , waitForDownload
  )
import Playwright.DropPayload
  ( DropData (DropData, dropDataMimeType, dropDataValue)
  , DropFile (DropFile, dropFileBytes, dropFileMimeType, dropFileName)
  , DropPayload (DropPayload, dropPayloadData, dropPayloadFiles)
  , emptyDropPayload
  )
import Playwright.Disposable
  ( Disposable (Disposable)
  , dispose
  )
import Playwright.DomEventType
  ( DomEventType (DomEventType)
  )
import Playwright.ElementHandle
  ( ElementHandle (ElementHandle)
  , boundingBox
  , clickHandle
  , clickHandleWith
  , contentFrameHandle
  , elementArgument
  , getAttributeHandle
  , innerHtmlHandle
  , innerTextHandle
  , inputValueHandle
  , isEditableHandle
  , ownerFrameHandle
  , scrollIntoViewIfNeeded
  , scrollIntoViewIfNeededWith
  , screenshotHandle
  , selectText
  , selectTextWith
  , setInputFilesHandle
  , tapHandle
  , textContentHandle
  , typeHandle
  , waitForElementState
  , waitForElementStateWith
  )
import Playwright.ElementHandle.Action
  ( checkHandle
  , disposeHandle
  , fillHandle
  , focusHandle
  , hoverHandle
  , selectOptionHandle
  , setCheckedHandle
  , uncheckHandle
  )
import Playwright.ElementHandle.Query
  ( querySelectorAllHandle
  , querySelectorHandle
  , waitForSelectorOnHandle
  , waitForSelectorOnHandleWith
  )
import Playwright.ElementState
  ( ElementState (ElementDisabled, ElementEditable, ElementEnabled, ElementHidden, ElementStable, ElementVisible)
  )
import Playwright.EmulateMedia
  ( EmulateMedia (EmulateMedia, emulateMediaColorScheme, emulateMediaType)
  , defaultEmulateMedia
  , emulateMedia
  )
import Playwright.Evaluate
  ( allInnerTexts
  , allTextContents
  , evalOnSelectorAll
  , evalOnSelectorAllBool
  , evalOnSelectorAllHandle
  , evalOnSelectorAllHandleNumber
  , evalOnSelectorAllHandleTexts
  , evalOnSelectorAllNumber
  , evalOnSelectorAllTexts
  , evalOnSelectorAllWith
  , evalOnSelectorBool
  , evalOnSelectorHandle
  , evalOnSelectorHandleText
  , evalOnSelector
  , evalOnSelectorText
  , evalOnSelectorTextWith
  , evalOnSelectorWith
  , evaluate
  , evaluateBool
  , evaluateBoolWith
  , evaluateHandle
  , evaluateHandleExpression
  , evaluateHandleOnElement
  , evaluateHandleOnFrame
  , evaluateHandleOnLocator
  , evaluateHandleWith
  , evaluateNumber
  , evaluateNumberWith
  , evaluateNumbersWith
  , evaluateOnElement
  , evaluateOnElementBoolWith
  , evaluateOnElementNumber
  , evaluateOnElementText
  , evaluateOnElementWith
  , evaluateOnFrame
  , evaluateOnFrameText
  , evaluateOnHandle
  , evaluateOnHandleExpression
  , evaluateOnHandleExpressionNumbers
  , evaluateOnHandleNumbers
  , evaluateOnLocator
  , evaluateOnLocatorAllTexts
  , evaluateOnLocatorBool
  , evaluateOnLocatorNumber
  , evaluateOnLocatorText
  , evaluateOnLocatorWith
  , evaluateSpecial
  , evaluateSpecialWith
  , evaluateText
  , evaluateTextWith
  , evaluateTexts
  , evaluateWith
  )
import Playwright.Exactness
  ( Exactness (Exact, Inexact)
  )
import Playwright.Expose
  ( contextExposeFunction
  , exposeFunction
  )
import Playwright.FetchOptions
  ( FetchFormField (FetchFormField, fetchFormFieldName, fetchFormFieldValue)
  , FetchMultipartField (FetchMultipartField, FetchMultipartFileField)
  , FetchMultipartFile (FetchMultipartFile, fetchMultipartFileBytes, fetchMultipartFileMimeType, fetchMultipartFileName)
  , FetchOptions (FetchOptions, fetchEncodedParams, fetchFailOnStatusCode, fetchFormData, fetchHeaders, fetchIgnoreHTTPSErrors, fetchMaxRedirects, fetchMaxRetries, fetchMethod, fetchMultipart, fetchPostData, fetchQueryParams, fetchTimeout, fetchUrl)
  , FetchPostData (FetchPostBytes, FetchPostJson, FetchPostText)
  , FetchQueryParam (FetchQueryParam, fetchQueryParamName, fetchQueryParamValue)
  , defaultFetchOptions
  )
import Playwright.FileChooser
  ( FileChooser (FileChooser, fileChooserElement, fileChooserMultiple)
  , armFileChooser
  , waitForFileChooser
  , waitForFileChooserWith
  )
import Playwright.Frame
  ( Frame (Frame, frameChannel, frameParent, frameUrl)
  , childFrames
  , currentName
  , currentUrl
  , framesEqual
  , gotoFrame
  )
import Playwright.Frame.Inspect
  ( frameElement
  )
import Playwright.Frame.Wait
  ( waitForSelectorHandleOnFrame
  )
import Playwright.FulfillOptions
  ( ContentType (ContentType, contentTypeToText)
  , FulfillBody (FulfillBytes, FulfillText)
  , FulfillOptions (FulfillOptions, fulfillBody, fulfillContentType, fulfillHeaders, fulfillResponse, fulfillStatus)
  , defaultFulfillOptions
  )
import Playwright.Geolocation
  ( Accuracy (Accuracy, accuracyToScientific)
  , Geolocation (Geolocation, geolocationAccuracy, geolocationLatitude, geolocationLongitude)
  , Latitude (Latitude, latitudeToScientific)
  , Longitude (Longitude, longitudeToScientific)
  , latitudeFromInt
  , longitudeFromInt
  )
import Playwright.GetByRoleOptions
  ( GetByRoleOptions (GetByRoleOptions, getByRoleChecked, getByRoleDisabled, getByRoleExact, getByRoleExpanded, getByRoleIncludeHidden, getByRoleLevel, getByRoleName, getByRolePressed, getByRoleSelected)
  , defaultGetByRoleOptions
  )
import Playwright.AddLocatorHandlerOptions
  ( AddLocatorHandlerOptions (AddLocatorHandlerOptions, addLocatorHandlerNoWaitAfter, addLocatorHandlerTimes)
  , LocatorHandlerTimes (LocatorHandlerTimes, locatorHandlerTimesToInt)
  , defaultAddLocatorHandlerOptions
  )
import Playwright.RegisterSelectorOptions
  ( RegisterSelectorOptions (RegisterSelectorOptions, registerSelectorContentScript)
  , defaultRegisterSelectorOptions
  )
import Playwright.GotoOptions
  ( GotoOptions (GotoOptions, gotoOptionsReferer, gotoOptionsTimeout, gotoOptionsWaitUntil)
  , defaultGotoOptions
  )
import Playwright.HttpCredentials
  ( HttpCredentials (HttpCredentials, httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsSend, httpCredentialsUsername)
  , HttpPassword (HttpPassword, httpPasswordToText)
  , HttpUsername (HttpUsername, httpUsernameToText)
  )
import Playwright.HttpCredentialsSend
  ( HttpCredentialsSend (HttpCredentialsSendAlways, HttpCredentialsSendUnauthorized)
  )
import Playwright.HttpHeader
  ( HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  )
import Playwright.HttpMethod
  ( HttpMethod (HttpMethod, httpMethodToText)
  )
import Playwright.HttpStatus
  ( HttpStatus (HttpStatus, httpStatusToInt)
  , httpStatusOk
  )
import Playwright.InputFile
  ( InputFile (InputFile, inputFileBytes, inputFileName)
  )
import Playwright.InputFileName
  ( InputFileName (InputFileName, inputFileNameToText)
  )
import Playwright.Instance
  ( Playwright (Playwright, playwrightChannel, playwrightChromium, playwrightFirefox, playwrightWebkit)
  , chromium
  , firefox
  , initializePlaywright
  , webkit
  , withPlaywright
  )
import Playwright.JsHandle
  ( JsHandle (JsHandle)
  , asElement
  , getProperty
  , getPropertyList
  , jsHandleArgument
  , jsHandleToString
  , jsonValue
  , jsonValueNumber
  , jsonValueSpecial
  , jsonValueText
  , jsonValueTexts
  , waitUntilJsHandlePreview
  )
import Playwright.Key
  ( Key (Key, keyToText)
  )
import Playwright.Keyboard
  ( keyboardDown
  , keyboardInsertText
  , keyboardPress
  , keyboardType
  , keyboardUp
  )
import Playwright.LaunchOptions
  ( LaunchOptions (LaunchOptions, launchOptionsChromiumSandbox, launchOptionsHeadless, launchOptionsSlowMo, launchOptionsTimeout)
  , defaultLaunchOptions
  )
import Playwright.LoadState
  ( LoadState (LoadStateCommit, LoadStateDomContentLoaded, LoadStateLoad, LoadStateNetworkIdle)
  )
import Playwright.Locale
  ( Locale (Locale, localeToText)
  )
import Playwright.Locator
  ( Locator (Locator, locatorContextChannel, locatorFrame, locatorMainFrame, locatorPageChannel, locatorSelector)
  , allLocators
  , blur
  , chainLocator
  , check
  , checkWith
  , childFrameLocator
  , childLocator
  , clear
  , click
  , clickWith
  , describeLocator
  , dblclick
  , dispatchEvent
  , dragAndDrop
  , dropOnto
  , elementHandle
  , elementHandles
  , fill
  , filterVisible
  , first
  , focus
  , getAttribute
  , hover
  , highlight
  , hideHighlight
  , innerHtml
  , innerText
  , innerTextWith
  , inputValue
  , isChecked
  , isDisabled
  , isEditable
  , isEnabled
  , isHidden
  , isVisible
  , lastLocator
  , locatorBoundingBox
  , locatorCount
  , locatorDescription
  , locatorScrollIntoView
  , locatorSelectText
  , nth
  , press
  , pressSequentially
  , selectOption
  , setChecked
  , setInputFiles
  , tap
  , textContent
  , typeText
  , uncheck
  , uncheckWith
  , waitFor
  , waitForWith
  )
import Playwright.LocatorHandler
  ( addLocatorHandler
  , addLocatorHandlerWith
  , removeLocatorHandler
  )
import Playwright.Mouse
  ( mouseClick
  , mouseDown
  , mouseMove
  , mouseUp
  , mouseWheel
  )
import Playwright.MediaType
  ( MediaType (MediaNoOverride, MediaPrint, MediaScreen)
  )
import Playwright.MimeType
  ( MimeType (MimeType, mimeTypeToText)
  )
import Playwright.NewContextOptions
  ( NewContextOptions (NewContextOptions, newContextAcceptDownloads, newContextBaseURL, newContextBypassCSP, newContextColorScheme, newContextDeviceScaleFactor, newContextExtraHTTPHeaders, newContextGeolocation, newContextHasTouch, newContextHttpCredentials, newContextIgnoreHTTPSErrors, newContextIsMobile, newContextJavaScriptEnabled, newContextLocale, newContextNoDefaultViewport, newContextOffline, newContextPermissions, newContextStorageState, newContextTimezoneId, newContextUserAgent, newContextViewport)
  , defaultNewContextOptions
  )
import Playwright.NetworkSizes
  ( BodySize (BodySize, bodySizeToInt)
  , HeadersSize (HeadersSize, headersSizeToInt)
  , NetworkSizes (NetworkSizes, networkRequestBodySize, networkRequestHeadersSize, networkResponseBodySize, networkResponseHeadersSize)
  )
import Playwright.Page
  ( Page (Page, pageChannel, pageContextChannel, pageMainFrame)
  , addInitScript
  , addScriptTag
  , addStyleTag
  , armConsole
  , armDialog
  , armFrameDetached
  , armFrameNavigated
  , armLoadEvent
  , armPopup
  , bringToFront
  , closePage
  , content
  , getByLabel
  , getByRole
  , getByTestId
  , getByText
  , goBack
  , goForward
  , goto
  , isClosed
  , locator
  , locatorOnFrame
  , locatorPage
  , frameLocator
  , onLoad
  , opener
  , pageFromRef
  , pageRequest
  , pagesEqual
  , pdf
  , pdfWith
  , querySelector
  , querySelectorAll
  , querySelectorStrict
  , reload
  , requestGC
  , screenshot
  , setContent
  , setDefaultNavigationTimeout
  , setDefaultTimeout
  , setExtraHTTPHeaders
  , setViewportSize
  , title
  , url
  , waitForConsole
  , waitForClose
  , waitForDialog
  , waitForFunction
  , waitForFunctionWith
  , waitForLoadEvent
  , waitForLoadState
  , waitForNavigation
  , waitForNavigationWith
  , waitForFrameNavigated
  , waitForFrameDetached
  , waitForPopup
  , waitForRequest
  , waitForRequestFailed
  , waitForRequestFailedWith
  , waitForRequestFinished
  , waitForRequestWith
  , waitForResponse
  , waitForResponseWith
  , waitForSelector
  , waitForSelectorHandle
  , waitForSelectorHandleWith
  , waitForSelectorStrict
  , waitForTimeout
  , waitForURL
  , waitForURLWith
  , frameByName
  , frames
  , mainFrame
  )
import Playwright.Page.Query
  ( pageGetAttribute
  , pageInnerHtml
  , pageInnerText
  , pageInputValue
  , pageIsChecked
  , pageIsDisabled
  , pageIsEditable
  , pageIsEnabled
  , pageIsHidden
  , pageIsVisible
  , pageTextContent
  )
import Playwright.Page.Network
  ( onRequest
  , onRequestFailed
  , onRequestFinished
  , onResponse
  , requests
  , responseFinished
  )
import Playwright.PageError
  ( PageError (PageError, pageErrorMessage, pageErrorName, pageErrorStack)
  , clearPageErrors
  , pageErrors
  , waitForPageError
  )
import Playwright.PdfOptions
  ( PdfOptions (PdfOptions, pdfOptionsOutline, pdfOptionsTagged)
  , defaultPdfOptions
  )
import Playwright.PermissionName
  ( PermissionName (PermissionName, permissionNameToText)
  )
import Playwright.Point
  ( Point (Point)
  )
import Playwright.PropertyName
  ( PropertyName (PropertyName, propertyNameToText)
  )
import Playwright.Request
  ( Request (Request, requestChannel)
  , requestFailure
  , requestHeaders
  , requestIsNavigation
  , requestMethod
  , requestPostData
  , requestPostDataJSON
  , requestRedirectedFrom
  , requestRedirectedTo
  , requestResourceType
  , requestSizes
  , requestUrl
  , requestsEqual
  )
import Playwright.RequestFailure
  ( RequestFailure (RequestFailure, requestFailureText)
  )
import Playwright.RequestFrame
  ( requestFrame
  , responseFrame
  )
import Playwright.ResourceType
  ( ResourceType (ResourceType, resourceTypeToText)
  )
import Playwright.Response
  ( Response (Response, responseChannel)
  , responseHeaders
  , responseOk
  , requestResponse
  , responseRequest
  , responseStatus
  , responseStatusText
  , responseText
  , responseUrl
  )
import Playwright.Route
  ( Route
  , abortRoute
  , continueRoute
  , continueRouteWith
  , contextRoute
  , contextRouteWith
  , fallbackRoute
  , fallbackRouteWith
  , fulfillRoute
  , route
  , routeFetch
  , routeFetchWith
  , routeRequest
  , routeWith
  , unroute
  , unrouteAll
  , unrouteContext
  , unrouteContextAll
  )
import Playwright.RouteOptions
  ( RouteOptions (RouteOptions, routeTimes)
  , RouteTimes (RouteTimes, routeTimesToInt)
  , defaultRouteOptions
  )
import Playwright.ScreenshotOptions
  ( ScreenshotOptions (ScreenshotOptions, screenshotOptionsFullPage, screenshotOptionsTimeout)
  , defaultScreenshotOptions
  )
import Playwright.ScriptTag
  ( ScriptTag (ScriptTag, scriptTagContent, scriptTagType, scriptTagUrl)
  , emptyScriptTag
  )
import Playwright.SelectOption
  ( SelectOption (SelectIndex, SelectLabel, SelectValue, SelectValueOrLabel)
  )
import Playwright.Selector
  ( Selector
  , enterFrameSelector
  , nestedSelector
  , selectorFromText
  , selectorToText
  )
import Playwright.Selectors
  ( registerSelector
  , registerSelectorWith
  )
import Playwright.SerializedArgument
  ( boolArgument
  , channelArgument
  , numberArgument
  , numbersArgument
  , objectNumberArgument
  , objectSpecialArgument
  , objectTextArgument
  , specialArgument
  , textArgument
  )
import Playwright.SpecialJsValue
  ( SpecialJsValue (SpecialInfinity, SpecialNaN, SpecialNegInfinity, SpecialNegZero, SpecialNull, SpecialUndefined)
  )
import Playwright.StorageItem
  ( StorageItem (StorageItem, storageItemName, storageItemValue)
  , StorageName (StorageName, storageNameToText)
  , StorageValue (StorageValue, storageValueToText)
  )
import Playwright.StorageKind
  ( StorageKind (StorageLocal, StorageSession)
  )
import Playwright.StorageState
  ( OriginStorageState (OriginStorageState, originStateLocalStorage, originStateOrigin)
  , StorageState (StorageState, storageStateCookies, storageStateOrigins)
  , emptyStorageState
  )
import Playwright.StyleTag
  ( StyleTag (StyleTag, styleTagContent, styleTagUrl)
  , emptyStyleTag
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  , milliseconds
  , timeoutToDouble
  )
import Playwright.TimezoneId
  ( TimezoneId (TimezoneId, timezoneIdToText)
  )
import Playwright.Url
  ( Url
  , urlFromText
  , urlToText
  )
import Playwright.UrlGlob
  ( UrlGlob (UrlGlob)
  )
import Playwright.UserAgent
  ( UserAgent (UserAgent, userAgentToText)
  )
import Playwright.ViewportSize
  ( ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize, viewportHeight, viewportWidth)
  , ViewportWidth (ViewportWidth)
  )
import Playwright.WaitForState
  ( WaitForState (WaitForAttached, WaitForDetached, WaitForHidden, WaitForVisible)
  )
import Playwright.WebStorage
  ( WebStorage (WebStorage, webStorageKind, webStoragePage)
  , localStorage
  , sessionStorage
  , storageClear
  , storageGetItem
  , storageItems
  , storageRemoveItem
  , storageSetItem
  )
import Playwright.WheelDelta
  ( WheelDelta (WheelDelta)
  , wheelDeltaFromInt
  )
import Playwright.Worker
  ( Worker (Worker, workerChannel)
  , evaluateOnWorker
  , evaluateOnWorkerText
  , waitForWorker
  , workerUrl
  , workers
  )
