module Playwright
  ( Browser (..)
  , BrowserContext (..)
  , BrowserName (..)
  , BrowserType (..)
  , AriaRole (..)
  , BoundingBox (..)
  , BindingName (..)
  , CheckOptions (..)
  , ColorScheme (..)
  , ConsoleMessage (..)
  , ConsoleType (..)
  , ContentType (..)
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
  , Dialog (..)
  , DialogType (..)
  , Disposable (..)
  , DomEventType (..)
  , ElementHandle (..)
  , EmulateMedia (..)
  , Exactness (..)
  , FileChooser (..)
  , Frame (..)
  , FulfillBody (..)
  , FulfillOptions (..)
  , GetByRoleOptions (..)
  , GotoOptions (..)
  , HeaderName (..)
  , HeaderValue (..)
  , HttpHeader (..)
  , HttpMethod (..)
  , HttpStatus (..)
  , InputFile (..)
  , InputFileName (..)
  , JsHandle (..)
  , Key (..)
  , LaunchOptions (..)
  , LoadState (..)
  , Locator (..)
  , MediaType (..)
  , Page (..)
  , PageError (..)
  , Playwright (..)
  , Point (..)
  , PropertyName (..)
  , Request (..)
  , ResourceType (..)
  , Response (..)
  , Route (..)
  , ScreenshotOptions (..)
  , ScriptTag (..)
  , SelectOption (..)
  , Selector
  , SpecialJsValue (..)
  , StorageItem (..)
  , StorageKind (..)
  , StorageName (..)
  , StorageValue (..)
  , StyleTag (..)
  , Timeout
  , Url
  , UrlGlob (..)
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
  , addCookies
  , addInitScript
  , addScriptTag
  , addStyleTag
  , allLocators
  , armConsole
  , armDialog
  , armFileChooser
  , armPopup
  , asElement
  , boolArgument
  , boundingBox
  , browserFromRef
  , browserTypeFromRef
  , check
  , checkWith
  , channelArgument
  , childLocator
  , childFrames
  , chromium
  , clear
  , clearCookies
  , clearCookiesNamed
  , clearPageErrors
  , click
  , clickHandle
  , closeBrowser
  , closeContext
  , closePage
  , content
  , contentFrameHandle
  , contextFromRef
  , continueRoute
  , cookieForUrl
  , cookies
  , cookiesFor
  , cssPixelFromInt
  , currentName
  , currentUrl
  , dblclick
  , defaultCheckOptions
  , defaultEmulateMedia
  , defaultFulfillOptions
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultScreenshotOptions
  , defaultTimeout
  , dispatchEvent
  , dispose
  , dragAndDrop
  , dismissDialog
  , elementArgument
  , elementHandle
  , elementHandles
  , emptyScriptTag
  , emptyStyleTag
  , emulateMedia
  , evaluate
  , evaluateBool
  , evaluateBoolWith
  , evaluateHandle
  , evaluateHandleOnFrame
  , evaluateHandleWith
  , evaluateNumber
  , evaluateNumberWith
  , evaluateNumbersWith
  , evaluateOnElement
  , evaluateOnElementNumber
  , evaluateOnElementText
  , evaluateOnFrame
  , evaluateOnFrameText
  , evaluateOnHandle
  , evaluateOnHandleExpression
  , evaluateOnHandleExpressionNumbers
  , evaluateOnHandleNumbers
  , evaluateOnLocator
  , evaluateOnLocatorAllTexts
  , evaluateOnLocatorBool
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
  , fill
  , firefox
  , first
  , focus
  , frameByName
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
  , hover
  , httpStatusOk
  , initializePlaywright
  , innerHtml
  , innerHtmlHandle
  , innerText
  , innerTextHandle
  , inputValue
  , inputValueHandle
  , isChecked
  , isDisabled
  , isEditable
  , isEnabled
  , isHidden
  , isVisible
  , jsHandleArgument
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
  , launch
  , localStorage
  , locator
  , locatorCount
  , mainFrame
  , milliseconds
  , mouseClick
  , mouseDown
  , mouseMove
  , mouseUp
  , mouseWheel
  , nestedSelector
  , newContext
  , newPage
  , nth
  , numberArgument
  , numbersArgument
  , objectNumberArgument
  , objectSpecialArgument
  , objectTextArgument
  , opener
  , ownerFrameHandle
  , pageContext
  , pageErrors
  , pageFromRef
  , pagesEqual
  , press
  , querySelector
  , querySelectorStrict
  , reload
  , requestFrame
  , requestHeaders
  , requestIsNavigation
  , requestMethod
  , requestPostData
  , requestResourceType
  , requestUrl
  , responseHeaders
  , responseOk
  , responseRequest
  , responseStatus
  , responseText
  , responseUrl
  , route
  , routeRequest
  , screenshot
  , scrollIntoViewIfNeeded
  , scrollIntoViewIfNeededWith
  , selectOption
  , selectText
  , selectTextWith
  , selectorFromText
  , selectorToText
  , sessionExpires
  , sessionStorage
  , setChecked
  , setContent
  , setExtraHTTPHeaders
  , setInputFiles
  , setInputFilesHandle
  , setViewportSize
  , specialArgument
  , storageClear
  , storageGetItem
  , storageItems
  , storageRemoveItem
  , storageSetItem
  , textArgument
  , textContent
  , textContentHandle
  , timeoutToDouble
  , title
  , typeHandle
  , typeText
  , uncheck
  , uncheckWith
  , url
  , urlFromText
  , urlToText
  , waitFor
  , waitForConsole
  , waitForDialog
  , waitForFileChooser
  , waitForFunction
  , waitForLoadState
  , waitForNavigation
  , waitForNavigationWith
  , waitForPageError
  , waitForPopup
  , waitForRequest
  , waitForRequestWith
  , waitForResponse
  , waitForResponseWith
  , waitForSelector
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
  , withPlaywright
  , workerUrl
  , workers
  ) where

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
  , withPage
  )
import Playwright.BrowserContext
  ( BrowserContext (BrowserContext, browserContextChannel)
  , addCookies
  , clearCookies
  , clearCookiesNamed
  , closeContext
  , contextFromRef
  , cookies
  , cookiesFor
  , newPage
  , pageContext
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
import Playwright.ColorScheme
  ( ColorScheme (ColorSchemeDark, ColorSchemeLight, ColorSchemeNoOverride, ColorSchemeNoPreference)
  )
import Playwright.ConsoleMessage
  ( ConsoleMessage (ConsoleMessage, consoleMessageArgs, consoleMessageText, consoleMessageType)
  )
import Playwright.ConsoleType
  ( ConsoleType (ConsoleAssert, ConsoleClear, ConsoleCount, ConsoleDebug, ConsoleDir, ConsoleDirXml, ConsoleError, ConsoleInfo, ConsoleLog, ConsoleProfile, ConsoleProfileEnd, ConsoleTable, ConsoleTimeEnd, ConsoleTrace, ConsoleVerbose, ConsoleWarning)
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
import Playwright.Dialog
  ( Dialog (Dialog, dialogChannel, dialogDefaultValue, dialogMessage, dialogType)
  , acceptDialog
  , acceptDialogWith
  , dismissDialog
  )
import Playwright.DialogType
  ( DialogType (DialogAlert, DialogBeforeUnload, DialogConfirm, DialogPrompt)
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
  , contentFrameHandle
  , elementArgument
  , getAttributeHandle
  , innerHtmlHandle
  , innerTextHandle
  , inputValueHandle
  , ownerFrameHandle
  , scrollIntoViewIfNeeded
  , scrollIntoViewIfNeededWith
  , selectText
  , selectTextWith
  , setInputFilesHandle
  , textContentHandle
  , typeHandle
  )
import Playwright.EmulateMedia
  ( EmulateMedia (EmulateMedia, emulateMediaColorScheme, emulateMediaType)
  , defaultEmulateMedia
  , emulateMedia
  )
import Playwright.Evaluate
  ( evaluate
  , evaluateBool
  , evaluateBoolWith
  , evaluateHandle
  , evaluateHandleOnFrame
  , evaluateHandleWith
  , evaluateNumber
  , evaluateNumberWith
  , evaluateNumbersWith
  , evaluateOnElement
  , evaluateOnElementNumber
  , evaluateOnElementText
  , evaluateOnFrame
  , evaluateOnFrameText
  , evaluateOnHandle
  , evaluateOnHandleExpression
  , evaluateOnHandleExpressionNumbers
  , evaluateOnHandleNumbers
  , evaluateOnLocator
  , evaluateOnLocatorAllTexts
  , evaluateOnLocatorBool
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
  ( exposeFunction
  )
import Playwright.FileChooser
  ( FileChooser (FileChooser, fileChooserElement, fileChooserMultiple)
  , armFileChooser
  , waitForFileChooser
  )
import Playwright.Frame
  ( Frame (Frame, frameChannel, frameParent, frameUrl)
  , childFrames
  , currentName
  , currentUrl
  , framesEqual
  )
import Playwright.FulfillOptions
  ( ContentType (ContentType, contentTypeToText)
  , FulfillBody (FulfillBytes, FulfillText)
  , FulfillOptions (FulfillOptions, fulfillBody, fulfillContentType, fulfillHeaders, fulfillStatus)
  , defaultFulfillOptions
  )
import Playwright.GetByRoleOptions
  ( GetByRoleOptions (GetByRoleOptions, getByRoleExact, getByRoleName)
  , defaultGetByRoleOptions
  )
import Playwright.GotoOptions
  ( GotoOptions (GotoOptions, gotoOptionsReferer, gotoOptionsTimeout, gotoOptionsWaitUntil)
  , defaultGotoOptions
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
  , jsonValue
  , jsonValueNumber
  , jsonValueSpecial
  , jsonValueText
  , jsonValueTexts
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
import Playwright.Locator
  ( Locator (Locator, locatorFrame, locatorSelector)
  , allLocators
  , check
  , checkWith
  , childLocator
  , clear
  , click
  , dblclick
  , dispatchEvent
  , dragAndDrop
  , elementHandle
  , elementHandles
  , fill
  , first
  , focus
  , getAttribute
  , hover
  , innerHtml
  , innerText
  , inputValue
  , isChecked
  , isDisabled
  , isEditable
  , isEnabled
  , isHidden
  , isVisible
  , lastLocator
  , locatorCount
  , nth
  , press
  , selectOption
  , setChecked
  , setInputFiles
  , textContent
  , typeText
  , uncheck
  , uncheckWith
  , waitFor
  , waitForWith
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
import Playwright.Page
  ( Page (Page, pageChannel, pageContextChannel, pageMainFrame)
  , addInitScript
  , addScriptTag
  , addStyleTag
  , armConsole
  , armDialog
  , armPopup
  , closePage
  , content
  , getByLabel
  , getByRole
  , getByTestId
  , getByText
  , goBack
  , goForward
  , goto
  , locator
  , opener
  , pageFromRef
  , pagesEqual
  , querySelector
  , querySelectorStrict
  , reload
  , screenshot
  , setContent
  , setExtraHTTPHeaders
  , setViewportSize
  , title
  , url
  , waitForConsole
  , waitForDialog
  , waitForFunction
  , waitForLoadState
  , waitForNavigation
  , waitForNavigationWith
  , waitForPopup
  , waitForRequest
  , waitForRequestWith
  , waitForResponse
  , waitForResponseWith
  , waitForSelector
  , waitForSelectorStrict
  , waitForTimeout
  , waitForURL
  , waitForURLWith
  , frameByName
  , frames
  , mainFrame
  )
import Playwright.PageError
  ( PageError (PageError, pageErrorMessage, pageErrorName, pageErrorStack)
  , clearPageErrors
  , pageErrors
  , waitForPageError
  )
import Playwright.Point
  ( Point (Point)
  )
import Playwright.PropertyName
  ( PropertyName (PropertyName, propertyNameToText)
  )
import Playwright.Request
  ( Request (Request, requestChannel)
  , requestHeaders
  , requestIsNavigation
  , requestMethod
  , requestPostData
  , requestResourceType
  , requestUrl
  )
import Playwright.RequestFrame
  ( requestFrame
  )
import Playwright.ResourceType
  ( ResourceType (ResourceType, resourceTypeToText)
  )
import Playwright.Response
  ( Response (Response, responseChannel)
  , responseHeaders
  , responseOk
  , responseRequest
  , responseStatus
  , responseText
  , responseUrl
  )
import Playwright.Route
  ( Route (Route, routeChannel)
  , abortRoute
  , continueRoute
  , fallbackRoute
  , fulfillRoute
  , route
  , routeRequest
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
  , nestedSelector
  , selectorFromText
  , selectorToText
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
import Playwright.Url
  ( Url
  , urlFromText
  , urlToText
  )
import Playwright.UrlGlob
  ( UrlGlob (UrlGlob)
  )
import Playwright.ViewportSize
  ( ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
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
