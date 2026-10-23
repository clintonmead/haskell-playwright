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
  , ClearCookiesOptions (..)
  , ClickOptions (..)
  , ScrollOption (..)
  , TapOptions (..)
  , HoverOptions (..)
  , DblclickOptions (..)
  , ClockMillis (..)
  , ClockTicks (..)
  , ClockTime (..)
  , ColorScheme (..)
  , Contrast (..)
  , ConsoleLocation (..)
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
  , DragAndDropOptions (..)
  , DropPayload (..)
  , ElementHandle (..)
  , ElementState (..)
  , EmulateMedia (..)
  , Exactness (..)
  , FileChooser (..)
  , fileChooserSetFiles
  , fileChoosersEqual
  , ForcedColors (..)
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
  , KeyboardModifier (..)
  , Latitude (..)
  , LaunchOptions (..)
  , LoadState (..)
  , Locale (..)
  , Locator (..)
  , LocatorHandlerTimes (..)
  , Longitude (..)
  , MediaType (..)
  , MimeType (..)
  , MouseButton (..)
  , NewContextOptions (..)
  , NewRequestOptions (..)
  , NetworkSizes (..)
  , OriginStorageState (..)
  , Page (..)
  , PageError (..)
  , PdfOptions (..)
  , WebError (..)
  , WebErrorLocation (..)
  , PermissionName (..)
  , Playwright
  , Point (..)
  , PropertyName (..)
  , ProxyHost (..)
  , ProxySettings (..)
  , RegisterSelectorOptions (..)
  , ReducedMotion (..)
  , Request (..)
  , RequestFailure (..)
  , RemoteAddr (..)
  , ResourceTiming (..)
  , ResourceType (..)
  , Response (..)
  , Route
  , RouteOptions (..)
  , RouteTimes (..)
  , ScreenshotAnimations (..)
  , ScreenshotCaret (..)
  , ScreenshotOptions (..)
  , ScreenshotScale (..)
  , ScreenshotType (..)
  , ScriptTag (..)
  , SecurityDetails (..)
  , SelectAttributes (..)
  , SelectOption (..)
  , SelectOptionOptions (..)
  , WaitForNavigationOptions (..)
  , defaultWaitForNavigationOptions
  , WaitForURLOptions (..)
  , defaultWaitForURLOptions
  , SetContentOptions (..)
  , defaultSetContentOptions
  , Selector
  , ServiceWorkersPolicy (..)
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
  , UrlMatch (..)
  , UserAgent (..)
  , ViewportHeight (..)
  , ViewportSize (..)
  , ViewportWidth (..)
  , WaitForState (..)
  , WebStorage (..)
  , WebSocket
  , WebSocketFrame (..)
  , WheelDelta (..)
  , Worker (..)
  , abortRoute
  , abortRouteWith
  , acceptDialog
  , acceptDialogWith
  , addContextInitScript
  , addContextInitScriptExposing
  , addCookies
  , addInitScript
  , addPageInitScriptExposing
  , addLocatorHandler
  , addLocatorHandlerWith
  , addScriptTag
  , addScriptTagOnFrame
  , addStyleTag
  , allInnerTexts
  , allLocators
  , allTextContents
  , andLocator
  , apiResponseBody
  , apiResponseDispose
  , apiResponseHeaders
  , apiResponseHeadersArray
  , apiResponseJson
  , apiResponseOk
  , apiResponseSecurityDetails
  , apiResponseServerAddr
  , apiResponseStatus
  , apiResponseStatusText
  , apiResponseText
  , apiResponseUrl
  , armConsole
  , armDialog
  , onDialog
  , onPageDialogClosed
  , armDownload
  , armFileChooser
  , armFrameDetached
  , armFrameNavigated
  , armLoadEvent
  , armDomContentLoadedEvent
  , armNavigatedFrame
  , armPopup
  , arrayValue
  , asElement
  , boolArgument
  , boolValue
  , blur
  , boundingBox
  , bringToFront
  , pageHideHighlight
  , browserBrowserType
  , browserContexts
  , browserFromRef
  , browsersEqual
  , browserTypeFromRef
  , browserTypesEqual
  , browserVersion
  , check
  , checkHandle
  , checkWith
  , channelArgument
  , chainLocator
  , childLocator
  , childFrameLocator
  , childFrames
  , contentFrame
  , frameLocatorOwner
  , chromium
  , clear
  , clearCookies
  , clearCookiesNamed
  , clearCookiesWith
  , clearGeolocation
  , clearPageErrors
  , clearPermissions
  , click
  , clickHandle
  , clickHandleWith
  , dblclickHandle
  , dblclickHandleWith
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
  , closePageReason
  , closePageWith
  , content
  , contentFrameHandle
  , BindingSource (BindingSource, bindingSourceContext, bindingSourceFrame, bindingSourcePage)
  , contextExposeBinding
  , contextExposeFunction
  , contextFromRef
  , contextBrowser
  , contextPages
  , contextRequest
  , contextRoute
  , contextRouteWith
  , continueRoute
  , continueRouteWith
  , cookieForHost
  , cookieForUrl
  , cookies
  , cookiesFor
  , contextsEqual
  , cssPixelFromDouble
  , cssPixelFromInt
  , currentName
  , parentFrame
  , currentUrl
  , isDetached
  , dblclick
  , defaultAddLocatorHandlerOptions
  , defaultCheckOptions
  , defaultClearCookiesOptions
  , defaultClickOptions
  , defaultTapOptions
  , defaultHoverOptions
  , defaultDblclickOptions
  , defaultContinueOptions
  , defaultDragAndDropOptions
  , defaultSelectOptionOptions
  , defaultEmulateMedia
  , defaultFetchOptions
  , defaultFulfillOptions
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , defaultNewRequestOptions
  , defaultPdfOptions
  , defaultRegisterSelectorOptions
  , defaultRouteOptions
  , defaultScreenshotOptions
  , defaultTimeout
  , describeLocator
  , deviceScaleFactorFromInt
  , dispatchEvent
  , dispatchEventHandle
  , dispatchEventHandleWith
  , dispatchEventWith
  , dispose
  , disposeHandle
  , downloadCancel
  , downloadDelete
  , downloadFailure
  , downloadPage
  , downloadPath
  , downloadSaveAs
  , downloadSuggestedFilename
  , downloadUrl
  , dragAndDrop
  , dragAndDropWith
  , dragTo
  , dragToWith
  , dropOnto
  , dismissDialog
  , dialogsEqual
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
  , evaluateOnFrameBool
  , evaluateOnFrameNumber
  , evaluateOnFrameText
  , evaluateOnFrameWith
  , evaluateOnHandle
  , evaluateOnHandleBool
  , evaluateOnHandleExpression
  , evaluateOnHandleExpressionNumbers
  , evaluateOnHandleNumber
  , evaluateOnHandleNumbers
  , evaluateOnHandleText
  , evaluateOnLocator
  , evaluateOnLocatorAllTexts
  , evaluateOnLocatorBool
  , evaluateOnLocatorNumber
  , evaluateOnLocatorText
  , evaluateOnLocatorWith
  , evaluateOnWorker
  , evaluateOnWorkerNumber
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
  , filterHas
  , filterHasNot
  , filterHasNotText
  , filterHasText
  , filterHasTextRegex
  , filterHasTextWith
  , filterVisible
  , firefox
  , first
  , focus
  , focusHandle
  , frameByName
  , frameByUrl
  , frameContent
  , frameElement
  , frameLocator
  , frames
  , framePage
  , framesEqual
  , fulfillRoute
  , getAttribute
  , getAttributeHandle
  , getByAltText
  , getByAltTextRegex
  , getByLabel
  , getByLabelRegex
  , getByPlaceholder
  , getByPlaceholderRegex
  , getByRole
  , getByTestId
  , getByTestIdAttr
  , getByTestIdAttrRegex
  , getByTestIdRegex
  , getByText
  , getByTextRegex
  , getByTitle
  , getByTitleRegex
  , getProperty
  , getPropertyList
  , goBack
  , goBackWith
  , goForward
  , goForwardWith
  , goto
  , gotoFrame
  , grantPermissions
  , grantPermissionsFor
  , hover
  , highlight
  , highlightWithStyle
  , hideHighlight
  , hoverHandle
  , pressHandle
  , httpStatusOk
  , innerHtml
  , innerHtmlHandle
  , innerText
  , innerTextHandle
  , innerTextWith
  , inputValue
  , inputValueHandle
  , isChecked
  , isCheckedHandle
  , isClosed
  , isContextClosed
  , isDisabled
  , isEditable
  , isEditableHandle
  , isDisabledHandle
  , isEnabledHandle
  , isHiddenHandle
  , isVisibleHandle
  , isEnabled
  , isHidden
  , isVisible
  , isWebSocketClosed
  , jsHandleArgument
  , jsHandleObjectArgument
  , jsHandleToString
  , jsonValue
  , jsonValueBool
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
  , launchPersistentContext
  , localStorage
  , locator
  , locatorBoundingBox
  , locatorCount
  , locatorDescription
  , locatorScreenshot
  , locatorScreenshotMasked
  , locatorOnFrame
  , locatorPage
  , locatorScrollIntoView
  , locatorSelectText
  , longitudeFromInt
  , mainFrame
  , milliseconds
  , mouseClick
  , mouseClickWith
  , mouseDblclick
  , mouseDown
  , mouseDownWith
  , mouseMove
  , mouseMoveWith
  , mouseUp
  , mouseUpWith
  , mouseWheel
  , nestedSelector
  , newBrowserPage
  , newBrowserPageWith
  , newContext
  , newContextWith
  , newContextForReuseWith
  , disconnectFromReusedContext
  , newPage
  , newRequestContext
  , newRequestContextWith
  , nth
  , numberArgument
  , numbersArgument
  , numberValue
  , objectArgument
  , objectHandleArgument
  , objectHandlesArgument
  , objectNumberArgument
  , objectSpecialArgument
  , objectTextArgument
  , objectValue
  , onLoad
  , onBrowserContext
  , onConsole
  , onContextClose
  , onContextPage
  , onContextPageClose
  , onContextPageLoad
  , onContextDialogClosed
  , onContextDownload
  , onContextFrameAttached
  , onContextFrameDetached
  , onContextFrameNavigated
  , onFrameAttached
  , onFrameDetached
  , onFrameNavigated
  , onContextWebError
  , onContextRequest
  , onContextRequestFailed
  , onContextRequestFinished
  , onContextResponse
  , onPageClose
  , onPageCrash
  , onRequest
  , onRequestFailed
  , onRequestFinished
  , onResponse
  , onServiceWorker
  , onWebSocket
  , onWebSocketClose
  , onWebSocketError
  , onWebSocketFrameReceived
  , onWebSocketFrameSent
  , onWorkerClose
  , opener
  , orLocator
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
  , pageTextContentWith
  , pageClickSelector
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
  , requestAllHeaders
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
  , requestServiceWorker
  , requestSizes
  , requestTiming
  , requestUrl
  , requests
  , requestsEqual
  , responseFrame
  , responseFinished
  , responseFromServiceWorker
  , responseAllHeaders
  , responseHeaderValue
  , responseHeaderValues
  , responseHeadersArray
  , responseBody
  , responseHeaders
  , responseJSON
  , responseOk
  , responseRequest
  , responseStatus
  , responseStatusText
  , responseHttpVersion
  , responseText
  , responseUrl
  , route
  , routeFetch
  , routeFetchWith
  , routeRequest
  , routeWith
  , screenshot
  , screenshotHandle
  , screenshotHandleWith
  , screenshotHandleMasked
  , screenshotMasked
  , scrollIntoViewIfNeeded
  , scrollIntoViewIfNeededWith
  , selectOption
  , selectOptionElements
  , selectOptionElementsWith
  , selectOptionHandle
  , selectOptionWith
  , selectText
  , selectTextWith
  , selectorFromText
  , selectorToText
  , serviceWorkers
  , sessionExpires
  , sessionStorage
  , setChecked
  , setCheckedHandle
  , setContent
  , setContentWith
  , setContentOnFrame
  , setContextDefaultNavigationTimeout
  , setContextDefaultTimeout
  , setContextExtraHTTPHeaders
  , setDefaultNavigationTimeout
  , setDefaultTimeout
  , setExtraHTTPHeaders
  , setGeolocation
  , setHTTPCredentials
  , setInputFiles
  , setInputFilesPaths
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
  , dblclickWith
  , hoverWith
  , tapWith
  , tapHandleWith
  , textArgument
  , textContent
  , textContentHandle
  , timeoutToDouble
  , title
  , touchscreenTap
  , typeHandle
  , typeText
  , uncheck
  , uncheckHandle
  , uncheckWith
  , undefinedArgument
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
  , waitForConsoleWhere
  , waitForContextClose
  , waitForContextConsole
  , armContextConsole
  , armContextDialog
  , waitForContextDialog
  , armContextPage
  , waitForContextPage
  , waitForContextRequestFinished
  , waitForContextRequestWhere
  , waitForContextResponseWhere
  , waitForCrash
  , waitForDialog
  , waitForDownload
  , waitForElementState
  , waitForElementStateWith
  , waitForFileChooser
  , waitForFileChooserWith
  , waitForFunction
  , waitForFunctionOnFrame
  , waitForFunctionPolling
  , waitForFunctionWith
  , waitForFunctionWithArg
  , waitForFunctionOnLocator
  , waitForFunctionOnLocatorWith
  , waitForFunctionOnLocatorWithArg
  , waitForDomContentLoadedEvent
  , waitForLoadEvent
  , waitForLoadState
  , waitForLoadStateWith
  , waitForLoadStateFrame
  , waitForURLFrame
  , waitForNavigation
  , waitForNavigationWith
  , waitForNavigationOptions
  , armNavigationOptions
  , waitForNavigationFrame
  , waitForNavigationFrameOptions
  , armNavigationFrame
  , waitForFrameNavigated
  , waitForFrameDetached
  , waitForNavigatedFrame
  , waitForPageError
  , waitForPopup
  , waitUntilJsHandlePreview
  , armRequest
  , armRequestWhere
  , armRequestWith
  , waitForRequest
  , armRequestFailed
  , waitForRequestFailed
  , waitForRequestWhere
  , waitForRequestFailedWith
  , waitForRequestFinished
  , armRequestFinished
  , waitForRequestWith
  , armResponse
  , armResponseWhere
  , armResponseWith
  , waitForResponse
  , waitForResponseWhere
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
  , waitForURLOptions
  , waitForWorker
  , armWorkerConsole
  , waitForWorkerConsole
  , waitForServiceWorker
  , waitForWebSocket
  , waitForWebSocketClose
  , waitForWebSocketError
  , waitForWebSocketFrameReceived
  , waitForWebSocketFrameSent
  , waitForWith
  , webkit
  , webSocketUrl
  , webSocketsEqual
  , wheelDeltaFromInt
  , withBrowser
  , withPage
  , withPageWith
  , runPlaywright
  , workerUrl
  , workers
  , workersEqual
  ) where

import Playwright.AcceptDownloads
  ( AcceptDownloads (AcceptDownloadsAccept, AcceptDownloadsDeny)
  )
import Playwright.APIRequest
  ( newRequestContext
  , newRequestContextWith
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
  , apiResponseSecurityDetails
  , apiResponseServerAddr
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
  , browserContexts
  , browserFromRef
  , browsersEqual
  , browserVersion
  , closeBrowser
  , contextBrowser
  , newBrowserPage
  , newBrowserPageWith
  , newContext
  , newContextForReuseWith
  , newContextWith
  , disconnectFromReusedContext
  , onBrowserContext
  , withPage
  , withPageWith
  )
import Playwright.ContextEvents
  ( WebError (WebError, webErrorPage, webErrorError, webErrorLocation)
  , WebErrorLocation (WebErrorLocation, webErrorLocationUrl, webErrorLocationLine, webErrorLocationColumn)
  , onContextDialogClosed
  , onContextDownload
  , onContextFrameAttached
  , onContextFrameDetached
  , onContextFrameNavigated
  , onContextPageClose
  , onContextPageLoad
  , onContextWebError
  , onFrameAttached
  , onFrameDetached
  , onFrameNavigated
  )
import Playwright.BrowserContext
  ( BrowserContext (BrowserContext, browserContextChannel)
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
  , contextPages
  , cookies
  , cookiesFor
  , contextsEqual
  , contextRequest
  , grantPermissions
  , grantPermissionsFor
  , isContextClosed
  , newPage
  , onContextClose
  , onContextPage
  , pageContext
  , setContextDefaultNavigationTimeout
  , setContextDefaultTimeout
  , setContextExtraHTTPHeaders
  , setGeolocation
  , setHTTPCredentials
  , setOffline
  , setStorageState
  , storageState
  , waitForContextClose
  , armContextPage
  , waitForContextPage
  )
import Playwright.BrowserName
  ( BrowserName (Chromium, Firefox, WebKit)
  )
import Playwright.BrowserType
  ( BrowserType (BrowserType, browserTypeChannel, browserTypeExecutablePath, browserTypeName)
  , browserBrowserType
  , browserTypeFromRef
  , browserTypesEqual
  , launch
  , launchPersistentContext
  , withBrowser
  )
import Playwright.CheckOptions
  ( CheckOptions (CheckOptions, checkOptionsForce, checkOptionsPosition, checkOptionsTimeout, checkOptionsTrial)
  , defaultCheckOptions
  )
import Playwright.ClearCookiesOptions
  ( ClearCookiesOptions (ClearCookiesOptions, clearCookiesDomain, clearCookiesName, clearCookiesNameRegex, clearCookiesPath)
  , defaultClearCookiesOptions
  )
import Playwright.DblclickOptions
  ( DblclickOptions (DblclickOptions, dblclickOptionsForce, dblclickOptionsModifiers, dblclickOptionsPosition, dblclickOptionsScroll, dblclickOptionsTimeout, dblclickOptionsTrial)
  , defaultDblclickOptions
  )
import Playwright.HoverOptions
  ( HoverOptions (HoverOptions, hoverOptionsForce, hoverOptionsModifiers, hoverOptionsPosition, hoverOptionsScroll, hoverOptionsTimeout, hoverOptionsTrial)
  , defaultHoverOptions
  )
import Playwright.TapOptions
  ( TapOptions (TapOptions, tapOptionsForce, tapOptionsModifiers, tapOptionsPosition, tapOptionsTimeout, tapOptionsTrial)
  , defaultTapOptions
  )
import Playwright.ScrollOption
  ( ScrollOption (ScrollAuto, ScrollNone)
  )
import Playwright.Touchscreen
  ( touchscreenTap
  )
import Playwright.ClickOptions
  ( ClickOptions (ClickOptions, clickOptionsButton, clickOptionsClickCount, clickOptionsForce, clickOptionsModifiers, clickOptionsNoWaitAfter, clickOptionsPosition, clickOptionsScroll, clickOptionsSteps, clickOptionsTimeout, clickOptionsTrial)
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
import Playwright.Contrast
  ( Contrast (ContrastMore, ContrastNoOverride, ContrastNoPreference)
  )
import Playwright.ConsoleMessage
  ( ConsoleLocation (ConsoleLocation, consoleLocationColumnNumber, consoleLocationLineNumber, consoleLocationUrl)
  , ConsoleMessage (ConsoleMessage, consoleMessageArgs, consoleMessageLocation, consoleMessagePage, consoleMessageText, consoleMessageTimestamp, consoleMessageType, consoleMessageWorkerRef)
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
  , cookieForHost
  , cookieForUrl
  , sessionExpires
  )
import Playwright.CookieSameSite
  ( CookieSameSite (CookieSameSiteLax, CookieSameSiteNone, CookieSameSiteStrict)
  )
import Playwright.CssPixel
  ( CssPixel (CssPixel, cssPixelToScientific)
  , cssPixelFromDouble
  , cssPixelFromInt
  )
import Playwright.DeviceScaleFactor
  ( DeviceScaleFactor (DeviceScaleFactor, deviceScaleFactorToScientific)
  , deviceScaleFactorFromInt
  )
import Playwright.Dialog
  ( Dialog (Dialog, dialogChannel, dialogDefaultValue, dialogMessage, dialogPage, dialogType)
  , acceptDialog
  , acceptDialogWith
  , dialogsEqual
  , dismissDialog
  )
import Playwright.DialogType
  ( DialogType (DialogAlert, DialogBeforeUnload, DialogConfirm, DialogPrompt)
  )
import Playwright.Download
  ( Download
  , SuggestedFilename (SuggestedFilename, suggestedFilenameToText)
  , armDownload
  , downloadCancel
  , downloadDelete
  , downloadFailure
  , downloadPage
  , downloadPath
  , downloadSaveAs
  , downloadSuggestedFilename
  , downloadUrl
  , waitForDownload
  )
import Playwright.DragAndDropOptions
  ( DragAndDropOptions (DragAndDropOptions, dragAndDropOptionsForce, dragAndDropOptionsSourcePosition, dragAndDropOptionsSteps, dragAndDropOptionsTargetPosition, dragAndDropOptionsTimeout, dragAndDropOptionsTrial)
  , defaultDragAndDropOptions
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
  , dblclickHandle
  , dblclickHandleWith
  , contentFrameHandle
  , dispatchEventHandle
  , dispatchEventHandleWith
  , elementArgument
  , getAttributeHandle
  , innerHtmlHandle
  , innerTextHandle
  , inputValueHandle
  , isCheckedHandle
  , isEditableHandle
  , isDisabledHandle
  , isEnabledHandle
  , isHiddenHandle
  , isVisibleHandle
  , ownerFrameHandle
  , scrollIntoViewIfNeeded
  , scrollIntoViewIfNeededWith
  , screenshotHandle
  , screenshotHandleWith
  , selectText
  , selectTextWith
  , setInputFilesHandle
  , tapHandle
  , tapHandleWith
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
  , pressHandle
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
  ( EmulateMedia (EmulateMedia, emulateMediaColorScheme, emulateMediaContrast, emulateMediaForcedColors, emulateMediaReducedMotion, emulateMediaType)
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
  , evaluateOnFrameBool
  , evaluateOnFrameNumber
  , evaluateOnFrameText
  , evaluateOnFrameWith
  , evaluateOnHandle
  , evaluateOnHandleBool
  , evaluateOnHandleExpression
  , evaluateOnHandleExpressionNumbers
  , evaluateOnHandleNumber
  , evaluateOnHandleNumbers
  , evaluateOnHandleText
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
  ( BindingSource (BindingSource, bindingSourceContext, bindingSourceFrame, bindingSourcePage)
  , contextExposeBinding
  , contextExposeFunction
  , exposeFunction
  )
import Playwright.InitScript
  ( addContextInitScriptExposing
  , addPageInitScriptExposing
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
  ( FileChooser (FileChooser, fileChooserElement, fileChooserMultiple, fileChooserPage)
  , armFileChooser
  , fileChoosersEqual
  , fileChooserSetFiles
  , waitForFileChooser
  , waitForFileChooserWith
  )
import Playwright.ForcedColors
  ( ForcedColors (ForcedColorsActive, ForcedColorsNoOverride, ForcedColorsNone)
  )
import Playwright.Frame
  ( Frame (Frame, frameChannel, frameParent, frameUrl)
  , childFrames
  , frameContent
  , currentName
  , parentFrame
  , currentUrl
  , isDetached
  , framesEqual
  , gotoFrame
  , setContentOnFrame
  )
import Playwright.Frame.Inspect
  ( frameElement
  )
import Playwright.Frame.Wait
  ( waitForLoadStateFrame
  , waitForSelectorHandleOnFrame
  , waitForURLFrame
  )
import Playwright.FulfillOptions
  ( ContentType (ContentType, contentTypeToText)
  , FulfillBody (FulfillBytes, FulfillText)
  , FulfillOptions (FulfillOptions, fulfillBody, fulfillContentType, fulfillHeaders, fulfillPath, fulfillResponse, fulfillStatus)
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
  ( GetByRoleOptions (GetByRoleOptions, getByRoleChecked, getByRoleDisabled, getByRoleExact, getByRoleExpanded, getByRoleIncludeHidden, getByRoleLevel, getByRoleName, getByRoleNameRegex, getByRolePressed, getByRoleSelected)
  , defaultGetByRoleOptions
  )
import Playwright.AddLocatorHandlerOptions
  ( AddLocatorHandlerOptions (AddLocatorHandlerOptions, addLocatorHandlerNoWaitAfter, addLocatorHandlerTimes)
  , LocatorHandlerTimes (LocatorHandlerTimes, locatorHandlerTimesToInt)
  , defaultAddLocatorHandlerOptions
  )
import Playwright.ReducedMotion
  ( ReducedMotion (ReducedMotionNoOverride, ReducedMotionNoPreference, ReducedMotionReduce)
  )
import Playwright.RegisterSelectorOptions
  ( RegisterSelectorOptions (RegisterSelectorOptions, registerSelectorContentScript)
  , defaultRegisterSelectorOptions
  )
import Playwright.RemoteAddr
  ( RemoteAddr (RemoteAddr, remoteAddrIpAddress, remoteAddrPort)
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
  ( chromium
  , firefox
  , runPlaywright
  , webkit
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.JsHandle
  ( JsHandle (JsHandle)
  , asElement
  , getProperty
  , getPropertyList
  , jsHandleArgument
  , jsHandleObjectArgument
  , jsHandleToString
  , jsonValue
  , jsonValueBool
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
import Playwright.KeyboardModifier
  ( KeyboardModifier (KeyboardModifierAlt, KeyboardModifierControl, KeyboardModifierControlOrMeta, KeyboardModifierMeta, KeyboardModifierShift)
  )
import Playwright.LaunchOptions
  ( LaunchOptions (LaunchOptions, launchOptionsArgs, launchOptionsChromiumSandbox, launchOptionsDownloadsPath, launchOptionsExecutablePath, launchOptionsHeadless, launchOptionsIgnoreDefaultArgs, launchOptionsProxy, launchOptionsSlowMo, launchOptionsTimeout)
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
  , andLocator
  , blur
  , chainLocator
  , check
  , checkWith
  , childFrameLocator
  , childLocator
  , clear
  , contentFrame
  , frameLocatorOwner
  , click
  , clickWith
  , describeLocator
  , dblclick
  , dispatchEvent
  , dispatchEventWith
  , dragAndDrop
  , dragAndDropWith
  , dragTo
  , dragToWith
  , dropOnto
  , elementHandle
  , elementHandles
  , fill
  , filterHas
  , filterHasNot
  , filterHasNotText
  , filterHasText
  , filterHasTextRegex
  , filterHasTextWith
  , filterVisible
  , first
  , focus
  , getAttribute
  , hover
  , highlight
  , highlightWithStyle
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
  , locatorScreenshot
  , locatorScreenshotMasked
  , screenshotHandleMasked
  , locatorScrollIntoView
  , locatorSelectText
  , nth
  , orLocator
  , press
  , pressSequentially
  , selectOption
  , selectOptionElements
  , selectOptionElementsWith
  , selectOptionWith
  , setChecked
  , setInputFiles
  , setInputFilesPaths
  , tap
  , tapWith
  , hoverWith
  , dblclickWith
  , textContent
  , typeText
  , uncheck
  , uncheckWith
  , waitFor
  , waitForFunctionOnLocator
  , waitForFunctionOnLocatorWith
  , waitForFunctionOnLocatorWithArg
  , waitForWith
  )
import Playwright.LocatorHandler
  ( addLocatorHandler
  , addLocatorHandlerWith
  , removeLocatorHandler
  )
import Playwright.Mouse
  ( mouseClick
  , mouseClickWith
  , mouseDblclick
  , mouseDown
  , mouseDownWith
  , mouseMove
  , mouseMoveWith
  , mouseUp
  , mouseUpWith
  , mouseWheel
  )
import Playwright.MouseButton
  ( MouseButton (MouseButtonLeft, MouseButtonMiddle, MouseButtonRight)
  )
import Playwright.MediaType
  ( MediaType (MediaNoOverride, MediaPrint, MediaScreen)
  )
import Playwright.MimeType
  ( MimeType (MimeType, mimeTypeToText)
  )
import Playwright.NewContextOptions
  ( NewContextOptions (NewContextOptions, newContextAcceptDownloads, newContextBaseURL, newContextBypassCSP, newContextColorScheme, newContextContrast, newContextDeviceScaleFactor, newContextExtraHTTPHeaders, newContextForcedColors, newContextGeolocation, newContextHasTouch, newContextHttpCredentials, newContextIgnoreHTTPSErrors, newContextIsMobile, newContextJavaScriptEnabled, newContextLocale, newContextNoDefaultViewport, newContextOffline, newContextPermissions, newContextProxy, newContextReducedMotion, newContextScreen, newContextServiceWorkers, newContextStorageState, newContextStrictSelectors, newContextTimezoneId, newContextUserAgent, newContextViewport)
  , defaultNewContextOptions
  )
import Playwright.NewRequestOptions
  ( NewRequestOptions (NewRequestOptions, newRequestBaseURL, newRequestExtraHTTPHeaders, newRequestFailOnStatusCode, newRequestHttpCredentials, newRequestHttpCredentialsList, newRequestIgnoreHTTPSErrors, newRequestMaxRedirects, newRequestProxy, newRequestStorageState, newRequestTimeout, newRequestUserAgent)
  , defaultNewRequestOptions
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
  , addScriptTagOnFrame
  , addStyleTag
  , bringToFront
  , pageHideHighlight
  , closePage
  , closePageReason
  , closePageWith
  , content
  , getByAltText
  , getByAltTextRegex
  , getByLabel
  , getByLabelRegex
  , getByPlaceholder
  , getByPlaceholderRegex
  , getByRole
  , getByTestId
  , getByTestIdAttr
  , getByTestIdAttrRegex
  , getByTestIdRegex
  , getByText
  , getByTextRegex
  , getByTitle
  , getByTitleRegex
  , goBack
  , goBackWith
  , goForward
  , goForwardWith
  , goto
  , isClosed
  , locator
  , locatorOnFrame
  , locatorPage
  , frameLocator
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
  , screenshotMasked
  , setContent
  , setContentWith
  , setDefaultNavigationTimeout
  , setDefaultTimeout
  , setExtraHTTPHeaders
  , setViewportSize
  , title
  , url
  , waitForClose
  , waitForCrash
  , onPageClose
  , onPageCrash
  , frameByName
  , frameByUrl
  , frames
  , framePage
  , mainFrame
  )
import Playwright.Page.Wait
  ( armConsole
  , armDialog
  , onDialog
  , onPageDialogClosed
  , armFrameDetached
  , armFrameNavigated
  , armLoadEvent
  , armDomContentLoadedEvent
  , armNavigatedFrame
  , armPopup
  , onLoad
  , onConsole
  , waitForConsole
  , waitForConsoleWhere
  , waitForDialog
  , waitForFunction
  , waitForFunctionOnFrame
  , waitForFunctionPolling
  , waitForFunctionWith
  , waitForFunctionWithArg
  , waitForDomContentLoadedEvent
  , waitForLoadEvent
  , waitForLoadState
  , waitForLoadStateWith
  , waitForNavigation
  , waitForNavigationWith
  , waitForNavigationOptions
  , armNavigationOptions
  , waitForNavigationFrame
  , waitForNavigationFrameOptions
  , armNavigationFrame
  , waitForFrameNavigated
  , waitForFrameDetached
  , waitForNavigatedFrame
  , waitForPopup
  , armRequest
  , armRequestWhere
  , armRequestWith
  , waitForRequest
  , armRequestFailed
  , waitForRequestFailed
  , waitForRequestWhere
  , waitForRequestFailedWith
  , waitForRequestFinished
  , armRequestFinished
  , waitForRequestWith
  , armResponse
  , armResponseWhere
  , armResponseWith
  , waitForResponse
  , waitForResponseWhere
  , waitForResponseWith
  , waitForSelector
  , waitForSelectorHandle
  , waitForSelectorHandleWith
  , waitForSelectorStrict
  , waitForTimeout
  , waitForURL
  , waitForURLWith
  , waitForURLOptions
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
  , pageTextContentWith
  , pageClickSelector
  )
import Playwright.Page.Network
  ( onRequest
  , onRequestFailed
  , onRequestFinished
  , onResponse
  , requests
  , responseFinished
  )
import Playwright.BrowserContext.Network
  ( onContextRequest
  , onContextRequestFailed
  , onContextRequestFinished
  , onContextResponse
  , waitForContextRequestFinished
  , waitForContextRequestWhere
  , waitForContextResponseWhere
  )
import Playwright.BrowserContext.Wait
  ( waitForContextConsole
  , armContextConsole
  , armContextDialog
  , waitForContextDialog
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
import Playwright.ProxySettings
  ( ProxyHost (ProxyHost, proxyHostToText)
  , ProxySettings (ProxySettings, proxySettingsBypass, proxySettingsPassword, proxySettingsServer, proxySettingsUsername)
  )
import Playwright.Request
  ( Request (Request, requestChannel)
  , requestAllHeaders
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
  , requestTiming
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
import Playwright.RequestServiceWorker
  ( requestServiceWorker
  )
import Playwright.ResourceTiming
  ( ResourceTiming (ResourceTiming, resourceTimingConnectEnd, resourceTimingConnectStart, resourceTimingDomainLookupEnd, resourceTimingDomainLookupStart, resourceTimingRequestStart, resourceTimingResponseEnd, resourceTimingResponseStart, resourceTimingSecureConnectionStart, resourceTimingStartTime)
  )
import Playwright.ResourceType
  ( ResourceType (ResourceType, resourceTypeToText)
  )
import Playwright.Response
  ( Response (Response, responseChannel)
  , responseAllHeaders
  , responseHeaderValue
  , responseHeaderValues
  , responseHeadersArray
  , responseBody
  , responseHeaders
  , responseJSON
  , responseOk
  , requestResponse
  , responseRequest
  , responseFromServiceWorker
  , responseStatus
  , responseStatusText
  , responseHttpVersion
  , responseText
  , responseUrl
  )
import Playwright.Route
  ( Route
  , abortRoute
  , abortRouteWith
  , continueRoute
  , continueRouteWith
  , fallbackRoute
  , fallbackRouteWith
  , fulfillRoute
  , routeFetch
  , routeFetchWith
  , routeRequest
  )
import Playwright.Route.Intercept
  ( contextRoute
  , contextRouteWith
  , route
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
  ( ScreenshotAnimations
      ( ScreenshotAnimationsAllow
      , ScreenshotAnimationsDisabled
      )
  , ScreenshotCaret
      ( ScreenshotCaretHide
      , ScreenshotCaretInitial
      )
  , ScreenshotOptions
      ( ScreenshotOptions
      , screenshotOptionsAnimations
      , screenshotOptionsCaret
      , screenshotOptionsClip
      , screenshotOptionsFullPage
      , screenshotOptionsMaskColor
      , screenshotOptionsOmitBackground
      , screenshotOptionsPath
      , screenshotOptionsQuality
      , screenshotOptionsScale
      , screenshotOptionsStyle
      , screenshotOptionsTimeout
      , screenshotOptionsType
      )
  , ScreenshotScale
      ( ScreenshotScaleCss
      , ScreenshotScaleDevice
      )
  , ScreenshotType
      ( ScreenshotJpeg
      , ScreenshotPng
      , ScreenshotWebp
      )
  , defaultScreenshotOptions
  )
import Playwright.ScriptTag
  ( ScriptTag (ScriptTag, scriptTagContent, scriptTagPath, scriptTagType, scriptTagUrl)
  , emptyScriptTag
  )
import Playwright.SecurityDetails
  ( SecurityDetails (SecurityDetails, securityDetailsIssuer, securityDetailsProtocol, securityDetailsSubjectName, securityDetailsValidFrom, securityDetailsValidTo)
  )
import Playwright.SelectOption
  ( SelectAttributes (SelectAttributes, selectAttributesIndex, selectAttributesLabel, selectAttributesValue)
  , SelectOption (SelectIndex, SelectLabel, SelectMatching, SelectValue, SelectValueOrLabel)
  )
import Playwright.WaitForNavigationOptions
  ( WaitForNavigationOptions (WaitForNavigationOptions, waitForNavigationOptionsTimeout, waitForNavigationOptionsUrl, waitForNavigationOptionsWaitUntil)
  , defaultWaitForNavigationOptions
  )
import Playwright.WaitForURLOptions
  ( WaitForURLOptions (WaitForURLOptions, waitForURLOptionsTimeout, waitForURLOptionsWaitUntil)
  , defaultWaitForURLOptions
  )
import Playwright.SetContentOptions
  ( SetContentOptions (SetContentOptions, setContentOptionsTimeout, setContentOptionsWaitUntil)
  , defaultSetContentOptions
  )
import Playwright.SelectOptionOptions
  ( SelectOptionOptions (SelectOptionOptions, selectOptionOptionsForce, selectOptionOptionsTimeout)
  , defaultSelectOptionOptions
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
import Playwright.ServiceWorkersPolicy
  ( ServiceWorkersPolicy (ServiceWorkersAllow, ServiceWorkersBlock)
  )
import Playwright.SerializedArgument
  ( boolArgument
  , channelArgument
  , numberArgument
  , numbersArgument
  , objectArgument
  , objectHandleArgument
  , objectHandlesArgument
  , objectNumberArgument
  , objectSpecialArgument
  , objectTextArgument
  , specialArgument
  , textArgument
  , undefinedArgument
  )
import Playwright.SerializedValue
  ( arrayValue
  , boolValue
  , numberValue
  , objectValue
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
  ( StyleTag (StyleTag, styleTagContent, styleTagPath, styleTagUrl)
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
import Playwright.UrlMatch
  ( UrlMatch (UrlMatchGlob, UrlMatchPredicate, UrlMatchRegex)
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
import Playwright.WebSocket
  ( WebSocket
  , isWebSocketClosed
  , onWebSocket
  , onWebSocketClose
  , onWebSocketError
  , onWebSocketFrameReceived
  , onWebSocketFrameSent
  , waitForWebSocket
  , waitForWebSocketClose
  , waitForWebSocketError
  , waitForWebSocketFrameReceived
  , waitForWebSocketFrameSent
  , webSocketUrl
  , webSocketsEqual
  )
import Playwright.WebSocketFrame
  ( WebSocketFrame (WebSocketBinary, WebSocketText)
  )
import Playwright.WheelDelta
  ( WheelDelta (WheelDelta)
  , wheelDeltaFromInt
  )
import Playwright.Worker
  ( Worker (Worker, workerChannel)
  , evaluateOnWorker
  , evaluateOnWorkerNumber
  , evaluateOnWorkerText
  , onServiceWorker
  , onWorkerClose
  , serviceWorkers
  , waitForServiceWorker
  , waitForWorker
  , armWorkerConsole
  , waitForWorkerConsole
  , workerUrl
  , workers
  , workersEqual
  )
