module Main
  ( main
  ) where

import Playwright.BeforeunloadSpec qualified as BeforeunloadSpec
import Playwright.BrowserSpec qualified as BrowserSpec
import Playwright.BrowserContextAddCookiesSpec qualified as BrowserContextAddCookiesSpec
import Playwright.BrowserContextAddInitScriptSpec qualified as BrowserContextAddInitScriptSpec
import Playwright.BrowserContextBaseUrlSpec qualified as BrowserContextBaseUrlSpec
import Playwright.BrowserContextBasicSpec qualified as BrowserContextBasicSpec
import Playwright.BrowserContextClearCookiesSpec qualified as BrowserContextClearCookiesSpec
import Playwright.BrowserContextCookiesSpec qualified as BrowserContextCookiesSpec
import Playwright.BrowserContextCredentialsSpec qualified as BrowserContextCredentialsSpec
import Playwright.BrowserContextCspSpec qualified as BrowserContextCspSpec
import Playwright.BrowserContextDeviceSpec qualified as BrowserContextDeviceSpec
import Playwright.BrowserContextDsfSpec qualified as BrowserContextDsfSpec
import Playwright.BrowserContextEventsSpec qualified as BrowserContextEventsSpec
import Playwright.BrowserContextExposeFunctionSpec qualified as BrowserContextExposeFunctionSpec
import Playwright.BrowserContextFetchSpec qualified as BrowserContextFetchSpec
import Playwright.BrowserContextJavaScriptSpec qualified as BrowserContextJavaScriptSpec
import Playwright.BrowserContextLocaleSpec qualified as BrowserContextLocaleSpec
import Playwright.BrowserContextNetworkEventSpec qualified as BrowserContextNetworkEventSpec
import Playwright.BrowserContextOfflineSpec qualified as BrowserContextOfflineSpec
import Playwright.BrowserContextPageEventSpec qualified as BrowserContextPageEventSpec
import Playwright.BrowserContextPagesSpec qualified as BrowserContextPagesSpec
import Playwright.BrowserContextProxySpec qualified as BrowserContextProxySpec
import Playwright.BrowserContextRouteSpec qualified as BrowserContextRouteSpec
import Playwright.BrowserContextServiceWorkerPolicySpec qualified as BrowserContextServiceWorkerPolicySpec
import Playwright.BrowserContextSetExtraHTTPHeadersSpec qualified as BrowserContextSetExtraHTTPHeadersSpec
import Playwright.BrowserContextStorageStateSpec qualified as BrowserContextStorageStateSpec
import Playwright.BrowserContextStrictSpec qualified as BrowserContextStrictSpec
import Playwright.BrowserContextTimezoneSpec qualified as BrowserContextTimezoneSpec
import Playwright.BrowserContextUserAgentSpec qualified as BrowserContextUserAgentSpec
import Playwright.BrowserContextViewportMobileSpec qualified as BrowserContextViewportMobileSpec
import Playwright.BrowserContextViewportSpec qualified as BrowserContextViewportSpec
import Playwright.BrowserTypeBasicSpec qualified as BrowserTypeBasicSpec
import Playwright.BrowserTypeLaunchSpec qualified as BrowserTypeLaunchSpec
import Playwright.CapabilitiesSpec qualified as CapabilitiesSpec
import Playwright.ConnectionSpec qualified as ConnectionSpec
import Playwright.DefaultBrowserContext1Spec qualified as DefaultBrowserContext1Spec
import Playwright.DefaultBrowserContext2Spec qualified as DefaultBrowserContext2Spec
import Playwright.DownloadSpec qualified as DownloadSpec
import Playwright.DownloadsPathSpec qualified as DownloadsPathSpec
import Playwright.EmulationFocusSpec qualified as EmulationFocusSpec
import Playwright.ElementHandleBoundingBoxSpec qualified as ElementHandleBoundingBoxSpec
import Playwright.ElementHandleClickSpec qualified as ElementHandleClickSpec
import Playwright.ElementHandleContentFrameSpec qualified as ElementHandleContentFrameSpec
import Playwright.ElementHandleConvenienceSpec qualified as ElementHandleConvenienceSpec
import Playwright.ElementHandleEvalOnSelectorSpec qualified as ElementHandleEvalOnSelectorSpec
import Playwright.ElementHandleMiscSpec qualified as ElementHandleMiscSpec
import Playwright.ElementHandleOwnerFrameSpec qualified as ElementHandleOwnerFrameSpec
import Playwright.ElementHandlePressSpec qualified as ElementHandlePressSpec
import Playwright.ElementHandleQuerySelectorSpec qualified as ElementHandleQuerySelectorSpec
import Playwright.ElementHandleScrollIntoViewSpec qualified as ElementHandleScrollIntoViewSpec
import Playwright.ElementHandleScreenshotSpec qualified as ElementHandleScreenshotSpec
import Playwright.ElementHandleSelectTextSpec qualified as ElementHandleSelectTextSpec
import Playwright.ElementHandleTypeSpec qualified as ElementHandleTypeSpec
import Playwright.ElementHandleWaitForElementStateSpec qualified as ElementHandleWaitForElementStateSpec
import Playwright.EvalOnSelectorAllSpec qualified as EvalOnSelectorAllSpec
import Playwright.EvalOnSelectorSpec qualified as EvalOnSelectorSpec
import Playwright.FrameEvaluateSpec qualified as FrameEvaluateSpec
import Playwright.FrameFrameElementSpec qualified as FrameFrameElementSpec
import Playwright.FrameGotoSpec qualified as FrameGotoSpec
import Playwright.FrameHierarchySpec qualified as FrameHierarchySpec
import Playwright.GeolocationSpec qualified as GeolocationSpec
import Playwright.GlobalFetchCookieSpec qualified as GlobalFetchCookieSpec
import Playwright.GlobalFetchSpec qualified as GlobalFetchSpec
import Playwright.HitTargetSpec qualified as HitTargetSpec
import Playwright.IntegrationSpec qualified as IntegrationSpec
import Playwright.InterceptionSpec qualified as InterceptionSpec
import Playwright.JsHandleAsElementSpec qualified as JsHandleAsElementSpec
import Playwright.JsHandleEvaluateSpec qualified as JsHandleEvaluateSpec
import Playwright.JsHandleJsonValueSpec qualified as JsHandleJsonValueSpec
import Playwright.JsHandlePropertiesSpec qualified as JsHandlePropertiesSpec
import Playwright.JsHandleToStringSpec qualified as JsHandleToStringSpec
import Playwright.LocatorClickSpec qualified as LocatorClickSpec
import Playwright.LocatorConvenienceSpec qualified as LocatorConvenienceSpec
import Playwright.LocatorDispatchEventTouchSpec qualified as LocatorDispatchEventTouchSpec
import Playwright.LocatorElementHandleSpec qualified as LocatorElementHandleSpec
import Playwright.LocatorEvaluateSpec qualified as LocatorEvaluateSpec
import Playwright.LocatorFrameSpec qualified as LocatorFrameSpec
import Playwright.LocatorHighlightSpec qualified as LocatorHighlightSpec
import Playwright.LocatorIsVisibleSpec qualified as LocatorIsVisibleSpec
import Playwright.LocatorListSpec qualified as LocatorListSpec
import Playwright.LocatorMiscSpec qualified as LocatorMiscSpec
import Playwright.LocatorMisc2Spec qualified as LocatorMisc2Spec
import Playwright.LocatorQuerySpec qualified as LocatorQuerySpec
import Playwright.LocatorSpec qualified as LocatorSpec
import Playwright.OopifSpec qualified as OopifSpec
import Playwright.PageAddInitScriptSpec qualified as PageAddInitScriptSpec
import Playwright.PageAddLocatorHandlerSpec qualified as PageAddLocatorHandlerSpec
import Playwright.PageAddScriptTagSpec qualified as PageAddScriptTagSpec
import Playwright.PageAddStyleTagSpec qualified as PageAddStyleTagSpec
import Playwright.PageAutowaitingBasicSpec qualified as PageAutowaitingBasicSpec
import Playwright.PageAutowaitingNoHangSpec qualified as PageAutowaitingNoHangSpec
import Playwright.PageBasicSpec qualified as PageBasicSpec
import Playwright.PageCheckSpec qualified as PageCheckSpec
import Playwright.PageClockSpec qualified as PageClockSpec
import Playwright.PageClickSpec qualified as PageClickSpec
import Playwright.PageClickReactSpec qualified as PageClickReactSpec
import Playwright.PageClickScrollSpec qualified as PageClickScrollSpec
import Playwright.PageClickTimeoutSpec qualified as PageClickTimeoutSpec
import Playwright.PageCloseSpec qualified as PageCloseSpec
import Playwright.PageDispatchEventSpec qualified as PageDispatchEventSpec
import Playwright.PageDialogSpec qualified as PageDialogSpec
import Playwright.PageDragSpec qualified as PageDragSpec
import Playwright.PageDropSpec qualified as PageDropSpec
import Playwright.PageEventConsoleSpec qualified as PageEventConsoleSpec
import Playwright.PageEventCrashSpec qualified as PageEventCrashSpec
import Playwright.PageEventLoadSpec qualified as PageEventLoadSpec
import Playwright.PageEventNetworkSpec qualified as PageEventNetworkSpec
import Playwright.PageEventPopupSpec qualified as PageEventPopupSpec
import Playwright.PageEventRequestSpec qualified as PageEventRequestSpec
import Playwright.PageEvaluateSpec qualified as PageEvaluateSpec
import Playwright.PageEvaluateHandleSpec qualified as PageEvaluateHandleSpec
import Playwright.PageEmulateMediaSpec qualified as PageEmulateMediaSpec
import Playwright.PageEventPageErrorSpec qualified as PageEventPageErrorSpec
import Playwright.PageExposeFunctionSpec qualified as PageExposeFunctionSpec
import Playwright.PageFileChooserSpec qualified as PageFileChooserSpec
import Playwright.PageFillSpec qualified as PageFillSpec
import Playwright.PageFocusSpec qualified as PageFocusSpec
import Playwright.PageGotoSpec qualified as PageGotoSpec
import Playwright.PageHistorySpec qualified as PageHistorySpec
import Playwright.PageKeyboardSpec qualified as PageKeyboardSpec
import Playwright.PageLocalStorageSpec qualified as PageLocalStorageSpec
import Playwright.PageMouseSpec qualified as PageMouseSpec
import Playwright.PageNavigationSpec qualified as PageNavigationSpec
import Playwright.PageNetworkIdleSpec qualified as PageNetworkIdleSpec
import Playwright.PageNetworkPostDataSpec qualified as PageNetworkPostDataSpec
import Playwright.PageNetworkRequestSpec qualified as PageNetworkRequestSpec
import Playwright.PageNetworkResponseSpec qualified as PageNetworkResponseSpec
import Playwright.PageNetworkSizesSpec qualified as PageNetworkSizesSpec
import Playwright.PagePdfSpec qualified as PagePdfSpec
import Playwright.PageRequestAbortSpec qualified as PageRequestAbortSpec
import Playwright.PageRequestContinueSpec qualified as PageRequestContinueSpec
import Playwright.PageRequestFallbackSpec qualified as PageRequestFallbackSpec
import Playwright.PageRequestFulfillSpec qualified as PageRequestFulfillSpec
import Playwright.PageRequestInterceptSpec qualified as PageRequestInterceptSpec
import Playwright.PageRequestGcSpec qualified as PageRequestGcSpec
import Playwright.PageRouteSpec qualified as PageRouteSpec
import Playwright.PageScreenshotSpec qualified as PageScreenshotSpec
import Playwright.PageSetContentSpec qualified as PageSetContentSpec
import Playwright.PageSelectOptionSpec qualified as PageSelectOptionSpec
import Playwright.PageSetExtraHTTPHeadersSpec qualified as PageSetExtraHTTPHeadersSpec
import Playwright.PageSetInputFilesSpec qualified as PageSetInputFilesSpec
import Playwright.PageStrictSpec qualified as PageStrictSpec
import Playwright.PageWaitForFunctionSpec qualified as PageWaitForFunctionSpec
import Playwright.PageWaitForLoadStateSpec qualified as PageWaitForLoadStateSpec
import Playwright.PageWaitForNavigationSpec qualified as PageWaitForNavigationSpec
import Playwright.PageWaitForResponseSpec qualified as PageWaitForResponseSpec
import Playwright.PageWaitForRequestSpec qualified as PageWaitForRequestSpec
import Playwright.PageWaitForSelector2Spec qualified as PageWaitForSelector2Spec
import Playwright.PageWaitForSelectorSpec qualified as PageWaitForSelectorSpec
import Playwright.PageWaitForUrlSpec qualified as PageWaitForUrlSpec
import Playwright.PermissionsSpec qualified as PermissionsSpec
import Playwright.PopupSpec qualified as PopupSpec
import Playwright.ProxySpec qualified as ProxySpec
import Playwright.QuerySelectorSpec qualified as QuerySelectorSpec
import Playwright.RetargetSpec qualified as RetargetSpec
import Playwright.ResourceTimingSpec qualified as ResourceTimingSpec
import Playwright.SelectorsCssSpec qualified as SelectorsCssSpec
import Playwright.SelectorsFrameSpec qualified as SelectorsFrameSpec
import Playwright.SelectorsGetBySpec qualified as SelectorsGetBySpec
import Playwright.SelectorsMiscSpec qualified as SelectorsMiscSpec
import Playwright.SelectorsRoleSpec qualified as SelectorsRoleSpec
import Playwright.SelectorsRegisterSpec qualified as SelectorsRegisterSpec
import Playwright.SelectorsTextSpec qualified as SelectorsTextSpec
import Playwright.SharedWorkerSpec qualified as SharedWorkerSpec
import Playwright.TapSpec qualified as TapSpec
import Playwright.TransportSpec qualified as TransportSpec
import Playwright.UnrouteBehaviorSpec qualified as UnrouteBehaviorSpec
import Playwright.WheelSpec qualified as WheelSpec
import Playwright.WorkersSpec qualified as WorkersSpec
import Test.Tasty
  ( defaultMain
  , testGroup
  )

main :: IO ()
main =
  defaultMain $
    testGroup
      "playwright"
        [ TransportSpec.tests
        , BeforeunloadSpec.tests
        , BrowserSpec.tests
        , BrowserContextAddCookiesSpec.tests
        , BrowserContextAddInitScriptSpec.tests
        , BrowserContextBaseUrlSpec.tests
        , BrowserContextBasicSpec.tests
        , BrowserContextClearCookiesSpec.tests
        , BrowserContextCookiesSpec.tests
        , BrowserContextCredentialsSpec.tests
        , BrowserContextCspSpec.tests
        , BrowserContextDeviceSpec.tests
        , BrowserContextDsfSpec.tests
        , BrowserContextEventsSpec.tests
        , BrowserContextExposeFunctionSpec.tests
        , BrowserContextFetchSpec.tests
        , BrowserContextJavaScriptSpec.tests
        , BrowserContextLocaleSpec.tests
        , BrowserContextNetworkEventSpec.tests
        , BrowserContextOfflineSpec.tests
        , BrowserContextPageEventSpec.tests
        , BrowserContextPagesSpec.tests
        , BrowserContextProxySpec.tests
        , BrowserContextRouteSpec.tests
        , BrowserContextServiceWorkerPolicySpec.tests
        , BrowserContextSetExtraHTTPHeadersSpec.tests
        , BrowserContextStorageStateSpec.tests
        , BrowserContextStrictSpec.tests
        , BrowserContextTimezoneSpec.tests
        , BrowserContextUserAgentSpec.tests
        , BrowserContextViewportMobileSpec.tests
        , BrowserContextViewportSpec.tests
        , BrowserTypeBasicSpec.tests
        , BrowserTypeLaunchSpec.tests
        , CapabilitiesSpec.tests
        , ConnectionSpec.tests
        , DefaultBrowserContext1Spec.tests
        , DefaultBrowserContext2Spec.tests
        , DownloadSpec.tests
        , DownloadsPathSpec.tests
        , EmulationFocusSpec.tests
        , ElementHandleBoundingBoxSpec.tests
        , ElementHandleClickSpec.tests
        , ElementHandleContentFrameSpec.tests
        , ElementHandleConvenienceSpec.tests
        , ElementHandleEvalOnSelectorSpec.tests
        , ElementHandleMiscSpec.tests
        , ElementHandleOwnerFrameSpec.tests
        , ElementHandlePressSpec.tests
        , ElementHandleQuerySelectorSpec.tests
        , ElementHandleScrollIntoViewSpec.tests
        , ElementHandleScreenshotSpec.tests
        , ElementHandleSelectTextSpec.tests
        , ElementHandleTypeSpec.tests
        , ElementHandleWaitForElementStateSpec.tests
        , EvalOnSelectorAllSpec.tests
        , EvalOnSelectorSpec.tests
        , FrameEvaluateSpec.tests
        , FrameFrameElementSpec.tests
        , FrameGotoSpec.tests
        , FrameHierarchySpec.tests
        , GeolocationSpec.tests
        , GlobalFetchCookieSpec.tests
        , GlobalFetchSpec.tests
        , HitTargetSpec.tests
        , IntegrationSpec.tests
        , InterceptionSpec.tests
        , JsHandleAsElementSpec.tests
        , JsHandleEvaluateSpec.tests
        , JsHandleJsonValueSpec.tests
        , JsHandlePropertiesSpec.tests
        , JsHandleToStringSpec.tests
        , LocatorSpec.tests
        , LocatorClickSpec.tests
        , LocatorConvenienceSpec.tests
        , LocatorDispatchEventTouchSpec.tests
        , LocatorElementHandleSpec.tests
        , LocatorEvaluateSpec.tests
        , LocatorFrameSpec.tests
        , LocatorHighlightSpec.tests
        , LocatorIsVisibleSpec.tests
        , LocatorListSpec.tests
        , LocatorMiscSpec.tests
        , LocatorMisc2Spec.tests
        , LocatorQuerySpec.tests
        , OopifSpec.tests
        , PageAddInitScriptSpec.tests
        , PageAddLocatorHandlerSpec.tests
        , PageAddScriptTagSpec.tests
        , PageAddStyleTagSpec.tests
        , PageAutowaitingBasicSpec.tests
        , PageAutowaitingNoHangSpec.tests
        , PageBasicSpec.tests
        , PageCheckSpec.tests
        , PageClockSpec.tests
        , PageClickSpec.tests
        , PageClickReactSpec.tests
        , PageClickScrollSpec.tests
        , PageClickTimeoutSpec.timeout1Tests
        , PageClickTimeoutSpec.timeout2Tests
        , PageClickTimeoutSpec.timeout3Tests
        , PageClickTimeoutSpec.timeout4Tests
        , PageCloseSpec.tests
        , PageDispatchEventSpec.tests
        , PageDialogSpec.tests
        , PageDragSpec.tests
        , PageDropSpec.tests
        , PageEmulateMediaSpec.tests
        , PageEventConsoleSpec.tests
        , PageEventCrashSpec.tests
        , PageEventLoadSpec.tests
        , PageEventNetworkSpec.tests
        , PageEventPageErrorSpec.tests
        , PageEventPopupSpec.tests
        , PageEventRequestSpec.tests
        , PageEvaluateSpec.tests
        , PageEvaluateHandleSpec.tests
        , PageExposeFunctionSpec.tests
        , PageFileChooserSpec.tests
        , PageFillSpec.tests
        , PageFocusSpec.tests
        , PageGotoSpec.tests
        , PageHistorySpec.tests
        , PageKeyboardSpec.tests
        , PageLocalStorageSpec.tests
        , PageMouseSpec.tests
        , PageNavigationSpec.tests
        , PageNetworkIdleSpec.tests
        , PageNetworkPostDataSpec.tests
        , PageNetworkRequestSpec.tests
        , PageNetworkResponseSpec.tests
        , PageNetworkSizesSpec.tests
        , PagePdfSpec.tests
        , PageRequestAbortSpec.tests
        , PageRequestContinueSpec.tests
        , PageRequestFallbackSpec.tests
        , PageRequestFulfillSpec.tests
        , PageRequestInterceptSpec.tests
        , PageRequestGcSpec.tests
        , PageRouteSpec.tests
        , PageSelectOptionSpec.tests
        , PageScreenshotSpec.tests
        , PageSetContentSpec.tests
        , PageSetExtraHTTPHeadersSpec.tests
        , PageSetInputFilesSpec.tests
        , PageStrictSpec.tests
        , PageWaitForFunctionSpec.tests
        , PageWaitForLoadStateSpec.tests
        , PageWaitForNavigationSpec.tests
        , PageWaitForRequestSpec.tests
        , PageWaitForResponseSpec.tests
        , PageWaitForSelector2Spec.tests
        , PageWaitForSelectorSpec.tests
        , PageWaitForUrlSpec.tests
        , PermissionsSpec.tests
        , PopupSpec.tests
        , ProxySpec.tests
        , QuerySelectorSpec.tests
        , RetargetSpec.tests
        , ResourceTimingSpec.tests
        , SelectorsCssSpec.tests
        , SelectorsFrameSpec.tests
        , SelectorsGetBySpec.tests
        , SelectorsMiscSpec.tests
        , SelectorsRoleSpec.tests
        , SelectorsRegisterSpec.tests
        , SelectorsTextSpec.tests
        , SharedWorkerSpec.tests
        , TapSpec.tests
        , UnrouteBehaviorSpec.tests
        , WheelSpec.tests
        , WorkersSpec.tests
        ]
