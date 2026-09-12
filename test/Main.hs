module Main
  ( main
  ) where

import Playwright.BrowserContextAddCookiesSpec qualified as BrowserContextAddCookiesSpec
import Playwright.BrowserContextAddInitScriptSpec qualified as BrowserContextAddInitScriptSpec
import Playwright.BrowserContextBaseUrlSpec qualified as BrowserContextBaseUrlSpec
import Playwright.BrowserContextCookiesSpec qualified as BrowserContextCookiesSpec
import Playwright.BrowserContextCredentialsSpec qualified as BrowserContextCredentialsSpec
import Playwright.BrowserContextCspSpec qualified as BrowserContextCspSpec
import Playwright.BrowserContextExposeFunctionSpec qualified as BrowserContextExposeFunctionSpec
import Playwright.BrowserContextJavaScriptSpec qualified as BrowserContextJavaScriptSpec
import Playwright.BrowserContextLocaleSpec qualified as BrowserContextLocaleSpec
import Playwright.BrowserContextOfflineSpec qualified as BrowserContextOfflineSpec
import Playwright.BrowserContextPageEventSpec qualified as BrowserContextPageEventSpec
import Playwright.BrowserContextPagesSpec qualified as BrowserContextPagesSpec
import Playwright.BrowserContextRouteSpec qualified as BrowserContextRouteSpec
import Playwright.BrowserContextSetExtraHTTPHeadersSpec qualified as BrowserContextSetExtraHTTPHeadersSpec
import Playwright.BrowserContextStorageStateSpec qualified as BrowserContextStorageStateSpec
import Playwright.BrowserContextTimezoneSpec qualified as BrowserContextTimezoneSpec
import Playwright.BrowserContextViewportSpec qualified as BrowserContextViewportSpec
import Playwright.ConnectionSpec qualified as ConnectionSpec
import Playwright.DownloadSpec qualified as DownloadSpec
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
import Playwright.IntegrationSpec qualified as IntegrationSpec
import Playwright.JsHandleAsElementSpec qualified as JsHandleAsElementSpec
import Playwright.JsHandleEvaluateSpec qualified as JsHandleEvaluateSpec
import Playwright.JsHandleJsonValueSpec qualified as JsHandleJsonValueSpec
import Playwright.JsHandlePropertiesSpec qualified as JsHandlePropertiesSpec
import Playwright.JsHandleToStringSpec qualified as JsHandleToStringSpec
import Playwright.LocatorClickSpec qualified as LocatorClickSpec
import Playwright.LocatorElementHandleSpec qualified as LocatorElementHandleSpec
import Playwright.LocatorEvaluateSpec qualified as LocatorEvaluateSpec
import Playwright.LocatorFrameSpec qualified as LocatorFrameSpec
import Playwright.LocatorIsVisibleSpec qualified as LocatorIsVisibleSpec
import Playwright.LocatorListSpec qualified as LocatorListSpec
import Playwright.LocatorMiscSpec qualified as LocatorMiscSpec
import Playwright.LocatorQuerySpec qualified as LocatorQuerySpec
import Playwright.LocatorSpec qualified as LocatorSpec
import Playwright.PageAddInitScriptSpec qualified as PageAddInitScriptSpec
import Playwright.PageAddScriptTagSpec qualified as PageAddScriptTagSpec
import Playwright.PageAddStyleTagSpec qualified as PageAddStyleTagSpec
import Playwright.PageBasicSpec qualified as PageBasicSpec
import Playwright.PageCheckSpec qualified as PageCheckSpec
import Playwright.PageClockSpec qualified as PageClockSpec
import Playwright.PageClickSpec qualified as PageClickSpec
import Playwright.PageCloseSpec qualified as PageCloseSpec
import Playwright.PageDispatchEventSpec qualified as PageDispatchEventSpec
import Playwright.PageDialogSpec qualified as PageDialogSpec
import Playwright.PageDragSpec qualified as PageDragSpec
import Playwright.PageEventConsoleSpec qualified as PageEventConsoleSpec
import Playwright.PageEventLoadSpec qualified as PageEventLoadSpec
import Playwright.PageEventPopupSpec qualified as PageEventPopupSpec
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
import Playwright.PageNetworkIdleSpec qualified as PageNetworkIdleSpec
import Playwright.PageNetworkRequestSpec qualified as PageNetworkRequestSpec
import Playwright.PageNetworkResponseSpec qualified as PageNetworkResponseSpec
import Playwright.PagePdfSpec qualified as PagePdfSpec
import Playwright.PageRequestAbortSpec qualified as PageRequestAbortSpec
import Playwright.PageRequestContinueSpec qualified as PageRequestContinueSpec
import Playwright.PageRequestFallbackSpec qualified as PageRequestFallbackSpec
import Playwright.PageRequestFulfillSpec qualified as PageRequestFulfillSpec
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
import Playwright.PageWaitForSelectorSpec qualified as PageWaitForSelectorSpec
import Playwright.PageWaitForUrlSpec qualified as PageWaitForUrlSpec
import Playwright.PermissionsSpec qualified as PermissionsSpec
import Playwright.QuerySelectorSpec qualified as QuerySelectorSpec
import Playwright.SelectorsGetBySpec qualified as SelectorsGetBySpec
import Playwright.TapSpec qualified as TapSpec
import Playwright.TransportSpec qualified as TransportSpec
import Playwright.WheelSpec qualified as WheelSpec
import Playwright.WorkersSpec qualified as WorkersSpec
import Test.Tasty
  ( defaultMain
  , localOption
  , testGroup
  )
import Test.Tasty.Runners
  ( NumThreads (NumThreads)
  )

main :: IO ()
main =
  defaultMain $
    localOption (NumThreads 1) $
      testGroup
        "playwright"
        [ TransportSpec.tests
        , BrowserContextAddCookiesSpec.tests
        , BrowserContextAddInitScriptSpec.tests
        , BrowserContextBaseUrlSpec.tests
        , BrowserContextCookiesSpec.tests
        , BrowserContextCredentialsSpec.tests
        , BrowserContextCspSpec.tests
        , BrowserContextExposeFunctionSpec.tests
        , BrowserContextJavaScriptSpec.tests
        , BrowserContextLocaleSpec.tests
        , BrowserContextOfflineSpec.tests
        , BrowserContextPageEventSpec.tests
        , BrowserContextPagesSpec.tests
        , BrowserContextRouteSpec.tests
        , BrowserContextSetExtraHTTPHeadersSpec.tests
        , BrowserContextStorageStateSpec.tests
        , BrowserContextTimezoneSpec.tests
        , BrowserContextViewportSpec.tests
        , ConnectionSpec.tests
        , DownloadSpec.tests
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
        , IntegrationSpec.tests
        , JsHandleAsElementSpec.tests
        , JsHandleEvaluateSpec.tests
        , JsHandleJsonValueSpec.tests
        , JsHandlePropertiesSpec.tests
        , JsHandleToStringSpec.tests
        , LocatorSpec.tests
        , LocatorClickSpec.tests
        , LocatorElementHandleSpec.tests
        , LocatorEvaluateSpec.tests
        , LocatorFrameSpec.tests
        , LocatorIsVisibleSpec.tests
        , LocatorListSpec.tests
        , LocatorMiscSpec.tests
        , LocatorQuerySpec.tests
        , PageAddInitScriptSpec.tests
        , PageAddScriptTagSpec.tests
        , PageAddStyleTagSpec.tests
        , PageBasicSpec.tests
        , PageCheckSpec.tests
        , PageClockSpec.tests
        , PageClickSpec.tests
        , PageCloseSpec.tests
        , PageDispatchEventSpec.tests
        , PageDialogSpec.tests
        , PageDragSpec.tests
        , PageEmulateMediaSpec.tests
        , PageEventConsoleSpec.tests
        , PageEventLoadSpec.tests
        , PageEventPageErrorSpec.tests
        , PageEventPopupSpec.tests
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
        , PageNetworkIdleSpec.tests
        , PageNetworkRequestSpec.tests
        , PageNetworkResponseSpec.tests
        , PagePdfSpec.tests
        , PageRequestAbortSpec.tests
        , PageRequestContinueSpec.tests
        , PageRequestFallbackSpec.tests
        , PageRequestFulfillSpec.tests
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
        , PageWaitForSelectorSpec.tests
        , PageWaitForUrlSpec.tests
        , PermissionsSpec.tests
        , QuerySelectorSpec.tests
        , SelectorsGetBySpec.tests
        , TapSpec.tests
        , WheelSpec.tests
        , WorkersSpec.tests
        ]
