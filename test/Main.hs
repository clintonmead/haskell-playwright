module Main
  ( main
  ) where

import Playwright.BrowserContextAddCookiesSpec qualified as BrowserContextAddCookiesSpec
import Playwright.BrowserContextCookiesSpec qualified as BrowserContextCookiesSpec
import Playwright.ConnectionSpec qualified as ConnectionSpec
import Playwright.ElementHandleBoundingBoxSpec qualified as ElementHandleBoundingBoxSpec
import Playwright.ElementHandleClickSpec qualified as ElementHandleClickSpec
import Playwright.ElementHandleContentFrameSpec qualified as ElementHandleContentFrameSpec
import Playwright.ElementHandleConvenienceSpec qualified as ElementHandleConvenienceSpec
import Playwright.ElementHandleOwnerFrameSpec qualified as ElementHandleOwnerFrameSpec
import Playwright.ElementHandlePressSpec qualified as ElementHandlePressSpec
import Playwright.FrameEvaluateSpec qualified as FrameEvaluateSpec
import Playwright.FrameHierarchySpec qualified as FrameHierarchySpec
import Playwright.IntegrationSpec qualified as IntegrationSpec
import Playwright.JsHandleJsonValueSpec qualified as JsHandleJsonValueSpec
import Playwright.LocatorClickSpec qualified as LocatorClickSpec
import Playwright.LocatorElementHandleSpec qualified as LocatorElementHandleSpec
import Playwright.LocatorEvaluateSpec qualified as LocatorEvaluateSpec
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
import Playwright.PageClickSpec qualified as PageClickSpec
import Playwright.PageDispatchEventSpec qualified as PageDispatchEventSpec
import Playwright.PageDialogSpec qualified as PageDialogSpec
import Playwright.PageEventConsoleSpec qualified as PageEventConsoleSpec
import Playwright.PageEventPopupSpec qualified as PageEventPopupSpec
import Playwright.PageEvaluateSpec qualified as PageEvaluateSpec
import Playwright.PageEvaluateHandleSpec qualified as PageEvaluateHandleSpec
import Playwright.PageEmulateMediaSpec qualified as PageEmulateMediaSpec
import Playwright.PageEventPageErrorSpec qualified as PageEventPageErrorSpec
import Playwright.PageFillSpec qualified as PageFillSpec
import Playwright.PageFocusSpec qualified as PageFocusSpec
import Playwright.PageGotoSpec qualified as PageGotoSpec
import Playwright.PageHistorySpec qualified as PageHistorySpec
import Playwright.PageKeyboardSpec qualified as PageKeyboardSpec
import Playwright.PageLocalStorageSpec qualified as PageLocalStorageSpec
import Playwright.PageMouseSpec qualified as PageMouseSpec
import Playwright.PageNetworkRequestSpec qualified as PageNetworkRequestSpec
import Playwright.PageNetworkResponseSpec qualified as PageNetworkResponseSpec
import Playwright.PageScreenshotSpec qualified as PageScreenshotSpec
import Playwright.PageSetContentSpec qualified as PageSetContentSpec
import Playwright.PageSelectOptionSpec qualified as PageSelectOptionSpec
import Playwright.PageSetExtraHTTPHeadersSpec qualified as PageSetExtraHTTPHeadersSpec
import Playwright.PageStrictSpec qualified as PageStrictSpec
import Playwright.PageWaitForFunctionSpec qualified as PageWaitForFunctionSpec
import Playwright.PageWaitForLoadStateSpec qualified as PageWaitForLoadStateSpec
import Playwright.PageWaitForResponseSpec qualified as PageWaitForResponseSpec
import Playwright.PageWaitForRequestSpec qualified as PageWaitForRequestSpec
import Playwright.PageWaitForSelectorSpec qualified as PageWaitForSelectorSpec
import Playwright.PageWaitForUrlSpec qualified as PageWaitForUrlSpec
import Playwright.QuerySelectorSpec qualified as QuerySelectorSpec
import Playwright.SelectorsGetBySpec qualified as SelectorsGetBySpec
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
        , BrowserContextCookiesSpec.tests
        , ConnectionSpec.tests
        , ElementHandleBoundingBoxSpec.tests
        , ElementHandleClickSpec.tests
        , ElementHandleContentFrameSpec.tests
        , ElementHandleConvenienceSpec.tests
        , ElementHandleOwnerFrameSpec.tests
        , ElementHandlePressSpec.tests
        , FrameEvaluateSpec.tests
        , FrameHierarchySpec.tests
        , IntegrationSpec.tests
        , JsHandleJsonValueSpec.tests
        , LocatorSpec.tests
        , LocatorClickSpec.tests
        , LocatorElementHandleSpec.tests
        , LocatorEvaluateSpec.tests
        , LocatorIsVisibleSpec.tests
        , LocatorListSpec.tests
        , LocatorMiscSpec.tests
        , LocatorQuerySpec.tests
        , PageAddInitScriptSpec.tests
        , PageAddScriptTagSpec.tests
        , PageAddStyleTagSpec.tests
        , PageBasicSpec.tests
        , PageCheckSpec.tests
        , PageClickSpec.tests
        , PageDispatchEventSpec.tests
        , PageDialogSpec.tests
        , PageEmulateMediaSpec.tests
        , PageEventConsoleSpec.tests
        , PageEventPageErrorSpec.tests
        , PageEventPopupSpec.tests
        , PageEvaluateSpec.tests
        , PageEvaluateHandleSpec.tests
        , PageFillSpec.tests
        , PageFocusSpec.tests
        , PageGotoSpec.tests
        , PageHistorySpec.tests
        , PageKeyboardSpec.tests
        , PageLocalStorageSpec.tests
        , PageMouseSpec.tests
        , PageNetworkRequestSpec.tests
        , PageNetworkResponseSpec.tests
        , PageSelectOptionSpec.tests
        , PageScreenshotSpec.tests
        , PageSetContentSpec.tests
        , PageSetExtraHTTPHeadersSpec.tests
        , PageStrictSpec.tests
        , PageWaitForFunctionSpec.tests
        , PageWaitForLoadStateSpec.tests
        , PageWaitForRequestSpec.tests
        , PageWaitForResponseSpec.tests
        , PageWaitForSelectorSpec.tests
        , PageWaitForUrlSpec.tests
        , QuerySelectorSpec.tests
        , SelectorsGetBySpec.tests
        , WheelSpec.tests
        , WorkersSpec.tests
        ]
