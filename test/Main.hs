module Main
  ( main
  ) where

import Playwright.ConnectionSpec qualified as ConnectionSpec
import Playwright.ElementHandleBoundingBoxSpec qualified as ElementHandleBoundingBoxSpec
import Playwright.IntegrationSpec qualified as IntegrationSpec
import Playwright.LocatorClickSpec qualified as LocatorClickSpec
import Playwright.LocatorEvaluateSpec qualified as LocatorEvaluateSpec
import Playwright.LocatorIsVisibleSpec qualified as LocatorIsVisibleSpec
import Playwright.LocatorMiscSpec qualified as LocatorMiscSpec
import Playwright.LocatorQuerySpec qualified as LocatorQuerySpec
import Playwright.LocatorSpec qualified as LocatorSpec
import Playwright.PageAddInitScriptSpec qualified as PageAddInitScriptSpec
import Playwright.PageAddScriptTagSpec qualified as PageAddScriptTagSpec
import Playwright.PageCheckSpec qualified as PageCheckSpec
import Playwright.PageClickSpec qualified as PageClickSpec
import Playwright.PageDispatchEventSpec qualified as PageDispatchEventSpec
import Playwright.PageEvaluateSpec qualified as PageEvaluateSpec
import Playwright.PageFillSpec qualified as PageFillSpec
import Playwright.PageFocusSpec qualified as PageFocusSpec
import Playwright.PageGotoSpec qualified as PageGotoSpec
import Playwright.PageHistorySpec qualified as PageHistorySpec
import Playwright.PageKeyboardSpec qualified as PageKeyboardSpec
import Playwright.PageMouseSpec qualified as PageMouseSpec
import Playwright.PageSetContentSpec qualified as PageSetContentSpec
import Playwright.PageSelectOptionSpec qualified as PageSelectOptionSpec
import Playwright.PageSetExtraHTTPHeadersSpec qualified as PageSetExtraHTTPHeadersSpec
import Playwright.PageWaitForFunctionSpec qualified as PageWaitForFunctionSpec
import Playwright.PageWaitForLoadStateSpec qualified as PageWaitForLoadStateSpec
import Playwright.PageWaitForSelectorSpec qualified as PageWaitForSelectorSpec
import Playwright.SelectorsGetBySpec qualified as SelectorsGetBySpec
import Playwright.TransportSpec qualified as TransportSpec
import Playwright.WheelSpec qualified as WheelSpec
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
        , ConnectionSpec.tests
        , ElementHandleBoundingBoxSpec.tests
        , IntegrationSpec.tests
        , LocatorSpec.tests
        , LocatorClickSpec.tests
        , LocatorEvaluateSpec.tests
        , LocatorIsVisibleSpec.tests
        , LocatorMiscSpec.tests
        , LocatorQuerySpec.tests
        , PageAddInitScriptSpec.tests
        , PageAddScriptTagSpec.tests
        , PageCheckSpec.tests
        , PageClickSpec.tests
        , PageDispatchEventSpec.tests
        , PageEvaluateSpec.tests
        , PageFillSpec.tests
        , PageFocusSpec.tests
        , PageGotoSpec.tests
        , PageHistorySpec.tests
        , PageKeyboardSpec.tests
        , PageMouseSpec.tests
        , PageSelectOptionSpec.tests
        , PageSetContentSpec.tests
        , PageSetExtraHTTPHeadersSpec.tests
        , PageWaitForFunctionSpec.tests
        , PageWaitForLoadStateSpec.tests
        , PageWaitForSelectorSpec.tests
        , SelectorsGetBySpec.tests
        , WheelSpec.tests
        ]
