module Main
  ( main
  ) where

import Playwright.ConnectionSpec qualified as ConnectionSpec
import Playwright.IntegrationSpec qualified as IntegrationSpec
import Playwright.LocatorClickSpec qualified as LocatorClickSpec
import Playwright.LocatorIsVisibleSpec qualified as LocatorIsVisibleSpec
import Playwright.LocatorSpec qualified as LocatorSpec
import Playwright.PageCheckSpec qualified as PageCheckSpec
import Playwright.PageClickSpec qualified as PageClickSpec
import Playwright.PageEvaluateSpec qualified as PageEvaluateSpec
import Playwright.PageFillSpec qualified as PageFillSpec
import Playwright.PageFocusSpec qualified as PageFocusSpec
import Playwright.PageGotoSpec qualified as PageGotoSpec
import Playwright.PageKeyboardSpec qualified as PageKeyboardSpec
import Playwright.PageSetContentSpec qualified as PageSetContentSpec
import Playwright.PageWaitForSelectorSpec qualified as PageWaitForSelectorSpec
import Playwright.TransportSpec qualified as TransportSpec
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
        , IntegrationSpec.tests
        , LocatorSpec.tests
        , LocatorClickSpec.tests
        , LocatorIsVisibleSpec.tests
        , PageCheckSpec.tests
        , PageClickSpec.tests
        , PageEvaluateSpec.tests
        , PageFillSpec.tests
        , PageFocusSpec.tests
        , PageGotoSpec.tests
        , PageKeyboardSpec.tests
        , PageSetContentSpec.tests
        , PageWaitForSelectorSpec.tests
        ]
