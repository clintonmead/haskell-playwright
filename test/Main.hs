module Main
  ( main
  ) where

import Playwright.ConnectionSpec qualified as ConnectionSpec
import Playwright.IntegrationSpec qualified as IntegrationSpec
import Playwright.LocatorClickSpec qualified as LocatorClickSpec
import Playwright.LocatorSpec qualified as LocatorSpec
import Playwright.PageCheckSpec qualified as PageCheckSpec
import Playwright.PageFillSpec qualified as PageFillSpec
import Playwright.PageFocusSpec qualified as PageFocusSpec
import Playwright.PageSetContentSpec qualified as PageSetContentSpec
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
        , PageCheckSpec.tests
        , PageFillSpec.tests
        , PageFocusSpec.tests
        , PageSetContentSpec.tests
        ]
