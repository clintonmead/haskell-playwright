module Main
  ( main
  ) where

import Playwright.ConnectionSpec qualified as ConnectionSpec
import Playwright.IntegrationSpec qualified as IntegrationSpec
import Playwright.LocatorSpec qualified as LocatorSpec
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
        ]
