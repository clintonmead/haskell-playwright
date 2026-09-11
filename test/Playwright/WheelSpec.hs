module Playwright.WheelSpec
  ( tests
  ) where

import Playwright
  ( Point (Point)
  , cssPixelFromInt
  , mouseMove
  , mouseWheel
  , setContent
  , waitForFunction
  , wheelDeltaFromInt
  )
import Playwright.Harness
  ( withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

tests :: TestTree
tests =
  testGroup
    "wheel"
    [ testCase "should dispatch wheel events" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"width: 5000px; height: 5000px;\"></div>"
          mouseMove page (Point (cssPixelFromInt 50) (cssPixelFromInt 60))
          mouseWheel page (wheelDeltaFromInt 0) (wheelDeltaFromInt 100)
          _ <- waitForFunction page "() => window.scrollY === 100"
          pure ()
    ]
