module Playwright.IntegrationSpec
  ( tests
  ) where

import Data.ByteString qualified as BS
import Data.Text
  ( isInfixOf
  )
import Playwright
  ( ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , content
  , defaultScreenshotOptions
  , evaluateNumber
  , screenshot
  , setContent
  , setViewportSize
  , title
  )
import Playwright.Harness
  ( withAssetPage
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertBool
  , assertEqual
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "integration"
    [ testCase "setContent title and screenshot" $
        withChromiumPage $ \page -> do
          setContent page "<html><head><title>Haskell Playwright</title></head><body>ok</body></html>"
          pageTitle <- title page
          assertEqual "title" "Haskell Playwright" pageTitle
          html <- content page
          assertBool "content includes body" ("ok" `isInfixOf` html)
          png <- screenshot page defaultScreenshotOptions
          assertBool "screenshot is not empty" (not (BS.null png))
    , testCase "page.title should return the page title" $
        withAssetPage "title.html" $ \page -> do
          pageTitle <- title page
          assertEqual "title" "Woof-Woof" pageTitle
    , testCase "should set viewport size" $
        withChromiumPage $ \page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 600))
          width <- evaluateNumber page "() => window.innerWidth"
          height <- evaluateNumber page "() => window.innerHeight"
          assertEqual "width" (500 :: Double) width
          assertEqual "height" (600 :: Double) height
    ]
