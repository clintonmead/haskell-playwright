module Playwright.PageScreenshotSpec
  ( tests
  ) where

import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as BS
import Playwright
  ( ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , defaultGotoOptions
  , defaultScreenshotOptions
  , goto
  , screenshot
  , setViewportSize
  , urlFromText
  )
import Playwright.Harness
  ( withAssets
  , assertBool
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
    "page-screenshot"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <- screenshot page defaultScreenshotOptions
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
          assertBool "non-empty" (BS.length image > 100)
    ]

pngMagic :: ByteString
pngMagic =
  "\137PNG\r\n\26\n"
