module Playwright.ElementHandleScreenshotSpec
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
  , goto
  , querySelector
  , screenshotHandle
  , selectorFromText
  , setViewportSize
  , urlFromText
  )
import Playwright.Harness
  ( requireHandle
  , withAssets
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
    "elementhandle-screenshot"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          handle <- requireHandle =<< querySelector page (selectorFromText ".box:nth-of-type(3)")
          image <- screenshotHandle handle
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
          assertBool "non-empty" (BS.length image > 100)
    ]

pngMagic :: ByteString
pngMagic =
  "\137PNG\r\n\26\n"
