module Playwright.BrowserContextDsfSpec
  ( tests
  ) where

import Data.Text
  ( isInfixOf
  )
import Playwright
  ( NewContextOptions (newContextDeviceScaleFactor)
  , UrlMatch (UrlMatchGlob)
  , defaultGotoOptions
  , defaultNewContextOptions
  , deviceScaleFactorFromInt
  , goto
  , requestUrl
  , urlFromText
  , urlToText
  , armRequest
  )
import Playwright.Harness
  ( withAssetsWith
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
    "browsercontext-dsf"
    [ testCase "should fetch lodpi assets" $
        withAssetsWith
          defaultNewContextOptions {newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 1)}
          $ \prefix page ->
            armRequest page (UrlMatchGlob "**/image*") >>= \pending -> do
              _ <- goto page (urlFromText (prefix <> "/highdpi.html")) defaultGotoOptions
              request <- pending
              assertBool "image1x" . ("image1x" `isInfixOf`) . urlToText =<< requestUrl request
    , testCase "should fetch hidpi assets" $
        withAssetsWith
          defaultNewContextOptions {newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 2)}
          $ \prefix page ->
            armRequest page (UrlMatchGlob "**/image*") >>= \pending -> do
              _ <- goto page (urlFromText (prefix <> "/highdpi.html")) defaultGotoOptions
              request <- pending
              assertBool "image2x" . ("image2x" `isInfixOf`) . urlToText =<< requestUrl request
    ]
