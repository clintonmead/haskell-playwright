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
  , waitForRequest
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
import UnliftIO.Async
  ( wait
  , withAsync
  )

tests :: TestTree
tests =
  testGroup
    "browsercontext-dsf"
    [ testCase "should fetch lodpi assets" $
        withAssetsWith
          defaultNewContextOptions {newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 1)}
          $ \prefix page ->
            withAsync (waitForRequest page (UrlMatchGlob "**/image*")) $ \pending -> do
              _ <- goto page (urlFromText (prefix <> "/highdpi.html")) defaultGotoOptions
              request <- wait pending
              assertBool "image1x" . ("image1x" `isInfixOf`) . urlToText =<< requestUrl request
    , testCase "should fetch hidpi assets" $
        withAssetsWith
          defaultNewContextOptions {newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 2)}
          $ \prefix page ->
            withAsync (waitForRequest page (UrlMatchGlob "**/image*")) $ \pending -> do
              _ <- goto page (urlFromText (prefix <> "/highdpi.html")) defaultGotoOptions
              request <- wait pending
              assertBool "image2x" . ("image2x" `isInfixOf`) . urlToText =<< requestUrl request
    ]
