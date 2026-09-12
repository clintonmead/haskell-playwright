module Playwright.PageWaitForNavigationSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Data.Text
  ( isInfixOf
  )
import Playwright
  ( UrlGlob (UrlGlob)
  , defaultGotoOptions
  , evaluateTextWith
  , goto
  , milliseconds
  , responseOk
  , responseUrl
  , textArgument
  , urlFromText
  , urlToText
  , waitForNavigation
  , waitForNavigationWith
  )
import Playwright.Harness
  ( assertWaitTimeout
  , withAssetPage
  , withAssets
  , assertBool
  , assertEqual
  , assertFailure
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
    "page-wait-for-navigation"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForNavigation page) $ \nav -> do
            _ <-
              evaluateTextWith
                page
                "url => { window.location.href = url; return url; }"
                (textArgument (prefix <> "/grid.html"))
            response <- wait nav
            case response of
              Nothing ->
                assertFailure "expected navigation response"
              Just value -> do
                assertEqual "ok" True =<< responseOk value
                url <- responseUrl value
                assertBool "grid.html" ("grid.html" `isInfixOf` urlToText url)
    , testCase "should respect timeout" $
        withAssetPage "empty.html" $ \page ->
          case milliseconds 1000 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertWaitTimeout (waitForNavigationWith page timeout (Just (UrlGlob "**/frame.html")))
    ]
