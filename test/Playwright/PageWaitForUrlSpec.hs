module Playwright.PageWaitForUrlSpec
  ( tests
  ) where

import Playwright
  ( UrlGlob (UrlGlob)
  , click
  , defaultGotoOptions
  , evaluateTextWith
  , goto
  , milliseconds
  , setContent
  , textArgument
  , urlFromText
  , waitForURL
  , waitForURLWith
  )
import Playwright.Harness
  ( assertWaitTimeout
  , locate
  , withAssetPage
  , withAssets
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-wait-for-url"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            evaluateTextWith
              page
              "url => { window.location.href = url; return url; }"
              (textArgument (prefix <> "/grid.html"))
          waitForURL page (UrlGlob "**/grid.html")
    , testCase "should respect timeout" $
        withAssetPage "empty.html" $ \page ->
          case milliseconds 2500 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertWaitTimeout (waitForURLWith page (UrlGlob "**/frame.html") timeout)
    , testCase "should work with clicking on anchor links" $
        withAssetPage "empty.html" $ \page -> do
          setContent page "<a href='#foobar'>foobar</a>"
          click (locate page "a")
          waitForURL page (UrlGlob "**/*#foobar")
    ]
