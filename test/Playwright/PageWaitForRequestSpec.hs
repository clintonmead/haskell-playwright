module Playwright.PageWaitForRequestSpec
  ( tests
  ) where

import Playwright
  ( UrlGlob (UrlGlob)
  , armRequest
  , armRequestWhere
  , armRequestWith
  , defaultGotoOptions
  , evaluate
  , goto
  , milliseconds
  , requestUrl
  , urlFromText
  , urlToText
  , waitForRequest
  , waitForRequestWith
  , setDefaultTimeout
  )
import Playwright.Harness
  ( assertWaitTimeout
  , withAssets
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
    "page-wait-for-request"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let wanted = prefix <> "/digits/2.png"
          takeRequest <- armRequest page (UrlGlob wanted)
          _ <-
            evaluate
              page
              "() => { void fetch('/digits/1.png'); void fetch('/digits/2.png'); void fetch('/digits/3.png'); }"
          request <- takeRequest
          url <- requestUrl request
          assertEqual "url" wanted (urlToText url)
    , testCase "should work with predicate" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let wanted = prefix <> "/digits/2.png"
          takeRequest <- armRequestWhere page (\request -> (== wanted) . urlToText <$> requestUrl request)
          _ <-
            evaluate
              page
              "() => { void fetch('/digits/1.png'); void fetch('/digits/2.png'); void fetch('/digits/3.png'); }"
          request <- takeRequest
          url <- requestUrl request
          assertEqual "url" wanted (urlToText url)
    , testCase "should work with no timeout" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let wanted = prefix <> "/digits/2.png"
          case milliseconds 0 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              takeRequest <- armRequestWith page (UrlGlob wanted) timeout
              _ <-
                evaluate
                  page
                  "() => setTimeout(() => { void fetch('/digits/1.png'); void fetch('/digits/2.png'); void fetch('/digits/3.png'); }, 50)"
              request <- takeRequest
              url <- requestUrl request
              assertEqual "url" wanted (urlToText url)
    , testCase "should respect timeout" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case milliseconds 1 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertWaitTimeout (waitForRequestWith page (UrlGlob (prefix <> "/does-not-exist.png")) timeout)
    , testCase "should respect default timeout" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case milliseconds 1 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              setDefaultTimeout page timeout
              assertWaitTimeout (waitForRequest page (UrlGlob (prefix <> "/does-not-exist.png")))
    ]
