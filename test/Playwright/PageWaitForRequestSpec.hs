module Playwright.PageWaitForRequestSpec
  ( tests
  ) where

import Playwright
  ( UrlMatch (UrlMatchGlob, UrlMatchRegex)
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
  , onRequest
  , onRequest
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
          takeRequest <- armRequest page (UrlMatchGlob wanted)
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
              takeRequest <- armRequestWith page (UrlMatchGlob wanted) timeout
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
              assertWaitTimeout (waitForRequestWith page (UrlMatchGlob (prefix <> "/does-not-exist.png")) timeout)
    , testCase "should respect default timeout" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case milliseconds 1 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              setDefaultTimeout page timeout
              assertWaitTimeout (waitForRequest page (UrlMatchGlob (prefix <> "/does-not-exist.png")))
    , testCase "armRequest ignores a request made before it was armed" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          onRequest page (\_ -> pure ())
          _ <- evaluate page "() => fetch('/digits/1.png').then(r => r.text())"
          case milliseconds 500 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              takeRequest <- armRequestWith page (UrlMatchGlob (prefix <> "/digits/1.png")) timeout
              assertWaitTimeout takeRequest
    , testCase "should work with url match" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          takeRequest <- armRequest page (UrlMatchRegex "digits\\/\\d\\.png" "")
          _ <- evaluate page "() => fetch('/digits/1.png')"
          request <- takeRequest
          url <- requestUrl request
          assertEqual "url" (prefix <> "/digits/1.png") (urlToText url)
    , testCase "should work with url match regular expression with flags" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          takeRequest <- armRequest page (UrlMatchRegex "DIGITS/2\\.PNG" "i")
          _ <- evaluate page "() => fetch('/digits/2.png')"
          request <- takeRequest
          url <- requestUrl request
          assertEqual "url" (prefix <> "/digits/2.png") (urlToText url)
    ]
