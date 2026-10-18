module Playwright.PageWaitForResponseSpec
  ( tests
  ) where

import Control.Monad
  ( when
  )
import Data.Text
  ( isInfixOf
  )
import Playwright
  ( UrlGlob (UrlGlob)
  , armResponse
  , armResponseWhere
  , armResponseWith
  , defaultGotoOptions
  , evaluate
  , goto
  , milliseconds
  , responseUrl
  , setDefaultTimeout
  , urlFromText
  , urlToText
  , waitForResponse
  , waitForResponseWith
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
import UnliftIO.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  )

tests :: TestTree
tests =
  testGroup
    "page-wait-for-response"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let wanted = prefix <> "/digits/2.png"
          takeResponse <- armResponse page (UrlGlob wanted)
          _ <-
            evaluate
              page
              "() => { void fetch('/digits/1.png'); void fetch('/digits/2.png'); void fetch('/digits/3.png'); }"
          response <- takeResponse
          url <- responseUrl response
          assertEqual "url" wanted (urlToText url)
    , testCase "should respect timeout" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case milliseconds 1 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertWaitTimeout (waitForResponseWith page (UrlGlob (prefix <> "/does-not-exist.png")) timeout)
    , testCase "should respect default timeout" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case milliseconds 1 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              setDefaultTimeout page timeout
              assertWaitTimeout (waitForResponse page (UrlGlob (prefix <> "/does-not-exist.png")))
    , testCase "should work with predicate" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let wanted = prefix <> "/digits/2.png"
          takeResponse <- armResponseWhere page (\response -> (== wanted) . urlToText <$> responseUrl response)
          _ <-
            evaluate
              page
              "() => { void fetch('/digits/1.png'); void fetch('/digits/2.png'); void fetch('/digits/3.png'); }"
          response <- takeResponse
          url <- responseUrl response
          assertEqual "url" wanted (urlToText url)
    , testCase "sync predicate should be only called once" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let wanted = prefix <> "/digits/1.png"
          calls <- newIORef (0 :: Int)
          -- Chromium may fetch /favicon.ico after goto, and that response can
          -- reach the waiter first. Only count the digit responses.
          takeResponse <- armResponseWhere page $ \response -> do
            current <- urlToText <$> responseUrl response
            when ("/digits/" `isInfixOf` current) (modifyIORef' calls (+ 1))
            pure (current == wanted)
          _ <-
            evaluate
              page
              "async () => { await fetch('/digits/1.png'); await fetch('/digits/2.png'); await fetch('/digits/3.png'); }"
          response <- takeResponse
          url <- responseUrl response
          assertEqual "url" wanted (urlToText url)
          assertEqual "calls" 1 =<< readIORef calls
    , testCase "should work with no timeout" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let wanted = prefix <> "/digits/2.png"
          case milliseconds 0 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              takeResponse <- armResponseWith page (UrlGlob wanted) timeout
              _ <-
                evaluate
                  page
                  "() => setTimeout(() => { void fetch('/digits/1.png'); void fetch('/digits/2.png'); void fetch('/digits/3.png'); }, 50)"
              response <- takeResponse
              url <- responseUrl response
              assertEqual "url" wanted (urlToText url)
    ]
