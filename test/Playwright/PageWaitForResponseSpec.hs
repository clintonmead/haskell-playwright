module Playwright.PageWaitForResponseSpec
  ( tests
  ) where

import Control.Monad
  ( when
  )
import Control.Monad.IO.Class
  ( MonadIO
  )
import Data.Text
  ( Text
  , isInfixOf
  )
import Playwright
  ( UrlMatch (UrlMatchGlob, UrlMatchRegex)
  , armResponse
  , armResponseWhere
  , armResponseWith
  , defaultGotoOptions
  , evaluate
  , goto
  , milliseconds
  , responseText
  , responseUrl
  , setDefaultTimeout
  , urlFromText
  , urlToText
  , waitForResponse
  , waitForResponseWith
  )
import Playwright.Error
  ( PlaywrightError (WaitTimeoutLog)
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
import UnliftIO.Exception
  ( try
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
          takeResponse <- armResponse page (UrlMatchGlob wanted)
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
              assertWaitTimeout (waitForResponseWith page (UrlMatchGlob (prefix <> "/does-not-exist.png")) timeout)
    , testCase "should respect default timeout" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case milliseconds 1 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              setDefaultTimeout page timeout
              assertWaitTimeout (waitForResponse page (UrlMatchGlob (prefix <> "/does-not-exist.png")))
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
    , testCase "should work with async predicate" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          takeFile <-
            armResponseWhere page $ \response ->
              ("contents of the file" `isInfixOf`) <$> responseText response
          takeJson <-
            armResponseWhere page $ \response ->
              ("bar" `isInfixOf`) <$> responseText response
          _ <-
            evaluate
              page
              "() => { void fetch('/simple.json').then(r => r.json()); void fetch('/file-to-upload.txt').then(r => r.text()); }"
          fileResponse <- takeFile
          jsonResponse <- takeJson
          fileUrl <- responseUrl fileResponse
          jsonUrl <- responseUrl jsonResponse
          assertEqual "file url" (prefix <> "/file-to-upload.txt") (urlToText fileUrl)
          assertEqual "json url" (prefix <> "/simple.json") (urlToText jsonUrl)
    , testCase "should work with no timeout" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let wanted = prefix <> "/digits/2.png"
          case milliseconds 0 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              takeResponse <- armResponseWith page (UrlMatchGlob wanted) timeout
              _ <-
                evaluate
                  page
                  "() => setTimeout(() => { void fetch('/digits/1.png'); void fetch('/digits/2.png'); void fetch('/digits/3.png'); }, 50)"
              response <- takeResponse
              url <- responseUrl response
              assertEqual "url" wanted (urlToText url)
    , testCase "should log the url" $
        withAssets $ \_ page ->
          case milliseconds 1000 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              globResult <- try (waitForResponseWith page (UrlMatchGlob "foo.css") timeout)
              assertLoggedWait "waiting for response \"foo.css\"" globResult
              regexResult <- try (waitForResponseWith page (UrlMatchRegex "foo.css" "i") timeout)
              assertLoggedWait "waiting for response /foo.css/i" regexResult
    ]
  where
    assertLoggedWait :: (MonadIO f) => Text -> Either PlaywrightError b -> f ()
    assertLoggedWait needle result =
      case result of
        Left (WaitTimeoutLog _ logLine) ->
          when (not (needle `isInfixOf` logLine)) $
            assertFailure ("call log " <> show logLine <> " does not contain " <> show needle)
        Left err ->
          assertFailure ("expected WaitTimeoutLog, got: " <> show err)
        Right _ ->
          assertFailure "expected WaitTimeoutLog"
