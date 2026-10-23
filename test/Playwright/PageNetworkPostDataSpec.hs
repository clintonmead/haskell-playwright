module Playwright.PageNetworkPostDataSpec
  ( tests
  ) where

import UnliftIO.Exception
  ( SomeException
  , try
  )
import Data.Aeson
  ( Value (String)
  , encode
  , object
  , (.=)
  )
import Data.ByteString.Lazy
  ( toStrict
  )
import Data.Text
  ( Text
  , isInfixOf
  )
import Playwright
  ( Page
  , Request
  , UrlMatch (UrlMatchGlob)
  , defaultGotoOptions
  , evaluate
  , goto
  , HttpMethod (httpMethodToText)
  , requestMethod
  , requestPostData
  , requestPostDataJSON
  , requestUrl
  , urlFromText
  , urlToText
  , armRequest
  , Playwright
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.Harness
  ( withAssets
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
    "network-post-data"
    [ testCase "should return correct postData buffer for utf-8 body" $
        withAssets $ \prefix page -> do
          request <-
            postTitle
              page
              prefix
              ( "() => { const value = 'baẞ'; const request = new Request('"
                  <> prefix
                  <> "/title.html', { method: 'POST', body: JSON.stringify(value) }); request.headers.set('content-type', 'application/json;charset=UTF-8'); return fetch(request); }"
              )
          payload <- requestPostData request
          assertEqual "buffer" (Just (toStrict (encode (String "baẞ")))) payload
          json <- requestPostDataJSON request
          assertEqual "json" (Just (String "baẞ")) json
    , testCase "should return post data w/o content-type" $
        withAssets $ \prefix page -> do
          request <-
            postTitle
              page
              prefix
              ( "() => { const request = new Request('"
                  <> prefix
                  <> "/title.html', { method: 'POST', body: JSON.stringify({ value: 42 }) }); request.headers.set('content-type', ''); return fetch(request); }"
              )
          json <- requestPostDataJSON request
          assertEqual "json" (Just (object ["value" .= (42 :: Int)])) json
    , testCase "should throw on invalid JSON in post data" $
        withAssets $ \prefix page -> do
          request <-
            postTitle
              page
              prefix
              ( "() => { const request = new Request('"
                  <> prefix
                  <> "/title.html', { method: 'POST', body: '<not a json>' }); return fetch(request); }"
              )
          result <- try (requestPostDataJSON request)
          case result of
            Left (DecodeFailure message) ->
              assertBool "message" ("POST data is not a valid JSON object: " `isInfixOf` message)
            Left err ->
              assertFailure ("expected DecodeFailure, got: " <> show err)
            Right value ->
              assertFailure ("expected DecodeFailure, got: " <> show value)
    , testCase "should get post data for file/blob" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          pending <- armRequest page (UrlMatchGlob "**/*")
          _ <-
            evaluate
              page
              "() => { const file = new File(['file-contents'], 'filename.txt'); void fetch('/data', { method: 'POST', headers: { 'content-type': 'application/octet-stream' }, body: file }); }"
          request <- pending
          -- Upstream marks this it.fail on Chromium: the file body is not
          -- reported as post data there, so the expected text must not match.
          expectUpstreamFailure $ do
            payload <- requestPostData request
            assertEqual "postData" (Just "file-contents") payload
    , testCase "should get post data for navigator.sendBeacon api calls" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          pending <- armRequest page (UrlMatchGlob "**/*")
          _ <-
            evaluate
              page
              "() => navigator.sendBeacon(window.location.origin + '/api/foo', new Blob([JSON.stringify({ foo: 'bar' })]))"
          request <- pending
          -- Upstream marks this it.fail on Chromium: postData is empty there.
          expectUpstreamFailure $ do
            method <- httpMethodToText <$> requestMethod request
            assertEqual "method" "POST" method
            url <- urlToText <$> requestUrl request
            assertEqual "url" (prefix <> "/api/foo") url
            json <- requestPostDataJSON request
            assertEqual "json" (Just (object ["foo" .= ("bar" :: Text)])) json
    , testCase "should return post data for PUT requests" $
        withAssets $ \prefix page -> do
          request <-
            postTitle
              page
              prefix
              ( "() => { const request = new Request('"
                  <> prefix
                  <> "/title.html', { method: 'PUT', body: JSON.stringify({ value: 42 }) }); return fetch(request); }"
              )
          json <- requestPostDataJSON request
          assertEqual "json" (Just (object ["value" .= (42 :: Int)])) json
    ]

-- Mirrors Playwright's it.fail: the wrapped checks must throw, and a pass
-- means the known Chromium defect is still present.
expectUpstreamFailure :: Playwright () -> Playwright ()
expectUpstreamFailure action = do
  result <- try action :: Playwright (Either SomeException ())
  case result of
    Left _ -> pure ()
    Right () -> assertFailure "expected the upstream it.fail check to fail"

postTitle :: Page -> Text -> Text -> Playwright Request
postTitle page prefix script = do
  _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
  armRequest page (UrlMatchGlob (prefix <> "/title.html")) >>= \pending -> do
    _ <- evaluate page script
    pending
