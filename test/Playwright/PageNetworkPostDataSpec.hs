module Playwright.PageNetworkPostDataSpec
  ( tests
  ) where

import UnliftIO.Exception
  ( try
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
  , requestPostData
  , requestPostDataJSON
  , urlFromText
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
                  <> "/title.html', { method: 'POST', body: ' ' }); return fetch(request); }"
              )
          result <- try (requestPostDataJSON request)
          case result of
            Left (DecodeFailure message) ->
              assertBool "message" ("POST data is not a valid JSON object: " `isInfixOf` message)
            Left err ->
              assertFailure ("expected DecodeFailure, got: " <> show err)
            Right value ->
              assertFailure ("expected DecodeFailure, got: " <> show value)
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

postTitle :: Page -> Text -> Text -> Playwright Request
postTitle page prefix script = do
  _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
  armRequest page (UrlMatchGlob (prefix <> "/title.html")) >>= \pending -> do
    _ <- evaluate page script
    pending
