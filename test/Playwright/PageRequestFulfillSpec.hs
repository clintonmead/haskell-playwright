module Playwright.PageRequestFulfillSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , isSuffixOf
  , toLower
  )
import Playwright
  ( ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillHeaders, fulfillStatus)
  , HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  , HttpStatus (HttpStatus, httpStatusToInt)
  , UrlGlob (UrlGlob)
  , defaultFulfillOptions
  , defaultGotoOptions
  , evaluateText
  , fulfillRoute
  , goto
  , requestUrl
  , responseHeaders
  , responseStatus
  , responseStatusText
  , route
  , routeRequest
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( withAssets
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
    "page-request-fulfill"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          route page (UrlGlob "**/*") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 201)
                , fulfillHeaders = [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
                , fulfillContentType = Just (ContentType "text/html")
                , fulfillBody = Just (FulfillText "Yo, page!")
                }
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              status <- responseStatus value
              assertEqual "status" 201 (httpStatusToInt status)
              headers <- responseHeaders value
              assertEqual "foo" (Just "bar") (headerValue "foo" headers)
          body <- evaluateText page "() => document.body.textContent"
          assertEqual "body" "Yo, page!" body
    , testCase "should work with status code 422" $
        withAssets $ \prefix page -> do
          route page (UrlGlob "**/*") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 422)
                , fulfillBody = Just (FulfillText "Yo, page!")
                }
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              status <- responseStatus value
              assertEqual "status" 422 (httpStatusToInt status)
              text <- responseStatusText value
              assertEqual "statusText" "Unprocessable Entity" text
          body <- evaluateText page "() => document.body.textContent"
          assertEqual "body" "Yo, page!" body
    , testCase "should support cors with GET" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          route page (UrlGlob "**/cars*") $ \handled -> do
            request <- routeRequest handled
            url <- requestUrl request
            let
              origin :: Text
              origin =
                if "allow" `isSuffixOf` urlToText url
                  then "*"
                  else "none"
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "application/json")
                , fulfillHeaders =
                    [HttpHeader (HeaderName "access-control-allow-origin") (HeaderValue origin)]
                , fulfillStatus = Just (HttpStatus 200)
                , fulfillBody = Just (FulfillText "[\"electric\",\"gas\"]")
                }
          allowed <-
            evaluateText
              page
              "async () => { const response = await fetch('https://example.com/cars?allow', { mode: 'cors' }); return JSON.stringify(await response.json()); }"
          assertEqual "allowed" "[\"electric\",\"gas\"]" allowed
    ]

headerValue :: Text -> [HttpHeader] -> Maybe Text
headerValue name headers =
  case fmap (headerValueToText . httpHeaderValue)
    ( filter
        ( \header ->
            toLower (headerNameToText (httpHeaderName header)) == name
        )
        headers
    ) of
    [value] ->
      Just value
    _ ->
      Nothing
