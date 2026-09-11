module Playwright.PageRequestFulfillSpec
  ( tests
  ) where

import Data.Text
  ( Text
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
  , responseHeaders
  , responseStatus
  , route
  , urlFromText
  )
import Playwright.Harness
  ( withAssets
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
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
