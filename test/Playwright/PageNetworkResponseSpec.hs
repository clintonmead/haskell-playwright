module Playwright.PageNetworkResponseSpec
  ( tests
  ) where

import Codec.Compression.GZip
  ( compress
  )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe
  ( listToMaybe
  )
import Data.Text
  ( Text
  , toLower
  )
import Network.HTTP.Types
  ( hContentEncoding
  , hContentType
  , mkStatus
  , status200
  , status404
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Playwright
  ( HeaderName (headerNameToText)
  , HeaderValue (headerValueToText)
  , HttpHeader (httpHeaderName, httpHeaderValue)
  , defaultGotoOptions
  , goto
  , responseAllHeaders
  , responseHeaders
  , responseStatusText
  , responseText
  , urlFromText
  )
import Playwright.Harness
  ( withAssets
  , withChromiumPage
  , withLocalApp
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
    "page-network-response"
    [ testCase "should return text" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/simple.json")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "body" "{\"foo\": \"bar\"}\n" =<< responseText value
    , testCase "should work @smoke" $
        withChromiumPage $ \page ->
          withLocalApp headerApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                headers <- responseHeaders value
                assertEqual "foo" (Just "bar") (headerValue "foo" headers)
                assertEqual "baz" (Just "bAz") (headerValue "baz" headers)
    , testCase "should return uncompressed text" $
        withChromiumPage $ \page ->
          withLocalApp gzipApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/simple.json")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value ->
                -- responseHeaders does not list content-encoding (see TODO), so check the decoded body.
                assertEqual "body" "{\"foo\": \"bar\"}\n" =<< responseText value
    , testCase "allHeaders should include content-encoding" $
        withChromiumPage $ \page ->
          withLocalApp gzipApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/simple.json")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                headers <- responseAllHeaders value
                assertEqual "content-encoding" (Just "gzip") (headerValue "content-encoding" headers)
    , testCase "allHeaders should lowercase header names" $
        withChromiumPage $ \page ->
          withLocalApp headerApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                headers <- responseAllHeaders value
                assertEqual "baz" (Just "bAz") (headerValue "baz" headers)
                assertEqual
                  "uppercase names"
                  []
                  [ name
                  | header <- headers
                  , let name = headerNameToText (httpHeaderName header)
                  , name /= toLower name
                  ]
    , testCase "should return status text" $
        withChromiumPage $ \page ->
          withLocalApp coolApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/cool")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value ->
                assertEqual "status text" "cool!" =<< responseStatusText value
    , testCase "should return text for identity encoding" $
        withChromiumPage $ \page ->
          withLocalApp identityApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/identity.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value ->
                assertEqual "body" "<div>hello</div>" =<< responseText value
    ]

headerValue :: Text -> [HttpHeader] -> Maybe Text
headerValue name headers =
  listToMaybe
    [ headerValueToText (httpHeaderValue header)
    | header <- headers
    , toLower (headerNameToText (httpHeaderName header)) == toLower name
    ]

headerApp :: Application
headerApp _ respond =
  respond (responseLBS status200 [("foo", "bar"), ("BaZ", "bAz")] LBS8.empty)

gzipApp :: Application
gzipApp request respond =
  case pathInfo request of
    ["simple.json"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "application/json"), (hContentEncoding, "gzip")]
            (compress (LBS8.pack "{\"foo\": \"bar\"}\n"))
        )
    _ ->
      respond (responseLBS status404 [] LBS8.empty)

coolApp :: Application
coolApp _ respond =
  respond (responseLBS (mkStatus 200 "cool!") [] LBS8.empty)

identityApp :: Application
identityApp request respond =
  case pathInfo request of
    ["identity.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html; charset=utf-8"), ("Content-Encoding", "Identity")]
            (LBS8.pack "<div>hello</div>")
        )
    _ ->
      respond (responseLBS status404 [] LBS8.empty)
