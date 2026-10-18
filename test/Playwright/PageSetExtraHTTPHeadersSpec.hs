module Playwright.PageSetExtraHTTPHeadersSpec
  ( tests
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( pack
  )
import Network.HTTP.Types
  ( hContentType
  , hLocation
  , status200
  , status302
  )
import Network.Wai
  ( Application
  , pathInfo
  , requestHeaders
  , responseLBS
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader)
  , defaultGotoOptions
  , evaluateText
  , goto
  , pageContext
  , setContextExtraHTTPHeaders
  , setExtraHTTPHeaders
  , urlFromText
  )
import Playwright.Harness
  ( withChromiumPage
  , withLocalApp
  , assertEqual
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
    "page-set-extra-http-headers"
    [ testCase "should work" $
        withApplication (pure echoFooHeader) $ \port ->
          withChromiumPage $ \page -> do
            setExtraHTTPHeaders
              page
              [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
            _ <-
              goto
                page
                (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                defaultGotoOptions
            assertEqual "foo" "bar" =<< evaluateText page "() => document.body.innerText"
    , testCase "should work with redirects" $
        withChromiumPage $ \page ->
          withLocalApp redirectEchoApp $ \prefix -> do
            setExtraHTTPHeaders
              page
              [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
            _ <- goto page (urlFromText (prefix <> "/foo.html")) defaultGotoOptions
            assertEqual "foo" "bar" =<< evaluateText page "() => document.body.innerText"
    , testCase "should work with extra headers from browser context" $
        withChromiumPage $ \page ->
          withLocalApp echoFooHeader $ \prefix -> do
            setContextExtraHTTPHeaders
              (pageContext page)
              [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "foo" "bar" =<< evaluateText page "() => document.body.innerText"
    , testCase "should not duplicate referer header" $
        withChromiumPage $ \page ->
          withLocalApp echoRefererApp $ \prefix -> do
            let wanted = prefix <> "/empty.html"
            setExtraHTTPHeaders
              page
              [HttpHeader (HeaderName "referer") (HeaderValue wanted)]
            _ <- goto page (urlFromText wanted) defaultGotoOptions
            assertEqual "referer" wanted =<< evaluateText page "() => document.body.innerText"
    ]

echoFooHeader :: Application
echoFooHeader req respond =
  respond
    ( responseLBS
        status200
        [(hContentType, BS8.pack "text/plain")]
        (LBS8.fromStrict (fromMaybe BS8.empty (lookup "foo" (requestHeaders req))))
    )

-- | Redirects /foo.html to /empty.html. Every other path echoes foo.
redirectEchoApp :: Application
redirectEchoApp req respond =
  case pathInfo req of
    ["foo.html"] ->
      respond (responseLBS status302 [(hLocation, BS8.pack "/empty.html")] LBS8.empty)
    _ ->
      echoFooHeader req respond

-- | Echoes every referer header the server received, joined by commas, so a
-- duplicate shows up as a second value.
echoRefererApp :: Application
echoRefererApp req respond =
  respond
    ( responseLBS
        status200
        [(hContentType, BS8.pack "text/plain")]
        (LBS8.fromStrict (BS8.intercalate (BS8.pack ",") [value | (name, value) <- requestHeaders req, name == "referer"]))
    )
