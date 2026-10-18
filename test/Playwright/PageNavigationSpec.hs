module Playwright.PageNavigationSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text
  ( replace
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  , status404
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Playwright
  ( ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillHeaders, fulfillStatus)
  , HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader)
  , HttpStatus (HttpStatus)
  , UrlGlob (UrlGlob)
  , armNavigationOptions
  , click
  , closePage
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultWaitForNavigationOptions
  , fulfillRoute
  , goto
  , route
  , setContent
  , title
  , url
  , urlFromText
  , urlToText
  , waitForPopup
  )
import Playwright.Harness
  ( assertEqual
  , locate
  , withAssets
  , withChromiumPage
  , withLocalApp
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
    "page-navigation"
    [ testCase "should work with _blank target" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent
            page
            ("<a href=\"" <> prefix <> "/empty.html\" target=\"_blank\">Click me</a>")
          withAsync (waitForPopup page) $ \pending -> do
            click (locate page "\"Click me\"")
            popup <- wait pending
            closePage popup
    , testCase "should work with cross-process _blank target" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            cross = replace "127.0.0.1" "localhost" prefix
          setContent
            page
            ("<a href=\"" <> cross <> "/empty.html\" target=\"_blank\">Click me</a>")
          withAsync (waitForPopup page) $ \pending -> do
            click (locate page "\"Click me\"")
            popup <- wait pending
            closePage popup
    , testCase "should work with _blank target in form" $
        withAssets $ \prefix page -> do
          route page (UrlGlob "**/done.html") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "text/html")
                , fulfillBody = Just (FulfillText "Done")
                }
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent
            page
            "<form target=\"_blank\" action=\"done.html\" ><input type=\"submit\" value=\"Click me\"></form>"
          withAsync (waitForPopup page) $ \pending -> do
            click (locate page "\"Click me\"")
            popup <- wait pending
            closePage popup
          setContent
            page
            "<form target=\"_blank\" action=\"done.html\" method=\"post\"><input type=\"submit\" value=\"Click me\"></form>"
          withAsync (waitForPopup page) $ \pending -> do
            click (locate page "\"Click me\"")
            popup <- wait pending
            closePage popup
    , testCase "should not throw TargetClosedException on cross-origin redirect after click" $
        withChromiumPage $ \page ->
          withLocalApp titleApp $ \prefix -> do
            let
              cross = replace "127.0.0.1" "localhost" prefix
            route page (UrlGlob "**/redirect") $ \handled ->
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillStatus = Just (HttpStatus 302)
                  , fulfillHeaders = [HttpHeader (HeaderName "location") (HeaderValue (cross <> "/target.html"))]
                  }
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            setContent
              page
              ("<form action=\"" <> prefix <> "/redirect\" method=\"POST\"><button type=\"submit\">Submit</button></form>")
            takeNavigation <- armNavigationOptions page defaultWaitForNavigationOptions
            click (locate page "button")
            _ <- takeNavigation
            assertEqual "url" (cross <> "/target.html") . urlToText =<< url page
            assertEqual "title" "final page" =<< title page
    ]

-- | Serves the pages the cross-origin redirect lands on. It answers on any
-- Host, so the 127.0.0.1 and localhost origins both reach it.
titleApp :: Application
titleApp req respond =
  case pathInfo req of
    ["target.html"] ->
      respond (responseLBS status200 [(hContentType, BS8.pack "text/html")] (LBS8.pack "<title>final page</title>"))
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, BS8.pack "text/html")] LBS8.empty)
    _ ->
      respond (responseLBS status404 [] LBS8.empty)
