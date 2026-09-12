module Playwright.PageNavigationSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Data.Text
  ( replace
  )
import Playwright
  ( ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , UrlGlob (UrlGlob)
  , click
  , closePage
  , defaultFulfillOptions
  , defaultGotoOptions
  , fulfillRoute
  , goto
  , route
  , setContent
  , urlFromText
  , waitForPopup
  )
import Playwright.Harness
  ( locate
  , withAssets
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
    ]
