module Playwright.PageGotoSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( defaultGotoOptions
  , evaluate
  , goto
  , url
  , urlFromText
  , urlToText
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
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-goto"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          let
            empty :: Text
            empty = prefix <> "/empty.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          current <- urlToText <$> url page
          assertEqual "url" empty current
    , testCase "page.url should work" $
        withAssets $ \prefix page -> do
          blank <- urlToText <$> url page
          assertEqual "blank" "about:blank" blank
          let
            empty :: Text
            empty = prefix <> "/empty.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          current <- urlToText <$> url page
          assertEqual "empty" empty current
    , testCase "page.url should include hashes" $
        withAssets $ \prefix page -> do
          let
            hashed :: Text
            hashed = prefix <> "/empty.html#hash"
          _ <- goto page (urlFromText hashed) defaultGotoOptions
          current <- urlToText <$> url page
          assertEqual "hash" hashed current
          _ <- evaluate page "() => { window.location.hash = 'dynamic'; }"
          updated <- urlToText <$> url page
          assertEqual "dynamic" (prefix <> "/empty.html#dynamic") updated
    ]
