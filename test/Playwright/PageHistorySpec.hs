module Playwright.PageHistorySpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( defaultGotoOptions
  , evaluate
  , goBack
  , goForward
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
    "page-history"
    [ testCase "page.goBack should work with HistoryAPI" $
        withAssets $ \prefix page -> do
          let
            empty :: Text
            empty = prefix <> "/empty.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { history.pushState({}, '', '/first.html'); history.pushState({}, '', '/second.html'); }"
          assertEqual "second" (prefix <> "/second.html") =<< (urlToText <$> url page)
          _ <- goBack page
          assertEqual "first" (prefix <> "/first.html") =<< (urlToText <$> url page)
          _ <- goBack page
          assertEqual "empty" empty =<< (urlToText <$> url page)
          _ <- goForward page
          assertEqual "first again" (prefix <> "/first.html") =<< (urlToText <$> url page)
    ]
