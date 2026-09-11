module Playwright.PageHistorySpec
  ( tests
  ) where

import Data.Text
  ( Text
  , isInfixOf
  )
import Playwright
  ( SpecialJsValue (SpecialUndefined)
  , defaultGotoOptions
  , evaluate
  , evaluateSpecial
  , goBack
  , goForward
  , goto
  , reload
  , responseOk
  , responseUrl
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
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-history"
    [ testCase "page.goBack should work" $
        withAssets $ \prefix page -> do
          none <- goBack page
          case none of
            Nothing ->
              pure ()
            Just _ ->
              assertFailure "expected no history"
          let
            empty :: Text
            empty = prefix <> "/empty.html"
            grid :: Text
            grid = prefix <> "/grid.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          _ <- goto page (urlFromText grid) defaultGotoOptions
          back <- goBack page
          case back of
            Nothing ->
              assertFailure "expected goBack response"
            Just response -> do
              assertEqual "ok" True =<< responseOk response
              responseLocation <- urlToText <$> responseUrl response
              assertEqual "empty url" True (empty `isInfixOf` responseLocation)
          forward <- goForward page
          case forward of
            Nothing ->
              assertFailure "expected goForward response"
            Just response -> do
              assertEqual "ok" True =<< responseOk response
              responseLocation <- urlToText <$> responseUrl response
              assertEqual "grid url" True ("/grid.html" `isInfixOf` responseLocation)
          noneAgain <- goForward page
          case noneAgain of
            Nothing ->
              pure ()
            Just _ ->
              assertFailure "expected end of history"
    , testCase "page.goBack should work with HistoryAPI" $
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
    , testCase "page.reload should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- evaluate page "() => { window._foo = 10; }"
          _ <- reload page
          special <- evaluateSpecial page "() => window._foo"
          assertEqual "cleared" SpecialUndefined special
    ]
