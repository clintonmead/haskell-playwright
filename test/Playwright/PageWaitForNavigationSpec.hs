module Playwright.PageWaitForNavigationSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Data.Text
  ( isInfixOf
  )
import Playwright
  ( defaultGotoOptions
  , evaluateTextWith
  , goto
  , responseOk
  , responseUrl
  , textArgument
  , urlFromText
  , urlToText
  , waitForNavigation
  )
import Playwright.Harness
  ( withAssets
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-wait-for-navigation"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForNavigation page) $ \nav -> do
            _ <-
              evaluateTextWith
                page
                "url => { window.location.href = url; return url; }"
                (textArgument (prefix <> "/grid.html"))
            response <- wait nav
            case response of
              Nothing ->
                assertFailure "expected navigation response"
              Just value -> do
                assertEqual "ok" True =<< responseOk value
                url <- responseUrl value
                assertBool "grid.html" ("grid.html" `isInfixOf` urlToText url)
    ]
