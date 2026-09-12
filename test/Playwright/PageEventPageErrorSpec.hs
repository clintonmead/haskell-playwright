module Playwright.PageEventPageErrorSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Data.Text
  ( isInfixOf
  )
import Playwright
  ( PageError (pageErrorMessage, pageErrorName, pageErrorStack)
  , defaultGotoOptions
  , evaluate
  , goto
  , urlFromText
  , waitForPageError
  )
import Playwright.Harness
  ( withAssets
  , withChromiumPage
  , assertBool
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
    "page-event-pageerror"
    [ testCase "should fire" $
        withAssets $ \prefix page ->
          withAsync (waitForPageError page) $ \handle -> do
            _ <- goto page (urlFromText (prefix <> "/error.html")) defaultGotoOptions
            err <- wait handle
            assertEqual "name" "Error" (pageErrorName err)
            assertEqual "message" "Fancy error!" (pageErrorMessage err)
    , testCase "should contain sourceURL" $
        withAssets $ \prefix page ->
          withAsync (waitForPageError page) $ \handle -> do
            _ <- goto page (urlFromText (prefix <> "/error.html")) defaultGotoOptions
            err <- wait handle
            assertBool "myscript.js" ("myscript.js" `isInfixOf` maybe "" id (pageErrorStack err))
    , testCase "should contain the Error.name property" $
        withChromiumPage $ \page ->
          withAsync (waitForPageError page) $ \handle -> do
            _ <-
              evaluate
                page
                "() => { setTimeout(() => { const error = new Error('my-message'); error.name = 'my-name'; throw error; }, 0); }"
            err <- wait handle
            assertEqual "name" "my-name" (pageErrorName err)
            assertEqual "message" "my-message" (pageErrorMessage err)
    ]
