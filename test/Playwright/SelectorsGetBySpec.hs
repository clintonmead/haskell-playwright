module Playwright.SelectorsGetBySpec
  ( tests
  ) where

import Data.Text
  ( isInfixOf
  )
import Playwright
  ( Exactness (Exact, Inexact)
  , evaluateOnLocatorText
  , getByLabel
  , getByTestId
  , getByText
  , innerText
  , setContent
  )
import Playwright.Harness
  ( withChromiumPage
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
    "selectors-get-by"
    [ testCase "getByTestId should work" $
        withChromiumPage $ \page -> do
          setContent page "<div><div data-testid=\"Hello\">Hello world</div></div>"
          assertEqual "text" "Hello world" =<< innerText (getByTestId page "Hello")
    , testCase "getByText exact should work" $
        withChromiumPage $ \page -> do
          setContent page "<div>Hello world</div><div>Hello</div>"
          html <- evaluateOnLocatorText (getByText page "Hello" Exact) "e => e.outerHTML"
          assertEqual "exact hello" True (">Hello</div>" `isInfixOf` html)
    , testCase "getByLabel should work" $
        withChromiumPage $ \page -> do
          setContent page "<div><label for=target>Name</label><input id=target type=text></div>"
          assertEqual "text is label" "LABEL"
            =<< evaluateOnLocatorText (getByText page "Name" Inexact) "e => e.nodeName"
          assertEqual "label is input" "INPUT"
            =<< evaluateOnLocatorText (getByLabel page "Name" Inexact) "e => e.nodeName"
    ]
