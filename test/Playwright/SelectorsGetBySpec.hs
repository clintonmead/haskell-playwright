module Playwright.SelectorsGetBySpec
  ( tests
  ) where

import Data.Text
  ( isInfixOf
  )
import Playwright
  ( AriaRole (AriaRole)
  , Exactness (Exact, Inexact)
  , defaultGetByRoleOptions
  , evaluateOnLocatorAllTexts
  , evaluateOnLocatorText
  , getByLabel
  , getByRole
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
    , testCase "getByRole should work" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<a href=\"https://playwright.dev\">issues 123</a><a href=\"https://playwright.dev\">he llo 56</a><button>Click me</button>"
          buttons <-
            evaluateOnLocatorAllTexts
              (getByRole page (AriaRole "button") defaultGetByRoleOptions)
              "els => els.map(e => e.outerHTML)"
          assertEqual "button" ["<button>Click me</button>"] buttons
          links <-
            evaluateOnLocatorAllTexts
              (getByRole page (AriaRole "link") defaultGetByRoleOptions)
              "els => els.map(e => e.outerHTML)"
          assertEqual
            "links"
            [ "<a href=\"https://playwright.dev\">issues 123</a>"
            , "<a href=\"https://playwright.dev\">he llo 56</a>"
            ]
            links
    ]
