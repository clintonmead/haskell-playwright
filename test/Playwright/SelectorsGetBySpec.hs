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
  , getByAltText
  , getByAltTextRegex
  , getByLabel
  , getByPlaceholder
  , getByPlaceholderRegex
  , getByRole
  , getByTestId
  , getByTestIdAttr
  , getByTestIdRegex
  , getByText
  , getByTitle
  , getByTitleRegex
  , innerText
  , locatorCount
  , setContent
  )
import Playwright.Harness
  ( withChromiumPage
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
    , testCase "getByTestId with custom testId should work" $
        withChromiumPage $ \page -> do
          setContent page "<div><div data-my-custom-testid=\"Hello\">Hello world</div></div>"
          assertEqual "text" "Hello world" =<< innerText (getByTestIdAttr page "data-my-custom-testid" "Hello")
    , testCase "getByTestId with comma-separated testIdAttributes should match any" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<section><div data-pw=\"Hello\">first</div><div data-ti=\"Hello\">second</div><div data-testid=\"Hello\">third</div></section>"
          assertEqual "count" 2 =<< locatorCount (getByTestIdAttr page "data-pw,data-ti" "Hello")
          texts <- evaluateOnLocatorAllTexts (getByTestIdAttr page "data-pw,data-ti" "Hello") "els => els.map(e => e.textContent)"
          assertEqual "texts" ["first", "second"] texts
    , testCase "getByTestId should work for regex" $
        withChromiumPage $ \page -> do
          setContent page "<div><div data-testid=\"Hello\">Hello world</div></div>"
          assertEqual "text" "Hello world" =<< innerText (getByTestIdRegex page "/He[l]*o/")
    , testCase "getByTestId should escape id" $
        withChromiumPage $ \page -> do
          setContent page "<div><div data-testid='He\"llo'>Hello world</div></div>"
          assertEqual "text" "Hello world" =<< innerText (getByTestId page "He\"llo")
    , testCase "getByText should work" $
        withChromiumPage $ \page -> do
          setContent page "<div>yo</div><div>ya</div><div>\nye  </div>"
          html <- evaluateOnLocatorText (getByText page "ye" Inexact) "e => e.outerHTML"
          assertEqual "ye" True (">\nye  </div>" `isInfixOf` html)
    , testCase "getByPlaceholder should work" $
        withChromiumPage $ \page -> do
          setContent page "<div><input placeholder='Hello'><input placeholder='Hello World'></div>"
          assertEqual "inexact" 2 =<< locatorCount (getByPlaceholder page "hello" Inexact)
          assertEqual "exact" 1 =<< locatorCount (getByPlaceholder page "Hello" Exact)
          assertEqual "regex" 1 =<< locatorCount (getByPlaceholderRegex page "/wor/i")
    , testCase "getByAltText should work" $
        withChromiumPage $ \page -> do
          setContent page "<div><input alt='Hello'><input alt='Hello World'></div>"
          assertEqual "inexact" 2 =<< locatorCount (getByAltText page "hello" Inexact)
          assertEqual "exact" 1 =<< locatorCount (getByAltText page "Hello" Exact)
          assertEqual "regex" 1 =<< locatorCount (getByAltTextRegex page "/wor/i")
    , testCase "getByTitle should work" $
        withChromiumPage $ \page -> do
          setContent page "<div><input title='Hello'><input title='Hello World'></div>"
          assertEqual "inexact" 2 =<< locatorCount (getByTitle page "hello" Inexact)
          assertEqual "exact" 1 =<< locatorCount (getByTitle page "Hello" Exact)
          assertEqual "regex" 1 =<< locatorCount (getByTitleRegex page "/wor/i")
    ]
