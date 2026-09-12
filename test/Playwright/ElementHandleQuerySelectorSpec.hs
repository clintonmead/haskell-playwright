module Playwright.ElementHandleQuerySelectorSpec
  ( tests
  ) where

import Data.Maybe
  ( isNothing
  )
import Playwright
  ( elementArgument
  , evaluateTextWith
  , querySelector
  , querySelectorAllHandle
  , querySelectorHandle
  , selectorFromText
  , setContent
  )
import Playwright.Harness
  ( requireHandle
  , withChromiumPage
  , assertEqual
  , assertFailure
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
    "elementhandle-query-selector"
    [ testCase "should query existing element" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><div class=\"second\"><div class=\"inner\">A</div></div></body></html>"
          html <- requireHandle =<< querySelector page (selectorFromText "html")
          second <- requireHandle =<< querySelectorHandle html (selectorFromText ".second")
          inner <- requireHandle =<< querySelectorHandle second (selectorFromText ".inner")
          assertEqual "text" "A" =<< evaluateTextWith page "e => e.textContent" (elementArgument inner)
    , testCase "should return null for non-existing element" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><div class=\"second\"><div class=\"inner\">B</div></div></body></html>"
          html <- requireHandle =<< querySelector page (selectorFromText "html")
          second <- querySelectorHandle html (selectorFromText ".third")
          assertEqual "missing" True (isNothing second)
    , testCase "should query existing elements" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><div>A</div><br/><div>B</div></body></html>"
          html <- requireHandle =<< querySelector page (selectorFromText "html")
          elements <- querySelectorAllHandle html (selectorFromText "div")
          assertEqual "count" 2 (length elements)
          texts <- traverse (\el -> evaluateTextWith page "e => e.textContent" (elementArgument el)) elements
          assertEqual "texts" ["A", "B"] texts
    , testCase "should return empty array for non-existing elements" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><span>A</span><br/><span>B</span></body></html>"
          html <- requireHandle =<< querySelector page (selectorFromText "html")
          elements <- querySelectorAllHandle html (selectorFromText "div")
          assertEqual "empty" 0 (length elements)
    , testCase "xpath should query existing element" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><div class=\"second\"><div class=\"inner\">A</div></div></body></html>"
          html <- requireHandle =<< querySelector page (selectorFromText "html")
          seconds <- querySelectorAllHandle html (selectorFromText "xpath=./body/div[contains(@class, 'second')]")
          case seconds of
            [second] -> do
              inners <- querySelectorAllHandle second (selectorFromText "xpath=./div[contains(@class, 'inner')]")
              case inners of
                [inner] ->
                  assertEqual "text" "A" =<< evaluateTextWith page "e => e.textContent" (elementArgument inner)
                _ ->
                  assertFailure "expected one inner"
            _ ->
              assertFailure "expected one second"
    , testCase "xpath should return null for non-existing element" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><div class=\"second\"><div class=\"inner\">B</div></div></body></html>"
          html <- requireHandle =<< querySelector page (selectorFromText "html")
          second <- querySelectorAllHandle html (selectorFromText "xpath=/div[contains(@class, 'third')]")
          assertEqual "empty" 0 (length second)
    ]
