module Playwright.QuerySelectorSpec
  ( tests
  ) where

import Data.Maybe
  ( isJust
  , isNothing
  )
import Playwright
  ( evaluateHandle
  , evaluateOnElementBoolWith
  , evaluateOnElementText
  , jsHandleArgument
  , querySelector
  , querySelectorAll
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

-- Skipped: "should throw for non-string selector". The upstream case passes `null` to
-- page.$ and checks for a "selector: expected string, got object" error. The Haskell
-- API takes a Selector newtype (built with selectorFromText), so a non-string selector
-- cannot be constructed and there is no call site to exercise.
tests :: TestTree
tests =
  testGroup
    "queryselector"
    [ testCase "should query existing element with css selector" $
        withChromiumPage $ \page -> do
          setContent page "<section>test</section>"
          handle <- querySelector page (selectorFromText "css=section")
          assertEqual "found" True (isJust handle)
    , testCase "should query existing element with text selector" $
        withChromiumPage $ \page -> do
          setContent page "<section>test</section>"
          handle <- querySelector page (selectorFromText "text=\"test\"")
          assertEqual "found" True (isJust handle)
    , testCase "should query existing element with xpath selector" $
        withChromiumPage $ \page -> do
          setContent page "<section>test</section>"
          handle <- querySelector page (selectorFromText "xpath=/html/body/section")
          assertEqual "found" True (isJust handle)
    , testCase "should return null for non-existing element" $
        withChromiumPage $ \page -> do
          handle <- querySelector page (selectorFromText "non-existing-element")
          assertEqual "missing" True (isNothing handle)
    , testCase "should auto-detect xpath selector" $
        withChromiumPage $ \page -> do
          setContent page "<section>test</section>"
          handle <- querySelector page (selectorFromText "//html/body/section")
          assertEqual "found" True (isJust handle)
    , testCase "should auto-detect xpath selector with starting parenthesis" $
        withChromiumPage $ \page -> do
          setContent page "<section>test</section>"
          handle <- querySelector page (selectorFromText "(//section)[1]")
          assertEqual "found" True (isJust handle)
    , testCase "should auto-detect xpath selector starting with .." $
        withChromiumPage $ \page -> do
          setContent page "<div><section>test</section><span></span></div>"
          spanHandle <- requireHandle =<< querySelector page (selectorFromText "\"test\" >> ../span")
          assertEqual "span" "SPAN" =<< evaluateOnElementText spanHandle "e => e.nodeName"
          divHandle <- requireHandle =<< querySelector page (selectorFromText "\"test\" >> ..")
          assertEqual "div" "DIV" =<< evaluateOnElementText divHandle "e => e.nodeName"
    , testCase "should auto-detect text selector" $
        withChromiumPage $ \page -> do
          setContent page "<section>test</section>"
          handle <- querySelector page (selectorFromText "\"test\"")
          assertEqual "found" True (isJust handle)
    , testCase "should auto-detect css selector" $
        withChromiumPage $ \page -> do
          setContent page "<section>test</section>"
          handle <- querySelector page (selectorFromText "section")
          assertEqual "found" True (isJust handle)
    , testCase "should support >> syntax" $
        withChromiumPage $ \page -> do
          setContent page "<section><div>test</div></section>"
          handle <- querySelector page (selectorFromText "css=section >> css=div")
          assertEqual "found" True (isJust handle)
    , testCase "should query existing elements" $
        withChromiumPage $ \page -> do
          setContent page "<div>A</div><br/><div>B</div>"
          elements <- querySelectorAll page (selectorFromText "div")
          assertEqual "count" 2 (length elements)
          texts <- mapM (\element -> evaluateOnElementText element "e => e.textContent") elements
          assertEqual "texts" ["A", "B"] texts
    , testCase "should return empty array if nothing is found" $
        withChromiumPage $ \page -> do
          elements <- querySelectorAll page (selectorFromText "div")
          assertEqual "count" 0 (length elements)
    , testCase "xpath should query existing element" $
        withChromiumPage $ \page -> do
          setContent page "<section>test</section>"
          elements <- querySelectorAll page (selectorFromText "xpath=/html/body/section")
          assertEqual "count" 1 (length elements)
    , testCase "xpath should return empty array for non-existing element" $
        withChromiumPage $ \page -> do
          elements <- querySelectorAll page (selectorFromText "//html/body/non-existing-element")
          assertEqual "count" 0 (length elements)
    , testCase "xpath should return multiple elements" $
        withChromiumPage $ \page -> do
          setContent page "<div></div><div></div>"
          elements <- querySelectorAll page (selectorFromText "xpath=/html/body/div")
          assertEqual "count" 2 (length elements)
    , testCase "$$ should work with bogus Array.from" $
        withChromiumPage $ \page -> do
          setContent page "<div>hello</div><div></div>"
          div1 <- evaluateHandle page "() => { Array.from = () => []; return document.querySelector('div'); }"
          elements <- querySelectorAll page (selectorFromText "div")
          assertEqual "count" 2 (length elements)
          case elements of
            first : _ ->
              assertEqual "same element" True
                =<< evaluateOnElementBoolWith first "(div, div1) => div === div1" (jsHandleArgument div1)
            [] ->
              assertFailure "expected elements"
    ]
