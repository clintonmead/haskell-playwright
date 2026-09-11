module Playwright.QuerySelectorSpec
  ( tests
  ) where

import Data.Maybe
  ( isJust
  , isNothing
  )
import Playwright
  ( evaluateOnElementText
  , querySelector
  , selectorFromText
  , setContent
  )
import Playwright.Harness
  ( requireHandle
  , withChromiumPage
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
    ]
