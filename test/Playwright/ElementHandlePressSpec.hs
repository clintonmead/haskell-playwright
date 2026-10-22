module Playwright.ElementHandlePressSpec
  ( tests
  ) where

import Playwright
  ( Key (Key)
  , evaluateText
  , evaluateOnLocatorText
  , evaluateText
  , press
  , setContent
  )
import Playwright.Harness
  ( locate
  , withChromiumPage
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
    "elementhandle-press"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' />"
          press (locate page "input") (Key "h")
          assertEqual "value" "h" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
    , testCase "should not select existing value" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' value='hello' />"
          press (locate page "input") (Key "w")
          assertEqual "value" "whello" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
    , testCase "should reset selection when not focused" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' value='hello' /><div tabIndex=2>text</div>"
          _ <-
            evaluateText
              page
              "() => { const input = document.querySelector('input'); input.selectionStart = 2; input.selectionEnd = 4; document.querySelector('div').focus(); return 'set'; }"
          press (locate page "input") (Key "w")
          assertEqual "value" "whello" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
    , testCase "should work with number input" $
        withChromiumPage $ \page -> do
          setContent page "<input type='number' value=2 />"
          press (locate page "input") (Key "1")
          assertEqual "value" "12" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
    ]
