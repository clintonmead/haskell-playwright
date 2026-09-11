module Playwright.ElementHandleTypeSpec
  ( tests
  ) where

import Playwright
  ( evaluateOnLocatorText
  , querySelector
  , selectorFromText
  , setContent
  , typeHandle
  )
import Playwright.Harness
  ( locate
  , requireHandle
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
    "elementhandle-type"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' />"
          handle <- requireHandle =<< querySelector page (selectorFromText "input")
          typeHandle handle "hello"
          assertEqual "value" "hello" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
    , testCase "should not select existing value" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' value='hello' />"
          handle <- requireHandle =<< querySelector page (selectorFromText "input")
          typeHandle handle "world"
          assertEqual "value" "worldhello" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
    , testCase "should reset selection when not focused" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' value='hello' /><div tabIndex=2>text</div>"
          _ <-
            evaluateOnLocatorText
              (locate page "input")
              "input => { input.selectionStart = 2; input.selectionEnd = 4; document.querySelector('div').focus(); return input.value; }"
          handle <- requireHandle =<< querySelector page (selectorFromText "input")
          typeHandle handle "world"
          assertEqual "value" "worldhello" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
    , testCase "should not modify selection when focused" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' value='hello' />"
          _ <-
            evaluateOnLocatorText
              (locate page "input")
              "input => { input.focus(); input.selectionStart = 2; input.selectionEnd = 4; return input.value; }"
          handle <- requireHandle =<< querySelector page (selectorFromText "input")
          typeHandle handle "world"
          assertEqual "value" "heworldo" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
    , testCase "should work with number input" $
        withChromiumPage $ \page -> do
          setContent page "<input type='number' value=2 />"
          handle <- requireHandle =<< querySelector page (selectorFromText "input")
          typeHandle handle "13"
          assertEqual "value" "132" =<< evaluateOnLocatorText (locate page "input") "input => input.value"
    ]
