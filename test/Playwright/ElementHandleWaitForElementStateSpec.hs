module Playwright.ElementHandleWaitForElementStateSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( concurrently_
  )
import Playwright
  ( ElementState (ElementHidden, ElementVisible)
  , evaluateOnElement
  , milliseconds
  , querySelector
  , selectorFromText
  , setContent
  , waitForElementState
  , waitForElementStateWith
  )
import Playwright.Harness
  ( assertProtocolContains
  , requireHandle
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "elementhandle-wait-for-element-state"
    [ testCase "should wait for already visible" $
        withChromiumPage $ \page -> do
          setContent page "<div>content</div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          waitForElementState handle ElementVisible
    , testCase "should timeout waiting for visible" $
        withChromiumPage $ \page -> do
          setContent page "<div style='display:none'>content</div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          case milliseconds 1000 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertProtocolContains
                "Timeout 1000ms exceeded"
                (waitForElementStateWith handle ElementVisible timeout)
    , testCase "should wait for visible" $
        withChromiumPage $ \page -> do
          setContent page "<div style='display:none'>content</div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          concurrently_
            (waitForElementState handle ElementVisible)
            (evaluateOnElement handle "div => { div.style.display = 'block'; return true; }")
    , testCase "should throw waiting for visible when detached" $
        withChromiumPage $ \page -> do
          setContent page "<div style='display:none'>content</div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          concurrently_
            ( assertProtocolContains
                "Element is not attached to the DOM"
                (waitForElementState handle ElementVisible)
            )
            (evaluateOnElement handle "div => { div.remove(); return true; }")
    , testCase "should wait for already hidden" $
        withChromiumPage $ \page -> do
          setContent page "<div></div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          waitForElementState handle ElementHidden
    , testCase "should wait for hidden when detached" $
        withChromiumPage $ \page -> do
          setContent page "<div>content</div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          concurrently_
            (waitForElementState handle ElementHidden)
            (evaluateOnElement handle "div => { div.remove(); return true; }")
    ]
