module Playwright.ElementHandleWaitForElementStateSpec
  ( tests
  ) where

import UnliftIO.Async
  ( concurrently_
  , wait
  , withAsync
  )
import Playwright
  ( ElementState (ElementEditable, ElementEnabled, ElementHidden, ElementStable, ElementVisible)
  , defaultGotoOptions
  , evaluateOnElement
  , goto
  , milliseconds
  , querySelector
  , selectorFromText
  , setContent
  , urlFromText
  , waitForElementState
  , waitForElementStateWith
  )
import Playwright.Harness
  ( assertProtocolContains
  , assertStillPending
  , rafraf
  , requireHandle
  , withAssets
  , withChromiumPage
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
    , testCase "should wait for editable input" $
        withChromiumPage $ \page -> do
          setContent page "<input readonly>"
          handle <- requireHandle =<< querySelector page (selectorFromText "input")
          concurrently_
            (waitForElementState handle ElementEditable)
            (evaluateOnElement handle "input => { input.readOnly = false; return true; }")
    , testCase "should throw waiting for enabled when detached" $
        withChromiumPage $ \page -> do
          setContent page "<button disabled>Target</button>"
          handle <- requireHandle =<< querySelector page (selectorFromText "button")
          concurrently_
            ( assertProtocolContains
                "Element is not attached to the DOM"
                (waitForElementState handle ElementEnabled)
            )
            (evaluateOnElement handle "button => { button.remove(); return true; }")
    , testCase "should wait for aria enabled button" $
        withChromiumPage $ \page -> do
          setContent page "<button aria-disabled=true><span>Target</span></button>"
          target <- requireHandle =<< querySelector page (selectorFromText "text=Target")
          withAsync (waitForElementState target ElementEnabled) $ \pending -> do
            rafraf page 5
            assertStillPending pending
            _ <-
              evaluateOnElement
                target
                "el => { el.parentElement.setAttribute('aria-disabled', 'false'); return true; }"
            wait pending
    , testCase "should wait for button with an aria-disabled parent" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div role=\"group\" aria-disabled=true><button><span>Target</span></button></div>"
          target <- requireHandle =<< querySelector page (selectorFromText "text=Target")
          withAsync (waitForElementState target ElementEnabled) $ \pending -> do
            rafraf page 5
            assertStillPending pending
            _ <-
              evaluateOnElement
                target
                "el => { el.parentElement.parentElement.setAttribute('aria-disabled', 'false'); return true; }"
            wait pending
    , testCase "should wait for stable position" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/input/button.html")) defaultGotoOptions
          button <- requireHandle =<< querySelector page (selectorFromText "button")
          _ <-
            evaluateOnElement
              button
              "button => { button.style.transition = 'margin 10000ms linear 0s'; button.style.marginLeft = '20000px'; return true; }"
          withAsync (waitForElementState button ElementStable) $ \pending -> do
            rafraf page 5
            assertStillPending pending
            _ <- evaluateOnElement button "button => { button.style.transition = ''; return true; }"
            wait pending
    ]
