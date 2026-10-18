module Playwright.PageWaitForSelector1Spec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Playwright
  ( WaitForState (WaitForAttached, WaitForVisible)
  , defaultGotoOptions
  , evaluate
  , evaluateOnElementText
  , goto
  , milliseconds
  , querySelector
  , selectorFromText
  , setContent
  , urlFromText
  , waitForSelector
  , waitForSelectorHandle
  , waitForSelectorOnHandle
  , waitForSelectorOnHandleWith
  , waitForTimeout
  )
import Playwright.Harness
  ( assertEqual
  , assertFailure
  , assertProtocolContains
  , requireHandle
  , requireTimeout
  , withAssets
  , withChromiumPage
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
    "page-wait-for-selector-1"
    [ testCase "should immediately resolve promise if node exists" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          waitForSelector page (selectorFromText "*") WaitForVisible
          _ <- evaluate page "() => document.body.appendChild(document.createElement('div'))"
          waitForSelector page (selectorFromText "div") WaitForAttached
    , testCase "elementHandle.waitForSelector should immediately resolve if node exists" $
        withChromiumPage $ \page -> do
          setContent page "<span>extra</span><div><span>target</span></div>"
          divHandle <- requireHandle =<< querySelector page (selectorFromText "div")
          spanHandle <- requireHandle =<< waitForSelectorOnHandle divHandle (selectorFromText "span") WaitForAttached
          assertEqual "text" "target" =<< evaluateOnElementText spanHandle "e => e.textContent"
    , testCase "elementHandle.waitForSelector should wait" $
        withChromiumPage $ \page -> do
          setContent page "<div></div>"
          divHandle <- requireHandle =<< querySelector page (selectorFromText "div")
          withAsync (waitForSelectorOnHandle divHandle (selectorFromText "span") WaitForAttached) $ \pending -> do
            _ <- evaluateOnElementText divHandle "div => div.innerHTML = '<span>target</span>'"
            spanHandle <- requireHandle =<< wait pending
            assertEqual "text" "target" =<< evaluateOnElementText spanHandle "e => e.textContent"
    , testCase "elementHandle.waitForSelector should timeout" $
        withChromiumPage $ \page -> do
          setContent page "<div></div>"
          divHandle <- requireHandle =<< querySelector page (selectorFromText "div")
          case milliseconds 100 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertProtocolContains
                "Timeout 100ms exceeded"
                (waitForSelectorOnHandleWith divHandle (selectorFromText "span") WaitForAttached timeout)
    , testCase "should work with removed MutationObserver" $
        withChromiumPage $ \page -> do
          _ <- evaluate page "() => delete window.MutationObserver"
          withAsync (waitForSelectorHandle page (selectorFromText ".zombo") WaitForVisible) $ \pending -> do
            setContent page "<div class='zombo'>anything</div>"
            handle <- requireHandle =<< wait pending
            assertEqual "text" "anything" =<< evaluateOnElementText handle "e => e.textContent"
    , testCase "should resolve promise when node is added" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForSelectorHandle page (selectorFromText "div") WaitForAttached) $ \pending -> do
            _ <- evaluate page "() => document.body.appendChild(document.createElement('br'))"
            _ <- evaluate page "() => document.body.appendChild(document.createElement('div'))"
            handle <- requireHandle =<< wait pending
            assertEqual "tagName" "DIV" =<< evaluateOnElementText handle "e => e.tagName"
    , testCase "should work when node is added through innerHTML" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForSelector page (selectorFromText "h3 div") WaitForAttached) $ \pending -> do
            _ <- evaluate page "() => document.body.appendChild(document.createElement('span'))"
            _ <- evaluate page "() => document.querySelector('span').innerHTML = '<h3><div></div></h3>'"
            wait pending
    , testCase "should resolve promise when node is added in shadow dom" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForSelectorHandle page (selectorFromText "span") WaitForVisible) $ \pending -> do
            _ <-
              evaluate
                page
                "() => { const div = document.createElement('div'); div.attachShadow({ mode: 'open' }); document.body.appendChild(div); }"
            waitForTimeout page =<< requireTimeout 100
            _ <-
              evaluate
                page
                "() => { const span = document.createElement('span'); span.textContent = 'Hello from shadow'; document.querySelector('div').shadowRoot.appendChild(span); }"
            handle <- requireHandle =<< wait pending
            assertEqual "text" "Hello from shadow" =<< evaluateOnElementText handle "e => e.textContent"
    ]
