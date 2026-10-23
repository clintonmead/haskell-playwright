module Playwright.PageWaitForSelector1Spec
  ( tests
  ) where

import Control.Monad
  ( replicateM_
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )
import Playwright
  ( WaitForState (WaitForAttached, WaitForHidden, WaitForVisible)
  , defaultGotoOptions
  , evaluate
  , evaluateOnElementText
  , evaluateOnFrame
  , framesEqual
  , goto
  , mainFrame
  , milliseconds
  , ownerFrameHandle
  , pageClickSelector
  , querySelector
  , selectorFromText
  , setContent
  , setDefaultTimeout
  , urlFromText
  , waitForSelector
  , waitForSelectorHandle
  , waitForSelectorHandleOnFrame
  , waitForSelectorHandleWith
  , waitForSelectorOnHandle
  , waitForSelectorOnHandleWith
  , waitForTimeout
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , assertProtocolContains
  , assertProtocolContainsAll
  , attachFrame
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
    , testCase "elementHandle.waitForSelector should throw on navigation" $
        withAssets $ \prefix page -> do
          setContent page "<div></div>"
          divHandle <- requireHandle =<< querySelector page (selectorFromText "div")
          assertProtocolContains
            "waiting for locator('span') to be visible"
            ( withAsync (waitForSelectorOnHandle divHandle (selectorFromText "span") WaitForVisible) $ \pending -> do
                replicateM_ 10 (evaluate page "() => 1")
                _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
                wait pending
            )
    , testCase "should report logs when the selector resolves to multiple elements" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent
            page
            "<button style=\"display: none; position: absolute; top: 0px; left: 0px; width: 100%;\">Reset</button><button>Reset</button>"
          case milliseconds 1000 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              setDefaultTimeout page timeout
              assertProtocolContains
                "locator resolved to 2 elements. Proceeding with the first one: <button>Reset</button>"
                (pageClickSelector page (selectorFromText "text=Reset"))
    , testCase "should report logs while waiting for visible" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case milliseconds 5000 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertProtocolContainsAll
                [ "Timeout 5000ms exceeded."
                , "waiting for locator('div') to be visible"
                , "locator resolved to hidden <div id=\"mydiv\" class=\"foo bar\" foo=\"123456789012345678901234567890123456789012345678901234567890\">abcdefghijklmnopqrstuvwyxzabcdefghijklmnopqrstuvw\8230</div>"
                , "locator resolved to hidden <div class=\"another\"></div>"
                ]
                ( withAsync (waitForSelectorHandleWith page (selectorFromText "div") WaitForVisible timeout) $ \pending -> do
                    _ <-
                      evaluate
                        page
                        "() => { const div = document.createElement('div'); div.className = 'foo bar'; div.id = 'mydiv'; div.setAttribute('style', 'display: none'); div.setAttribute('foo', '123456789012345678901234567890123456789012345678901234567890'); div.textContent = 'abcdefghijklmnopqrstuvwyxzabcdefghijklmnopqrstuvwyxzabcdefghijklmnopqrstuvwyxz'; document.body.appendChild(div); }"
                    waitForTimeout page =<< requireTimeout 100
                    _ <- evaluate page "() => document.querySelector('div').remove()"
                    waitForTimeout page =<< requireTimeout 100
                    _ <-
                      evaluate
                        page
                        "() => { const div = document.createElement('div'); div.className = 'another'; div.style.display = 'none'; document.body.appendChild(div); }"
                    waitForTimeout page =<< requireTimeout 100
                    wait pending
                )
    , testCase "should report logs while waiting for hidden" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { const div = document.createElement('div'); div.className = 'foo bar'; div.id = 'mydiv'; div.textContent = 'hello'; document.body.appendChild(div); }"
          case milliseconds 5000 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertProtocolContainsAll
                [ "Timeout 5000ms exceeded."
                , "waiting for locator('div') to be hidden"
                , "locator resolved to visible <div id=\"mydiv\" class=\"foo bar\">hello</div>"
                , "locator resolved to visible <div class=\"another\">hello</div>"
                ]
                ( withAsync (waitForSelectorHandleWith page (selectorFromText "div") WaitForHidden timeout) $ \pending -> do
                    waitForTimeout page =<< requireTimeout 100
                    _ <-
                      evaluate
                        page
                        "() => { document.querySelector('div').remove(); const div = document.createElement('div'); div.className = 'another'; div.textContent = 'hello'; document.body.appendChild(div); }"
                    waitForTimeout page =<< requireTimeout 100
                    wait pending
                )
    , testCase "page.waitForSelector is shortcut for main frame" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          frame <- attachFrame page "frame1" (prefix <> "/empty.html")
          withAsync (waitForSelectorHandle page (selectorFromText "div") WaitForAttached) $ \pending -> do
            _ <- evaluateOnFrame frame "() => document.body.appendChild(document.createElement('div'))"
            _ <- evaluate page "() => document.body.appendChild(document.createElement('div'))"
            handle <- requireHandle =<< wait pending
            owner <- ownerFrameHandle handle
            assertBool "owner frame is the main frame" (maybe False (framesEqual (mainFrame page)) owner)
    , testCase "should run in specified frame" $
        withAssets $ \prefix page -> do
          frame1 <- attachFrame page "frame1" (prefix <> "/empty.html")
          frame2 <- attachFrame page "frame2" (prefix <> "/empty.html")
          withAsync (waitForSelectorHandleOnFrame frame2 (selectorFromText "div") WaitForAttached) $ \pending -> do
            _ <- evaluateOnFrame frame1 "() => document.body.appendChild(document.createElement('div'))"
            _ <- evaluateOnFrame frame2 "() => document.body.appendChild(document.createElement('div'))"
            handle <- requireHandle =<< wait pending
            owner <- ownerFrameHandle handle
            assertBool "owner frame is frame2" (maybe False (framesEqual frame2) owner)
    , testCase "should throw when frame is detached" $
        withAssets $ \prefix page -> do
          frame <- attachFrame page "frame1" (prefix <> "/empty.html")
          assertProtocolContains
            "Frame was detached"
            ( withAsync (waitForSelectorHandleOnFrame frame (selectorFromText ".box") WaitForVisible) $ \pending -> do
                waitForTimeout page =<< requireTimeout 100
                _ <- evaluate page "() => document.getElementById('frame1').remove()"
                wait pending
            )
    ]
