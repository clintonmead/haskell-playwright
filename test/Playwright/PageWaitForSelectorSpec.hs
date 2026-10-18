module Playwright.PageWaitForSelectorSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.Exception
  ( try
  )
import Data.Text
  ( Text
  , isInfixOf
  , toLower
  )
import Playwright
  ( Frame
  , Page
  , WaitForState (WaitForAttached, WaitForVisible)
  , asElement
  , contentFrameHandle
  , defaultGotoOptions
  , elementArgument
  , evaluate
  , evaluateHandle
  , evaluateOnElementText
  , evaluateOnFrame
  , evaluateTextWith
  , evaluateWith
  , frames
  , framesEqual
  , goto
  , mainFrame
  , ownerFrameHandle
  , querySelector
  , selectorFromText
  , setContent
  , textArgument
  , urlFromText
  , waitForSelector
  , waitForSelectorHandle
  , waitForSelectorHandleOnFrame
  , waitForSelectorOnHandle
  , waitForSelectorOnHandleWith
  , waitForTimeout
  , waitForWith
  , Playwright
  )
import Playwright.Error
  ( PlaywrightError (MissingObject, ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( requireTimeout
  , assertProtocolContains
  , locate
  , requireHandle
  , withAssets
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
    "page-wait-for-selector-1"
    [ testCase "should immediately resolve promise if node exists" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          waitForSelector page (selectorFromText "*") WaitForAttached
          _ <- evaluate page "() => { document.body.appendChild(document.createElement('div')); }"
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
          withAsync (requireHandle =<< waitForSelectorOnHandle divHandle (selectorFromText "span") WaitForAttached) $ \pending -> do
            _ <- evaluateOnElementText divHandle "div => { div.innerHTML = '<span>target</span>'; return 'ok'; }"
            spanHandle <- wait pending
            assertEqual "text" "target" =<< evaluateOnElementText spanHandle "e => e.textContent"
    , testCase "elementHandle.waitForSelector should timeout" $
        withChromiumPage $ \page -> do
          setContent page "<div></div>"
          divHandle <- requireHandle =<< querySelector page (selectorFromText "div")
          timeout <- requireTimeout 100
          assertProtocolContains
            "Timeout 100ms exceeded"
            (waitForSelectorOnHandleWith divHandle (selectorFromText "span") WaitForAttached timeout)
    , testCase "should timeout" $
        withChromiumPage $ \page -> do
          setContent page "<div></div>"
          timeout <- requireTimeout 100
          assertProtocolContains
            "Timeout 100ms exceeded"
            (waitForWith (locate page "span") timeout WaitForAttached)
    , testCase "should work with removed MutationObserver" $
        withChromiumPage $ \page -> do
          _ <- evaluate page "() => { delete window.MutationObserver; }"
          withAsync (requireHandle =<< waitForSelectorHandle page (selectorFromText ".zombo") WaitForVisible) $ \pending -> do
            setContent page "<div class='zombo'>anything</div>"
            handle <- wait pending
            assertEqual "text" "anything"
              =<< evaluateTextWith page "x => x.textContent" (elementArgument handle)
    , testCase "should resolve promise when node is added" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (requireHandle =<< waitForSelectorHandle page (selectorFromText "div") WaitForAttached) $ \pending -> do
            _ <- evaluate page "() => { document.body.appendChild(document.createElement('br')); }"
            _ <- evaluate page "() => { document.body.appendChild(document.createElement('div')); }"
            handle <- wait pending
            tag <- evaluateOnElementText handle "e => e.tagName"
            assertEqual "tag" "DIV" tag
    , testCase "should resolve promise when node is added in shadow dom" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (requireHandle =<< waitForSelectorHandle page (selectorFromText "span") WaitForVisible) $ \pending -> do
            _ <-
              evaluate
                page
                "() => { const div = document.createElement('div'); div.attachShadow({ mode: 'open' }); document.body.appendChild(div); }"
            waitForTimeout page =<< requireTimeout 100
            _ <-
              evaluate
                page
                "() => { const span = document.createElement('span'); span.textContent = 'Hello from shadow'; document.querySelector('div').shadowRoot.appendChild(span); }"
            handle <- wait pending
            assertEqual "text" "Hello from shadow" =<< evaluateOnElementText handle "e => e.textContent"
    , testCase "should work when node is added through innerHTML" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForSelector page (selectorFromText "h3 div") WaitForAttached) $ \pending -> do
            _ <- evaluate page "() => { document.body.appendChild(document.createElement('span')); }"
            _ <- evaluate page "() => { document.querySelector('span').innerHTML = '<h3><div></div></h3>'; }"
            wait pending
    , testCase "page.waitForSelector is shortcut for main frame" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- attachFrame page prefix "frame1"
          other <- frameAt page 1
          withAsync (requireHandle =<< waitForSelectorHandle page (selectorFromText "div") WaitForAttached) $ \pending -> do
            _ <- evaluateOnFrame other "() => { document.body.appendChild(document.createElement('div')); }"
            _ <- evaluate page "() => { document.body.appendChild(document.createElement('div')); }"
            handle <- wait pending
            owner <- ownerFrameHandle handle
            case owner of
              Nothing ->
                assertFailure "expected owner frame"
              Just frame ->
                assertEqual "main" True (framesEqual frame (mainFrame page))
    , testCase "should run in specified frame" $
        withAssets $ \prefix page -> do
          _ <- attachFrame page prefix "frame1"
          _ <- attachFrame page prefix "frame2"
          frame1 <- frameAt page 1
          frame2 <- frameAt page 2
          withAsync (requireHandle =<< waitForSelectorHandleOnFrame frame2 (selectorFromText "div") WaitForAttached) $ \pending -> do
            _ <- evaluateOnFrame frame1 "() => { document.body.appendChild(document.createElement('div')); }"
            _ <- evaluateOnFrame frame2 "() => { document.body.appendChild(document.createElement('div')); }"
            handle <- wait pending
            owner <- ownerFrameHandle handle
            case owner of
              Nothing ->
                assertFailure "expected owner frame"
              Just frame ->
                assertEqual "frame2" True (framesEqual frame frame2)
    , testCase "should throw when frame is detached" $
        withAssets $ \prefix page -> do
          _ <- attachFrame page prefix "frame1"
          frame <- frameAt page 1
          withAsync (waitForSelectorHandleOnFrame frame (selectorFromText ".box") WaitForVisible) $ \pending -> do
            detachFrame page "frame1"
            assertDetached =<< try (wait pending)
    ]

attachFrame :: Page -> Text -> Text -> Playwright Frame
attachFrame page prefix frameId = do
  handle <-
    evaluateHandle
      page
      ( "async () => { const frame = document.createElement('iframe'); frame.src = '"
          <> prefix
          <> "/empty.html'; frame.id = '"
          <> frameId
          <> "'; document.body.appendChild(frame); await new Promise(x => frame.onload = x); return frame; }"
      )
  element <- requireHandle =<< asElement handle
  requireFrame =<< contentFrameHandle element

detachFrame :: Page -> Text -> Playwright ()
detachFrame page frameId = do
  _ <-
    evaluateWith
      page
      "id => { const frame = document.getElementById(id); if (frame) frame.remove(); }"
      (textArgument frameId)
  pure ()

frameAt :: Page -> Int -> Playwright Frame
frameAt page index = do
  listed <- frames page
  case drop index listed of
    frame : _ ->
      pure frame
    [] ->
      assertFailure ("expected frame at index " <> show index)

requireFrame :: Maybe Frame -> Playwright Frame
requireFrame frame =
  case frame of
    Just value ->
      pure value
    Nothing ->
      assertFailure "expected a frame"

assertDetached :: Either PlaywrightError a -> Playwright ()
assertDetached result =
  case result of
    Left (ProtocolRpc failure)
      | "detach" `isInfixOf` toLower (protocolFailureMessage failure) ->
          pure ()
    Left (ProtocolRpc failure)
      | "target closed" `isInfixOf` toLower (protocolFailureMessage failure) ->
          pure ()
    Left (MissingObject _) ->
      pure ()
    Left err ->
      assertFailure ("expected detached-frame error, got: " <> show err)
    Right _ ->
      assertFailure "expected detached-frame error"
