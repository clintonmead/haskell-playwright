module Playwright.PageWaitForSelector2Spec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.Exception
  ( try
  )
import Control.Monad
  ( void
  )
import UnliftIO.IORef
  ( newIORef
  , readIORef
  , writeIORef
  )
import Data.Maybe
  ( isJust
  , isNothing
  )
import Data.Text
  ( Text
  , isInfixOf
  , replace
  , toLower
  )
import Playwright
  ( Frame
  , Page
  , WaitForState (WaitForAttached, WaitForDetached, WaitForHidden, WaitForVisible)
  , asElement
  , contentFrameHandle
  , defaultGotoOptions
  , elementArgument
  , evalOnSelector
  , evaluate
  , evaluateHandle
  , evaluateOnElementText
  , evaluateOnFrame
  , evaluateTextWith
  , evaluateWith
  , frames
  , framesEqual
  , goto
  , locator
  , ownerFrameHandle
  , pageTextContent
  , querySelector
  , reload
  , selectorFromText
  , setContent
  , textArgument
  , urlFromText
  , waitFor
  , waitForSelector
  , waitForSelectorHandle
  , waitForSelectorHandleOnFrame
  , waitForSelectorHandleWith
  , waitForSelectorOnHandle
  , waitForTimeout
  , Playwright
  )
import Playwright.Error
  ( PlaywrightError (MissingObject, ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( requireTimeout
  , assertProtocolContains
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
    "page-wait-for-selector-2"
    [ testCase "should survive cross-process navigation" $
        withAssets $ \prefix page -> do
          found <- newIORef False
          withAsync
            ( do
                _ <- waitForSelectorHandle page (selectorFromText ".box") WaitForVisible
                writeIORef found True
            )
            $ \pending -> do
              _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
              assertEqual "before reload" False =<< readIORef found
              _ <- reload page
              assertEqual "after reload" False =<< readIORef found
              _ <-
                goto
                  page
                  (urlFromText (replace "127.0.0.1" "localhost" prefix <> "/grid.html"))
                  defaultGotoOptions
              wait pending
              assertEqual "found" True =<< readIORef found
    , testCase "should wait for visible" $
        withChromiumPage $ \page -> do
          found <- newIORef False
          withAsync
            ( do
                _ <- waitForSelectorHandle page (selectorFromText "div") WaitForVisible
                writeIORef found True
            )
            $ \pending -> do
              setContent page "<div style='display: none; visibility: hidden;'>1</div>"
              assertEqual "hidden" False =<< readIORef found
              _ <- evaluate page "() => { document.querySelector('div').style.removeProperty('display'); }"
              assertEqual "display" False =<< readIORef found
              _ <- evaluate page "() => { document.querySelector('div').style.removeProperty('visibility'); }"
              wait pending
              assertEqual "visible" True =<< readIORef found
    , testCase "should not consider visible when zero-sized" $
        withChromiumPage $ \page -> do
          setContent page "<div style='width: 0; height: 0;'>1</div>"
          timeout <- requireTimeout 1000
          assertProtocolContains
            "Timeout 1000ms exceeded"
            (waitForSelectorHandleWith page (selectorFromText "div") WaitForVisible timeout)
          _ <- evaluate page "() => { document.querySelector('div').style.width = '10px'; }"
          assertProtocolContains
            "Timeout 1000ms exceeded"
            (waitForSelectorHandleWith page (selectorFromText "div") WaitForVisible timeout)
          _ <- evaluate page "() => { document.querySelector('div').style.height = '10px'; }"
          handle <- waitForSelectorHandleWith page (selectorFromText "div") WaitForVisible timeout
          assertEqual "visible" True (isJust handle)
    , testCase "should wait for visible recursively" $
        withChromiumPage $ \page -> do
          found <- newIORef False
          withAsync
            ( do
                _ <- waitForSelectorHandle page (selectorFromText "div#inner") WaitForVisible
                writeIORef found True
            )
            $ \pending -> do
              setContent page "<div style='display: none; visibility: hidden;'><div id=\"inner\">hi</div></div>"
              assertEqual "hidden" False =<< readIORef found
              _ <- evaluate page "() => { document.querySelector('div').style.removeProperty('display'); }"
              assertEqual "display" False =<< readIORef found
              _ <- evaluate page "() => { document.querySelector('div').style.removeProperty('visibility'); }"
              wait pending
              assertEqual "visible" True =<< readIORef found
    , testCase "should consider outside of viewport visible" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>.cover { position: fixed; left: 0; top: 0; width: 100px; height: 100px; background-color: red; transform: translateX(-200px); }</style><div class=\"cover\">cover</div>"
          waitFor (locator page (selectorFromText ".cover")) WaitForVisible
    , testCase "hidden should wait for hidden" $
        withChromiumPage $ \page -> do
          found <- newIORef False
          setContent page "<div style='display: block;'>content</div>"
          withAsync
            ( do
                _ <- waitForSelectorHandle page (selectorFromText "div") WaitForHidden
                writeIORef found True
            )
            $ \pending -> do
              waitForSelector page (selectorFromText "div") WaitForVisible
              assertEqual "visible" False =<< readIORef found
              _ <- evaluate page "() => { document.querySelector('div').style.setProperty('visibility', 'hidden'); }"
              wait pending
              assertEqual "hidden" True =<< readIORef found
    , testCase "hidden should wait for display: none" $
        withChromiumPage $ \page -> do
          found <- newIORef False
          setContent page "<div style='display: block;'>content</div>"
          withAsync
            ( do
                _ <- waitForSelectorHandle page (selectorFromText "div") WaitForHidden
                writeIORef found True
            )
            $ \pending -> do
              waitForSelector page (selectorFromText "div") WaitForVisible
              assertEqual "visible" False =<< readIORef found
              _ <- evaluate page "() => { document.querySelector('div').style.setProperty('display', 'none'); }"
              wait pending
              assertEqual "hidden" True =<< readIORef found
    , testCase "hidden should wait for removal" $
        withChromiumPage $ \page -> do
          found <- newIORef False
          setContent page "<div>content</div>"
          withAsync
            ( do
                _ <- waitForSelectorHandle page (selectorFromText "div") WaitForHidden
                writeIORef found True
            )
            $ \pending -> do
              waitForSelector page (selectorFromText "div") WaitForVisible
              assertEqual "present" False =<< readIORef found
              _ <- evaluate page "() => { document.querySelector('div').remove(); }"
              wait pending
              assertEqual "removed" True =<< readIORef found
    , testCase "should return null if waiting to hide non-existing element" $
        withChromiumPage $ \page -> do
          handle <- waitForSelectorHandle page (selectorFromText "non-existing") WaitForHidden
          assertEqual "null" True (isNothing handle)
    , testCase "should respect timeout" $
        withChromiumPage $ \page -> do
          timeout <- requireTimeout 3000
          assertProtocolContains
            "Timeout 3000ms exceeded"
            (waitForSelectorHandleWith page (selectorFromText "div") WaitForAttached timeout)
    , testCase "should have an error message specifically for awaiting an element to be hidden" $
        withChromiumPage $ \page -> do
          setContent page "<div>content</div>"
          timeout <- requireTimeout 1000
          assertProtocolContains
            "Timeout 1000ms exceeded"
            (waitForSelectorHandleWith page (selectorFromText "div") WaitForHidden timeout)
    , testCase "should respond to node attribute mutation" $
        withChromiumPage $ \page -> do
          found <- newIORef False
          withAsync
            ( do
                _ <- waitForSelectorHandle page (selectorFromText ".zombo") WaitForAttached
                writeIORef found True
            )
            $ \pending -> do
              setContent page "<div class='notZombo'></div>"
              assertEqual "before" False =<< readIORef found
              _ <- evaluate page "() => { document.querySelector('div').className = 'zombo'; }"
              wait pending
              assertEqual "mutated" True =<< readIORef found
    , testCase "should return the element handle" $
        withChromiumPage $ \page -> do
          withAsync (requireHandle =<< waitForSelectorHandle page (selectorFromText ".zombo") WaitForVisible) $ \pending -> do
            setContent page "<div class='zombo'>anything</div>"
            handle <- wait pending
            assertEqual "text" "anything"
              =<< evaluateTextWith page "x => x.textContent" (elementArgument handle)
    , testCase "should support >> selector syntax" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync
            ( requireHandle
                =<< waitForSelectorHandle page (selectorFromText "css=div >> css=span") WaitForAttached
            )
            $ \pending -> do
              _ <- evaluate page "() => { document.body.appendChild(document.createElement('br')); }"
              _ <- evaluate page "() => { document.body.appendChild(document.createElement('div')); }"
              _ <- evaluate page "() => { document.querySelector('div').appendChild(document.createElement('span')); }"
              handle <- wait pending
              tag <- evaluateOnElementText handle "e => e.tagName"
              assertEqual "tag" "SPAN" tag
    , testCase "should wait for detached if already detached" $
        withChromiumPage $ \page -> do
          setContent page "<section id=\"testAttribute\">43543</section>"
          handle <- waitForSelectorHandle page (selectorFromText "css=div") WaitForDetached
          assertEqual "null" True (isNothing handle)
    , testCase "should wait for detached" $
        withChromiumPage $ \page -> do
          found <- newIORef False
          setContent page "<section id=\"testAttribute\"><div>43543</div></section>"
          withAsync
            ( do
                _ <- waitForSelectorHandle page (selectorFromText "css=div") WaitForDetached
                writeIORef found True
            )
            $ \pending -> do
              assertEqual "before" False =<< readIORef found
              waitForSelector page (selectorFromText "css=section") WaitForVisible
              assertEqual "section" False =<< readIORef found
              _ <- evalOnSelector page (selectorFromText "div") "div => { div.remove(); }"
              wait pending
              assertEqual "detached" True =<< readIORef found
    , testCase "should support some fancy xpath" $
        withChromiumPage $ \page -> do
          setContent page "<p>red herring</p><p>hello  world  </p>"
          handle <-
            requireHandle
              =<< waitForSelectorHandle page (selectorFromText "//p[normalize-space(.)=\"hello world\"]") WaitForVisible
          assertEqual "text" "hello  world  "
            =<< evaluateTextWith page "x => x.textContent" (elementArgument handle)
    , testCase "should respect timeout xpath" $
        withChromiumPage $ \page -> do
          timeout <- requireTimeout 3000
          assertProtocolContains
            "Timeout 3000ms exceeded"
            (waitForSelectorHandleWith page (selectorFromText "//div") WaitForAttached timeout)
    , testCase "should run in specified frame xpath" $
        withAssets $ \prefix page -> do
          _ <- attachFrame page prefix "frame1"
          _ <- attachFrame page prefix "frame2"
          frame1 <- frameAt page 1
          frame2 <- frameAt page 2
          withAsync
            ( requireHandle
                =<< waitForSelectorHandleOnFrame frame2 (selectorFromText "//div") WaitForAttached
            )
            $ \pending -> do
              _ <- evaluateOnFrame frame1 "() => { document.body.appendChild(document.createElement('div')); }"
              _ <- evaluateOnFrame frame2 "() => { document.body.appendChild(document.createElement('div')); }"
              handle <- wait pending
              owner <- ownerFrameHandle handle
              case owner of
                Nothing ->
                  assertFailure "expected owner frame"
                Just frame ->
                  assertEqual "frame2" True (framesEqual frame frame2)
    , testCase "should throw when frame is detached xpath" $
        withAssets $ \prefix page -> do
          _ <- attachFrame page prefix "frame1"
          frame <- frameAt page 1
          withAsync (waitForSelectorHandleOnFrame frame (selectorFromText "//*[@class=\"box\"]") WaitForVisible) $ \pending -> do
            detachFrame page "frame1"
            assertDetached =<< try (wait pending)
    , testCase "should return the element handle xpath" $
        withChromiumPage $ \page -> do
          withAsync
            ( requireHandle
                =<< waitForSelectorHandle page (selectorFromText "//*[@class=\"zombo\"]") WaitForVisible
            )
            $ \pending -> do
              setContent page "<div class='zombo'>anything</div>"
              handle <- wait pending
              assertEqual "text" "anything"
                =<< evaluateTextWith page "x => x.textContent" (elementArgument handle)
    , testCase "should allow you to select an element with single slash xpath" $
        withChromiumPage $ \page -> do
          setContent page "<div>some text</div>"
          handle <-
            requireHandle
              =<< waitForSelectorHandle page (selectorFromText "//html/body/div") WaitForVisible
          assertEqual "text" "some text"
            =<< evaluateTextWith page "x => x.textContent" (elementArgument handle)
    , testCase "should correctly handle hidden shadow host" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<x-host hidden></x-host><script>const host = document.querySelector('x-host'); const root = host.attachShadow({ mode: 'open' }); const style = document.createElement('style'); style.textContent = ':host([hidden]) { display: none; }'; root.appendChild(style); const child = document.createElement('div'); child.textContent = 'Find me'; root.appendChild(child);</script>"
          assertEqual "text" (Just "Find me") =<< pageTextContent page (selectorFromText "div")
          waitForSelector page (selectorFromText "div") WaitForHidden
    , testCase "should fail if element handle was detached while waiting" $
        withChromiumPage $ \page -> do
          setContent page "<button>hello</button>"
          button <- requireHandle =<< querySelector page (selectorFromText "button")
          withAsync (waitForSelectorOnHandle button (selectorFromText "something") WaitForVisible) $ \pending -> do
            waitForTimeout page =<< requireTimeout 100
            _ <- evaluate page "() => { document.body.innerText = ''; }"
            assertProtocolContains "Element is not attached to the DOM" (wait pending)
    , testCase "should succeed if element handle was detached while waiting for hidden" $
        withChromiumPage $ \page -> do
          setContent page "<button>hello</button>"
          button <- requireHandle =<< querySelector page (selectorFromText "button")
          withAsync (waitForSelectorOnHandle button (selectorFromText "something") WaitForHidden) $ \pending -> do
            waitForTimeout page =<< requireTimeout 100
            _ <- evaluate page "() => { document.body.innerText = ''; }"
            void (wait pending)
    , testCase "should succeed if element handle was detached while waiting for detached" $
        withChromiumPage $ \page -> do
          setContent page "<button>hello</button>"
          button <- requireHandle =<< querySelector page (selectorFromText "button")
          withAsync (waitForSelectorOnHandle button (selectorFromText "something") WaitForDetached) $ \pending -> do
            waitForTimeout page =<< requireTimeout 100
            _ <- evaluate page "() => { document.body.innerText = ''; }"
            void (wait pending)
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
