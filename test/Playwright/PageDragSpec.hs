module Playwright.PageDragSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( DragAndDropOptions (dragAndDropOptionsSourcePosition, dragAndDropOptionsSteps, dragAndDropOptionsTargetPosition)
  , ElementHandle
  , Frame
  , Key (Key)
  , JsHandle
  , Locator
  , Page
  , Point (Point)
  , Playwright
  , UrlGlob (UrlGlob)
  , abortRoute
  , cssPixelFromInt
  , defaultDragAndDropOptions
  , defaultGotoOptions
  , dragAndDrop
  , dragAndDropWith
  , elementHandle
  , evaluate
  , evaluateBool
  , evaluateHandleOnElement
  , evaluateOnLocatorBool
  , evaluateOnLocatorText
  , evaluateText
  , frames
  , goto
  , hover
  , jsonValueTexts
  , keyboardPress
  , locatorOnFrame
  , mouseDown
  , mouseMove
  , mouseUp
  , route
  , selectorFromText
  , setContent
  , urlFromText
  , waitForFunction
  )
import Playwright.Harness
  ( assertFailure
  , attachFrame
  , locate
  , requireHandle
  , withAssetPage
  , withAssets
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
import UnliftIO.Async
  ( withAsync
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )

tests :: TestTree
tests =
  testGroup
    "page-drag"
    [ testCase "should work" $
        withAssetPage "drag-n-drop.html" $ \page -> do
          hover (locate page "#source")
          mouseDown page
          hover (locate page "#target")
          mouseUp page
          assertDropped (locate page "#target")
    , testCase "should send the right events" $
        withAssetPage "drag-n-drop.html" $ \page -> do
          events <- trackEvents =<< bodyHandle (locate page "body")
          hover (locate page "#source")
          mouseDown page
          hover (locate page "#target")
          mouseUp page
          assertEqual "events"
            [ "mousemove at 120;86"
            , "mousedown at 120;86"
            , "mousemove at 240;350"
            , "dragstart at 120;86"
            , "dragenter at 240;350"
            , "dragover at 240;350"
            , "drop at 240;350"
            , "dragend"
            ]
            =<< jsonValueTexts events
    , testCase "should not send dragover on the first mousemove" $
        withAssetPage "drag-n-drop.html" $ \page -> do
          events <- trackEvents =<< bodyHandle (locate page "body")
          hover (locate page "#source")
          mouseDown page
          hover (locate page "#target")
          assertEqual "events"
            [ "mousemove at 120;86"
            , "mousedown at 120;86"
            , "mousemove at 240;350"
            , "dragstart at 120;86"
            , "dragenter at 240;350"
            ]
            =<< jsonValueTexts events
    , testCase "should work inside iframe" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          frame <- attachFrame page "myframe" (prefix <> "/drag-n-drop.html")
          styleFrame page
          pageEvents <- trackEvents =<< bodyHandle (locate page "body")
          frameEvents <- trackEvents =<< bodyHandle (onFrame page frame "body")
          hover (onFrame page frame "#source")
          mouseDown page
          hover (onFrame page frame "#target")
          mouseUp page
          assertEqual "dropped" True
            =<< evaluateOnLocatorBool (onFrame page frame "#target") "target => target.contains(document.querySelector('#source'))"
          assertEqual "frame events"
            [ "mousemove at 120;86"
            , "mousedown at 120;86"
            , "mousemove at 240;350"
            , "dragstart at 120;86"
            , "dragenter at 240;350"
            , "dragover at 240;350"
            , "drop at 240;350"
            , "dragend"
            ]
            =<< jsonValueTexts frameEvents
          assertEqual "page events" [] =<< jsonValueTexts pageEvents
    , testCase "should cancel on escape" $
        withAssetPage "drag-n-drop.html" $ \page -> do
          events <- trackEvents =<< bodyHandle (locate page "body")
          hover (locate page "#source")
          mouseDown page
          hover (locate page "#target")
          keyboardPress page (Key "Escape")
          mouseUp page
          assertEqual "not dropped" False
            =<< evaluateOnLocatorBool (locate page "#target") "target => target.contains(document.querySelector('#source'))"
          assertEqual "events"
            [ "mousemove at 120;86"
            , "mousedown at 120;86"
            , "mousemove at 240;350"
            , "dragstart at 120;86"
            , "dragenter at 240;350"
            , "dragend"
            , "mouseup at 240;350"
            ]
            =<< jsonValueTexts events
    , testCase "should respect the drop effect" $
        withChromiumPage $ \page -> do
          assertEqual "copy/copy" True =<< testIfDropped page "copy" "copy"
          assertEqual "copy/move" False =<< testIfDropped page "copy" "move"
          assertEqual "all/link" True =<< testIfDropped page "all" "link"
          assertEqual "all/none" False =<< testIfDropped page "all" "none"
          assertEqual "copyMove/copy" True =<< testIfDropped page "copyMove" "copy"
          assertEqual "copyLink/copy" True =<< testIfDropped page "copyLink" "copy"
          assertEqual "linkMove/copy" False =<< testIfDropped page "linkMove" "copy"
          assertEqual "copyMove/link" False =<< testIfDropped page "copyMove" "link"
          assertEqual "copyLink/link" True =<< testIfDropped page "copyLink" "link"
          assertEqual "linkMove/link" True =<< testIfDropped page "linkMove" "link"
          assertEqual "copyMove/move" True =<< testIfDropped page "copyMove" "move"
          assertEqual "copyLink/move" False =<< testIfDropped page "copyLink" "move"
          assertEqual "linkMove/move" True =<< testIfDropped page "linkMove" "move"
          assertEqual "uninitialized/copy" True =<< testIfDropped page "uninitialized" "copy"
    , testCase "should work if the drag is canceled" $
        withAssetPage "drag-n-drop.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { document.body.addEventListener('dragstart', event => { event.preventDefault(); }, false); }"
          hover (locate page "#source")
          mouseDown page
          hover (locate page "#target")
          mouseUp page
          assertEqual "not dropped" False
            =<< evaluateOnLocatorBool (locate page "#target") "target => target.contains(document.querySelector('#source'))"
    , testCase "should work if the drag event is captured but not canceled" $
        withAssetPage "drag-n-drop.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { document.body.addEventListener('dragstart', event => { event.stopImmediatePropagation(); }, false); }"
          hover (locate page "#source")
          mouseDown page
          hover (locate page "#target")
          mouseUp page
          assertDropped (locate page "#target")
    , testCase "should be able to drag the mouse in a frame" $
        withAssetPage "frames/one-frame.html" $ \page -> do
          frame <- firstChildFrame page
          events <- trackEvents =<< bodyHandle (onFrame page frame "html")
          mouseMove page (point 30 30)
          mouseDown page
          mouseMove page (point 60 60)
          mouseUp page
          assertEqual "events"
            [ "mousemove at 20;20"
            , "mousedown at 20;20"
            , "mousemove at 50;50"
            , "mouseup at 50;50"
            ]
            =<< jsonValueTexts events
    , testCase "should work if a frame is stalled" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/drag-n-drop.html")) defaultGotoOptions
          captured <- newEmptyMVar
          route page (UrlGlob "**/empty.html") (putMVar captured)
          withAsync (attachFrame page "frame" (prefix <> "/empty.html")) $ \_ -> do
            pending <- takeMVar captured
            hover (locate page "#source")
            mouseDown page
            hover (locate page "#target")
            mouseUp page
            abortRoute pending
            assertDropped (locate page "#target")
    , testCase "should work with the helper method" $
        withAssetPage "drag-n-drop.html" $ \page -> do
          dragAndDrop (locate page "#source") (locate page "#target")
          assertDropped (locate page "#target")
    , testCase "should work with tweened mouse movement" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<body style=\"margin: 0; padding: 0;\">\n          <div style=\"width:100px;height:100px;background:red;\" id=\"red\"></div>\n          <div style=\"width:300px;height:100px;background:blue;\" id=\"blue\"></div>\n        </body>"
          events <- trackPageMouse page
          dragAndDropWith
            (locate page "#red")
            (locate page "#blue")
            defaultDragAndDropOptions {dragAndDropOptionsSteps = Just 4}
          assertEqual "events"
            [ "mousemove at 50;50"
            , "mousedown at 50;50"
            , "mousemove at 75;75"
            , "mousemove at 100;100"
            , "mousemove at 125;125"
            , "mousemove at 150;150"
            , "mouseup at 150;150"
            ]
            =<< jsonValueTexts events
    , testCase "should allow specifying the position" $
        withChromiumPage $ \page -> do
          setContent
            page
            "\n      <div style=\"width:100px;height:100px;background:red;\" id=\"red\">\n      </div>\n      <div style=\"width:100px;height:100px;background:blue;\" id=\"blue\">\n      </div>\n    "
          body <- bodyHandle (locate page "body")
          events <-
            evaluateHandleOnElement
              body
              "body => { const events = []; document.getElementById('red').addEventListener('mousedown', event => events.push(`mousedown at ${event.offsetX};${event.offsetY}`)); document.getElementById('blue').addEventListener('mouseup', event => events.push(`mouseup at ${event.offsetX};${event.offsetY}`)); return events; }"
          dragAndDropWith
            (locate page "#red")
            (locate page "#blue")
            defaultDragAndDropOptions
              { dragAndDropOptionsSourcePosition = Just (point 34 7)
              , dragAndDropOptionsTargetPosition = Just (point 10 20)
              }
          assertEqual "events" ["mousedown at 34;7", "mouseup at 10;20"] =<< jsonValueTexts events
    , testCase "should work with locators" $
        withAssetPage "drag-n-drop.html" $ \page -> do
          dragAndDrop (locate page "#source") (locate page "#target")
          assertDropped (locate page "#target")
    , testCase "should work if not doing a drag" $
        withChromiumPage $ \page -> do
          events <- trackEvents =<< bodyHandle (locate page "html")
          mouseMove page (point 50 50)
          mouseDown page
          mouseMove page (point 100 100)
          mouseUp page
          assertEqual "events"
            [ "mousemove at 50;50"
            , "mousedown at 50;50"
            , "mousemove at 100;100"
            , "mouseup at 100;100"
            ]
            =<< jsonValueTexts events
    , testCase "should report event.buttons" $
        withChromiumPage $ \page -> do
          body <- bodyHandle (locate page "body")
          logs <-
            evaluateHandleOnElement
              body
              "async body => { const div = document.createElement('div'); body.appendChild(div); div.style.width = '200px'; div.style.height = '200px'; div.style.backgroundColor = 'blue'; const logs = []; const onEvent = event => logs.push(`${event.type} ${event.buttons}`); div.addEventListener('mousedown', onEvent); div.addEventListener('mousemove', onEvent, { passive: false }); div.addEventListener('mouseup', onEvent); await new Promise(resolve => requestAnimationFrame(resolve)); return logs; }"
          mouseMove page (point 20 20)
          mouseDown page
          mouseMove page (point 40 40)
          mouseUp page
          assertEqual "logs" ["mousemove 0", "mousedown 1", "mousemove 1", "mouseup 0"] =<< jsonValueTexts logs
    , testCase "should handle custom dataTransfer" $
        withChromiumPage $ \page -> do
          setContent page "<button draggable=\"true\">Draggable</button>"
          _ <-
            evaluate
              page
              "() => { document.addEventListener('dragstart', event => { event.dataTransfer.setData('custom-type', 'Hello World'); }, false); document.addEventListener('dragenter', event => { event.preventDefault(); }, false); document.addEventListener('dragover', event => { event.preventDefault(); }, false); document.addEventListener('drop', event => { event.preventDefault(); window.result = { types: Array.from(event.dataTransfer.types), data: event.dataTransfer.getData('custom-type') }; }, false); }"
          hover (locate page "[draggable=\"true\"]")
          mouseDown page
          mouseMove page (point 100 100)
          mouseUp page
          _ <- waitForFunction page "() => window.result !== undefined"
          assertEqual "result"
            "{\"types\":[\"custom-type\"],\"data\":\"Hello World\"}"
            =<< evaluateText page "() => JSON.stringify(window.result)"
    , testCase "what happens when dragging element is destroyed" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<button draggable=\"true\">Draggable</button>\n    <div id=target>drop here</div>"
          _ <-
            evaluate
              page
              "() => { document.querySelector('#target').addEventListener('dragover', event => { document.querySelector('button')?.remove(); }, false); document.querySelector('#target').addEventListener('drop', event => { document.querySelector('#target').textContent = 'dropped'; }, false); }"
          dragAndDrop (locate page "button") (locate page "div")
          assertEqual "text" "drop here" =<< evaluateOnLocatorText (locate page "div") "div => div.textContent"
    ]

-- | Asserts the drop landed: the source ended up inside the target.
assertDropped :: Locator -> Playwright ()
assertDropped target =
  assertEqual "dropped" True
    =<< evaluateOnLocatorBool target "target => target.contains(document.querySelector('#source'))"

point :: Int -> Int -> Point
point x y =
  Point (cssPixelFromInt x) (cssPixelFromInt y)

onFrame :: Page -> Frame -> Text -> Locator
onFrame page frame selector =
  locatorOnFrame page frame (selectorFromText selector)

bodyHandle :: Locator -> Playwright ElementHandle
bodyHandle loc =
  requireHandle =<< elementHandle loc

-- | The first child frame of the page (the main frame is skipped).
firstChildFrame :: Page -> Playwright Frame
firstChildFrame page = do
  allFrames <- frames page
  case drop 1 allFrames of
    frame : _ ->
      pure frame
    [] ->
      assertFailure "expected a child frame"

-- | Makes the test iframe large and offset so the drag coordinates match the
-- expected positions.
styleFrame :: Page -> Playwright ()
styleFrame page = do
  _ <-
    evaluate
      page
      "() => { const iframe = document.querySelector('iframe'); iframe.style.width = '500px'; iframe.style.height = '600px'; iframe.style.marginLeft = '80px'; iframe.style.marginTop = '60px'; }"
  pure ()

-- | Records mouse events on the element as @type at x;y@ (@dragend@ has no
-- position, since browsers disagree on where it lands).
trackEvents :: ElementHandle -> Playwright JsHandle
trackEvents target =
  evaluateHandleOnElement
    target
    "target => { const events = []; for (const event of ['mousedown', 'mousemove', 'mouseup', 'dragstart', 'dragend', 'dragover', 'dragenter', 'dragleave', 'dragexit', 'drop']) { target.addEventListener(event, e => { if (event === 'dragend') events.push('dragend'); else events.push(`${event} at ${e.clientX};${e.clientY}`); }, false); } return events; }"

-- | Records mouse events on the document as @type at pageX;pageY@.
trackPageMouse :: Page -> Playwright JsHandle
trackPageMouse page = do
  body <- bodyHandle (locate page "body")
  evaluateHandleOnElement
    body
    "() => { const events = []; for (const type of ['mousedown', 'mousemove', 'mouseup']) { document.addEventListener(type, event => events.push(`${type} at ${event.pageX};${event.pageY}`)); } return events; }"

testIfDropped :: Page -> Text -> Text -> Playwright Bool
testIfDropped page effectAllowed dropEffect = do
  setContent
      page
      "\n        <div draggable=\"true\">drag target</div>\n        <drop-target>this is the drop target</drop-target>\n      "
  _ <-
    evaluate
      page
      ( "() => { window.dropped = false; document.querySelector('div').addEventListener('dragstart', event => { event.dataTransfer.effectAllowed = '"
          <> effectAllowed
          <> "'; event.dataTransfer.setData('text/plain', 'drag data'); }); const dropTarget = document.querySelector('drop-target'); dropTarget.addEventListener('dragover', event => { event.dataTransfer.dropEffect = '"
          <> dropEffect
          <> "'; event.preventDefault(); }); dropTarget.addEventListener('drop', () => { window.dropped = true; }); }"
      )
  hover (locate page "div")
  mouseDown page
  hover (locate page "drop-target")
  mouseUp page
  evaluateBool page "() => window.dropped"
