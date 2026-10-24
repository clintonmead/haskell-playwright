module Playwright.LocatorAnyFrameSpec
  ( tests
  ) where

import Control.Monad
  ( forM_
  , void
  )
import Data.Text
  ( Text
  , isInfixOf
  , pack
  , unpack
  )
import Data.Text qualified as Text
import Playwright
  ( AriaRole (AriaRole)
  , AriaSnapshotMode (AriaSnapshotModeAi)
  , AriaSnapshotOptions (ariaSnapshotOptionsMode)
  , ContentType (ContentType)
  , ExpectOptions (expectTimeout)
  , Exactness (Inexact)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , GotoOptions (gotoOptionsWaitUntil)
  , LoadState (LoadStateDomContentLoaded)
  , Locator
  , Page
  , Playwright
  , UrlMatch (UrlMatchGlob)
  , WaitForState (WaitForAttached)
  , click
  , contentFrame
  , defaultAriaSnapshotOptions
  , defaultExpectOptions
  , defaultFulfillOptions
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , evaluateOnFrame
  , evaluateOnFrameBool
  , evaluateOnLocator
  , evaluateOnLocatorAllTexts
  , evaluateOnLocatorText
  , expectLocator
  , expectLocatorCount
  , expectLocatorWith
  , filterHas
  , filterHasText
  , first
  , frameByUrl
  , frameLocator
  , frameLocatorOwner
  , frames
  , fulfillRoute
  , getAttribute
  , getByRole
  , goto
  , innerText
  , lastLocator
  , locator
  , locatorCount
  , locatorOnFrame
  , locatorToString
  , mainFrame
  , milliseconds
  , nth
  , orLocator
  , pageAriaSnapshotWith
  , route
  , selectorFromText
  , setDefaultTimeout
  , urlFromText
  , waitForSelectorHandleOnFrame
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , requireTimeout
  , withAssets
  )
import Playwright qualified as PW (childFrameLocator, childLocator)
import Playwright.Locator qualified as PWL (getByRole, getByText)
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Concurrent
  ( threadDelay
  )
import UnliftIO.Exception
  ( SomeException
  , displayException
  , try
  )

-- Ports of upstream tests/page/locator-any-frame.spec.ts.
--
-- Upstream's page.frameLocator() with no argument is the any-frame root
-- "internal:control=any-frame". Here it is spelled as anyFrame page and
-- extended with childLocator / childFrameLocator, which keeps the selector
-- chain the same as upstream's.
--
-- Text matchers (toHaveText and its array form) are not bound in the Haskell
-- Expect module. Those cases read innerText or evaluateAll instead, and the
-- array cases poll until the texts match.

tests :: TestTree
tests =
  testGroup
    "locator-any-frame"
    [ testCase "should click a button inside an iframe" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<button onclick=\"window.__clicked = true\">Click me</button>")
            ]
          gotoEmpty prefix page
          click (childLocator (anyFrame page) "internal:role=button[name=\"Click me\"i]")
          assertBool "button in iframe was clicked" =<< anyFrameClicked page
    , testCase "should click a button in the main frame" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe><button onclick=\"window.__clicked = true\">Click me</button>")
            , ("a.html", "<div>No buttons here</div>")
            ]
          gotoEmpty prefix page
          click (childLocator (anyFrame page) "button")
          assertBool "main button was clicked" =<< evaluateOnFrameBool (mainFrame page) clickedExpr
    , testCase "should fail click when elements match in multiple frames" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe><iframe src=\"b.html\"></iframe>")
            , ("a.html", "<button>one</button>")
            , ("b.html", "<button>two</button>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 3 "button"
          setTimeoutMs page (3000)
          assertErrorContains
            "click"
            ["frameLocator() matched elements in multiple frames"]
            (click (childLocator (anyFrame page) "button"))
    , testCase "should fail click upon strict mode violation inside a single frame" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<button>one</button><button>two</button>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 2 "button"
          setTimeoutMs page (3000)
          assertErrorContains
            "click"
            ["strict mode violation"]
            (click (childLocator (anyFrame page) "button"))
    , testCase "should time out on click when there are no matches" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<div>Nothing here</div>")
            ]
          gotoEmpty prefix page
          setTimeoutMs page (1000)
          assertErrorContains
            "click"
            ["Timeout 1000ms exceeded"]
            (click (childLocator (anyFrame page) "button"))
    , testCase "should count elements in a single frame" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<div>1</div><div>2</div><div>3</div>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 2 "div"
          assertEqual "div count" 3 =<< locatorCount (childLocator (anyFrame page) "div")
          assertEqual "button count" 0 =<< locatorCount (childLocator (anyFrame page) "button")
    , testCase "should fail count when elements match in multiple frames" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<div>main</div><iframe src=\"a.html\"></iframe>")
            , ("a.html", "<div>child</div>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 2 "div"
          assertErrorContains
            "count"
            ["frameLocator() matched elements in multiple frames"]
            (locatorCount (childLocator (anyFrame page) "div"))
    , testCase "should support toHaveCount" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<span>one</span><span>two</span>")
            ]
          gotoEmpty prefix page
          expectLocatorCount defaultExpectOptions (childLocator (anyFrame page) "span") 2
          expectLocatorCount defaultExpectOptions (childLocator (anyFrame page) "button") 0
    , testCase "should wait for a frame to appear with toHaveCount" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<div>No frames yet</div>")
            , ("a.html", "<span>one</span><span>two</span>")
            ]
          gotoEmpty prefix page
          void $
            evaluateOnFrame
              (mainFrame page)
              "() => { setTimeout(() => { const iframe = document.createElement('iframe'); iframe.src = 'a.html'; document.body.appendChild(iframe); }, 500); }"
          expectLocatorCount defaultExpectOptions (childLocator (anyFrame page) "span") 2
    , testCase "should fail toHaveCount when elements match in multiple frames" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe><iframe src=\"b.html\"></iframe>")
            , ("a.html", "<span>one</span>")
            , ("b.html", "<span>two</span>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 3 "span"
          assertErrorContains
            "toHaveCount"
            ["Expect failed"]
            ( expectLocatorWith
                defaultExpectOptions {expectTimeout = milliseconds 3000}
                (childLocator (anyFrame page) "span")
                "to.have.count 2"
            )
    , testCase "should support toHaveText" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<div>Hello iframe</div>")
            ]
          gotoEmpty prefix page
          assertEqual "text" "Hello iframe" =<< innerText (childLocator (anyFrame page) "div")
    , testCase "should support toHaveText with an array" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<span>one</span><span>two</span>")
            ]
          gotoEmpty prefix page
          pollTexts (childLocator (anyFrame page) "span") ["one", "two"]
    , testCase "should fail toHaveText when elements match in multiple frames" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe><iframe src=\"b.html\"></iframe>")
            , ("a.html", "<div>one</div>")
            , ("b.html", "<div>two</div>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 3 "div"
          setTimeoutMs page (3000)
          assertErrorContains
            "toHaveText (innerText)"
            ["frameLocator() matched elements in multiple frames"]
            (innerText (childLocator (anyFrame page) "div"))
    , testCase "should fail toHaveText with an array when elements match in multiple frames" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe><iframe src=\"b.html\"></iframe>")
            , ("a.html", "<span>one</span>")
            , ("b.html", "<span>two</span>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 3 "span"
          setTimeoutMs page (3000)
          assertErrorContains
            "toHaveText array (evaluateAll)"
            ["frameLocator() matched elements in multiple frames"]
            (evaluateOnLocatorAllTexts (childLocator (anyFrame page) "span") textsExpr)
    , testCase "should fail toHaveText upon strict mode violation inside a single frame" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<div>one</div><div>two</div>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 2 "div"
          setTimeoutMs page (3000)
          assertErrorContains
            "toHaveText (innerText)"
            ["strict mode violation"]
            (innerText (childLocator (anyFrame page) "div"))
    , testCase "should support evaluate" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<div data-foo=\"bar\">Hello</div>")
            ]
          gotoEmpty prefix page
          assertEqual "data-foo" "bar"
            =<< evaluateOnLocatorText (childLocator (anyFrame page) "div") "e => e.getAttribute('data-foo')"
    , testCase "should fail evaluate when elements match in multiple frames" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe><iframe src=\"b.html\"></iframe>")
            , ("a.html", "<div>one</div>")
            , ("b.html", "<div>two</div>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 3 "div"
          setTimeoutMs page (3000)
          assertErrorContains
            "evaluate"
            ["frameLocator() matched elements in multiple frames"]
            (evaluateOnLocator (childLocator (anyFrame page) "div") "e => e.textContent")
    , testCase "should time out on evaluate when there are no matches" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<div>Nothing here</div>")
            ]
          gotoEmpty prefix page
          setTimeoutMs page (1000)
          assertErrorContains
            "evaluate"
            ["Failed to find element matching selector"]
            (evaluateOnLocator (childLocator (anyFrame page) "button") "e => e.textContent")
    , testCase "should support evaluateAll" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<span>one</span><span>two</span>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 2 "span"
          assertEqual "texts" ["one", "two"]
            =<< evaluateOnLocatorAllTexts (childLocator (anyFrame page) "span") textsExpr
          assertEqual "button tags" []
            =<< evaluateOnLocatorAllTexts (childLocator (anyFrame page) "button") "els => els.map(e => e.tagName)"
    , testCase "should fail evaluateAll when elements match in multiple frames" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe><iframe src=\"b.html\"></iframe>")
            , ("a.html", "<span>one</span>")
            , ("b.html", "<span>two</span>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 3 "span"
          assertErrorContains
            "evaluateAll"
            ["frameLocator() matched elements in multiple frames"]
            (evaluateOnLocatorAllTexts (childLocator (anyFrame page) "span") "els => els.map(e => e.tagName)")
    , testCase "should support hasText filter" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<div>foo</div><div>bar</div>")
            ]
          gotoEmpty prefix page
          assertEqual "text" "bar"
            =<< innerText (filterHasText (childLocator (anyFrame page) "div") "bar")
    , testCase "should support first/last/nth" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<span>one</span><span>two</span><span>three</span>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 2 "span"
          let spans = childLocator (anyFrame page) "span"
          assertEqual "first" "one" =<< innerText (first spans)
          assertEqual "last" "three" =<< innerText (lastLocator spans)
          assertEqual "nth 1" "two" =<< innerText (nth spans 1)
    , testCase "should support nth in the middle of the chain" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<div><span>one</span></div><div><span>two</span></div>")
            ]
          gotoEmpty prefix page
          assertEqual "text" "two"
            =<< innerText (childLocator (nth (childLocator (anyFrame page) "div") 1) "span")
    , testCase "should support capture" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<div id=\"target\"><span>hello</span></div>")
            ]
          gotoEmpty prefix page
          let captured = childLocator (anyFrame page) "*css=div >> span"
          waitForChildFrames page 2 "span"
          assertEqual "id" (Just "target") =<< getAttribute captured "id"
    , testCase "should not match a chain across a frame boundary" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<section><iframe src=\"a.html\"></iframe></section>")
            , ("a.html", "<iframe src=\"b.html\"></iframe>")
            , ("b.html", "<button>deep</button>")
            ]
          gotoEmpty prefix page
          waitForFrameCount page 3
          settle
          assertEqual "section chain count" 0
            =<< locatorCount (childLocator (childLocator (anyFrame page) "section") "button")
          assertEqual "button text" "deep"
            =<< innerText (childLocator (anyFrame page) "button")
    , testCase "should enter a frame found in a nested frame" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe><button>main</button>")
            , ("a.html", "<iframe id=\"target\" src=\"b.html\"></iframe><button>decoy</button>")
            , ("b.html", "<button>inside</button>")
            ]
          gotoEmpty prefix page
          assertEqual "text" "inside"
            =<< innerText (childLocator (childFrameLocator (anyFrame page) "#target") "button")
    , testCase "should click inside an entered frame" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<iframe id=\"target\" src=\"b.html\"></iframe><button>Click me</button>")
            , ("b.html", "<button onclick=\"window.__clicked = true\">Click me</button>")
            ]
          gotoEmpty prefix page
          waitForFrameCount page 3
          click (childLocator (childFrameLocator (anyFrame page) "#target") "internal:role=button[name=\"Click me\"i]")
          assertBool "button in entered frame was clicked" =<< anyFrameClicked page
    , testCase "should not search nested frames after entering a frame" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe id=\"target\" src=\"a.html\"></iframe><button>main</button>")
            , ("a.html", "<iframe src=\"b.html\"></iframe>")
            , ("b.html", "<button>deep</button>")
            ]
          gotoEmpty prefix page
          waitForFrameCount page 3
          settle
          assertEqual "count" 0
            =<< locatorCount (childLocator (childFrameLocator (anyFrame page) "#target") "button")
    , testCase "should support two frameLocators" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<iframe id=\"x\" src=\"b.html\"></iframe>")
            , ("b.html", "<iframe id=\"y\" src=\"c.html\"></iframe><button>decoy</button>")
            , ("c.html", "<button>bottom</button>")
            ]
          gotoEmpty prefix page
          assertEqual "text" "bottom"
            =<< innerText
              ( childLocator
                  (childFrameLocator (childFrameLocator (anyFrame page) "#x") "#y")
                  "button"
              )
    , testCase "should support locator before frameLocator" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<section><iframe src=\"b.html\"></iframe></section><iframe src=\"c.html\"></iframe>")
            , ("b.html", "<button>in-section</button>")
            , ("c.html", "<button>outside</button>")
            ]
          gotoEmpty prefix page
          assertEqual "text" "in-section"
            =<< innerText
              ( childLocator
                  (childFrameLocator (childLocator (anyFrame page) "section") "iframe")
                  "button"
              )
    , testCase "should support owner of a frameLocator" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<iframe id=\"target\" src=\"b.html\"></iframe>")
            , ("b.html", "<button>inside</button>")
            ]
          gotoEmpty prefix page
          waitForFrameCount page 3
          assertEqual "owner id" (Just "target")
            =<< getAttribute (frameLocatorOwner (childFrameLocator (anyFrame page) "#target")) "id"
    , testCase "should wait for the frame to enter to appear" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<div>Nothing yet</div>")
            , ("b.html", "<button>late</button>")
            ]
          gotoEmpty prefix page
          waitForFrameCount page 2
          frameList <- frames page
          case drop 1 frameList of
            (child : _) ->
              void $
                evaluateOnFrame
                  child
                  "() => { setTimeout(() => { const iframe = document.createElement('iframe'); iframe.id = 'late'; iframe.src = 'b.html'; document.body.appendChild(iframe); }, 3000); }"
            [] -> assertFailure "expected a child frame"
          assertEqual "text" "late"
            =<< innerText (childLocator (childFrameLocator (anyFrame page) "#late") "button")
    , testCase "should fail when the frame to enter matches in multiple frames" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe><iframe src=\"b.html\"></iframe>")
            , ("a.html", "<iframe class=\"inner\" src=\"c.html\"></iframe>")
            , ("b.html", "<iframe class=\"inner\" src=\"c.html\"></iframe>")
            , ("c.html", "<button>Click me</button>")
            ]
          gotoEmpty prefix page
          waitForFrameCount page 5
          settle
          setTimeoutMs page (3000)
          assertErrorContains
            "click"
            ["frameLocator() matched elements in multiple frames"]
            (click (childLocator (childFrameLocator (anyFrame page) ".inner") "button"))
    , testCase "should support contentFrame" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<iframe id=\"target\" src=\"b.html\"></iframe><button>decoy</button>")
            , ("b.html", "<button>inside</button>")
            ]
          gotoEmpty prefix page
          assertEqual "text" "inside"
            =<< innerText (childLocator (contentFrame (childLocator (anyFrame page) "#target")) "button")
    , testCase "should support toBeVisible while another iframe is stalled" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe><iframe src=\"stall.html\"></iframe>")
            , ("a.html", "<button>Click me</button>")
            ]
          stallRoute page
          gotoDomContentLoaded prefix page
          waitForFrameCount page 3
          expectLocator (childLocator (anyFrame page) "button") "to.be.visible"
    , testCase "should support toHaveCount while another iframe is stalled" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe><iframe src=\"stall.html\"></iframe>")
            , ("a.html", "<button>Click me</button>")
            ]
          stallRoute page
          gotoDomContentLoaded prefix page
          waitForFrameCount page 3
          expectLocatorCount defaultExpectOptions (childLocator (anyFrame page) "button") 1
    , testCase "should support composite locators" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<div><span>foo</span></div><div><i>bar</i></div>")
            ]
          gotoEmpty prefix page
          assertEqual "text" "foo"
            =<< innerText
              ( filterHas
                  (childLocator (anyFrame page) "div")
                  (locator page (selectorFromText "span"))
              )
    , testCase "should only search frames inside the starting frame" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe><button>main</button>")
            , ("a.html", "<iframe src=\"b.html\"></iframe>")
            , ("b.html", "<button>deep</button>")
            ]
          gotoEmpty prefix page
          waitForFrameCount page 3
          settle
          middle <- frameByUrl page (UrlMatchGlob "**/a.html")
          case middle of
            Nothing -> assertFailure "expected the a.html frame"
            Just frame ->
              assertEqual "text" "deep"
                =<< innerText
                  (childLocator (locatorOnFrame page frame (selectorFromText "internal:control=any-frame")) "button")
    , testCase "should render frameLocator() in the locator description" $
        withAssets $ \_ page -> do
          assertEqual
            "description"
            "frameLocator().locator('#x').contentFrame().locator('button')"
            (locatorToString (childLocator (childFrameLocator (anyFrame page) "#x") "button"))
          assertEqual
            "description"
            "frameLocator().locator('section').locator('iframe').contentFrame().getByText('foo')"
            ( locatorToString
                ( PWL.getByText
                    (childFrameLocator (childLocator (anyFrame page) "section") "iframe")
                    "foo"
                    Inexact
                )
            )
    , testCase "should not allow frameLocator() inside a composite locator" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<button>main</button><iframe src=\"a.html\"></iframe>")
            , ("a.html", "<a href=\"#\">link</a>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 2 "a"
          assertErrorContains
            "or"
            [ "frameLocator() is not allowed inside composite locators, while querying"
            , "locator('button').or(frameLocator().locator('a'))"
            ]
            (locatorCount (orLocator (locator page (selectorFromText "button")) (childLocator (anyFrame page) "a")))
          assertErrorContains
            "filter has"
            ["frameLocator() is not allowed inside composite locators"]
            ( locatorCount
                ( filterHas
                    (locator page (selectorFromText "button"))
                    (childLocator (anyFrame page) "a")
                )
            )
          assertErrorContains
            "repeated frameLocator"
            [ "frameLocator() is not allowed inside composite locators, while querying"
            , "frameLocator().locator('button').or(frameLocator().locator('a'))"
            ]
            ( locatorCount
                ( orLocator
                    (childLocator (anyFrame page) "button")
                    (childLocator (anyFrame page) "a")
                )
            )
          assertErrorContains
            "repeated frameLocator in chain"
            ["frameLocator() is not allowed inside composite locators"]
            ( locatorCount
                ( orLocator
                    (childLocator (childFrameLocator (anyFrame page) "#f") "a")
                    (childLocator (childFrameLocator (anyFrame page) "#f") "button")
                )
            )
          assertErrorContains
            "leading frameLocator"
            ["frameLocator() matched elements in multiple frames"]
            ( locatorCount
                ( orLocator
                    (childLocator (anyFrame page) "a")
                    (locator page (selectorFromText "button"))
                )
            )
    , testCase "should support a composite locator under frameLocator()" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<div class=\"classname\">first</div><button>second</button>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 2 "button"
          pollTexts
            ( orLocator
                (childLocator (anyFrame page) ".classname")
                (getByRole page (AriaRole "button") defaultGetByRoleOptions)
            )
            ["first", "second"]
    , testCase "should support a composite locator under frameLocator() and a frame locator" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe>")
            , ("a.html", "<iframe id=\"f\" src=\"b.html\"></iframe>")
            , ("b.html", "<div class=\"classname\">first</div><button>second</button>")
            ]
          gotoEmpty prefix page
          waitForFrameCount page 3
          pollTexts
            ( orLocator
                (childLocator (childFrameLocator (anyFrame page) "#f") ".classname")
                (PWL.getByRole (frameLocator page (selectorFromText "#f")) (AriaRole "button") defaultGetByRoleOptions)
            )
            ["first", "second"]
    , testCase "should not allow owner on frameLocator()" $
        withAssets $ \_ page ->
          assertErrorContains
            "owner"
            ["Selector cannot be empty after frameLocator()"]
            (locatorCount (frameLocatorOwner (anyFrame page)))
    , testCase "should resolve aria-ref selectors" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<button>main</button><iframe src=\"a.html\"></iframe>")
            , ("a.html", "<button>inside</button>")
            ]
          gotoEmpty prefix page
          waitForChildFrames page 2 "button"
          snapshot <-
            pageAriaSnapshotWith
              page
              defaultAriaSnapshotOptions {ariaSnapshotOptionsMode = Just AriaSnapshotModeAi}
          case ariaRefAfter "button \"inside\" [ref=" snapshot of
            Nothing -> assertFailure ("no ref for the inside button in " <> unpack snapshot)
            Just ref -> do
              assertBool "ref shape" ("f" `Text.isPrefixOf` ref)
              assertEqual "text" "inside"
                =<< innerText (childLocator (anyFrame page) ("aria-ref=" <> ref))
    , testCase "should click while another iframe is stalled" $
        withAssets $ \prefix page -> do
          serve
            page
            [ ("empty.html", "<iframe src=\"a.html\"></iframe><iframe src=\"stall.html\"></iframe>")
            , ("a.html", "<button onclick=\"window.__clicked = true\">Click me</button>")
            ]
          stallRoute page
          gotoDomContentLoaded prefix page
          waitForFrameCount page 3
          click (childLocator (anyFrame page) "button")
          middle <- frameByUrl page (UrlMatchGlob "**/a.html")
          case middle of
            Nothing -> assertFailure "expected the a.html frame"
            Just frame -> assertBool "button in a.html was clicked" =<< evaluateOnFrameBool frame clickedExpr
    ]

clickedExpr :: Text
clickedExpr =
  "() => window.__clicked === true"

textsExpr :: Text
textsExpr =
  "els => els.map(e => e.textContent)"

-- | The root of upstream's page.frameLocator() with no argument. Extending it
-- with childLocator or childFrameLocator keeps the any-frame token first.
anyFrame :: Page -> Locator
anyFrame page =
  locator page (selectorFromText "internal:control=any-frame")

-- | True when any frame, including the main frame, has __clicked set.
anyFrameClicked :: Page -> Playwright Bool
anyFrameClicked page = do
  frameList <- frames page
  results <- mapM (\frame -> evaluateOnFrameBool frame clickedExpr) frameList
  pure (or results)

serve :: Page -> [(Text, Text)] -> Playwright ()
serve page routes =
  forM_ routes $ \(name, body) ->
    route page (UrlMatchGlob ("**/" <> name)) $ \handled ->
      fulfillRoute
        handled
        defaultFulfillOptions
          { fulfillContentType = Just (ContentType "text/html")
          , fulfillBody = Just (FulfillText body)
          }

stallRoute :: Page -> Playwright ()
stallRoute page =
  route page (UrlMatchGlob "**/stall.html") $ \_ -> pure ()

gotoEmpty :: Text -> Page -> Playwright ()
gotoEmpty prefix page = do
  _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
  pure ()

-- | Navigates without waiting for the stalled iframe to finish loading.
gotoDomContentLoaded :: Text -> Page -> Playwright ()
gotoDomContentLoaded prefix page = do
  _ <-
    goto
      page
      (urlFromText (prefix <> "/empty.html"))
      defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateDomContentLoaded}
  pure ()

-- | Polls until the page reports the expected number of frames, including the
-- main frame.
waitForFrameCount :: Page -> Int -> Playwright ()
waitForFrameCount page expected = go (100 :: Int)
  where
    go :: Int -> Playwright ()
    go remaining = do
      count <- length <$> frames page
      if count == expected
        then pure ()
        else
          if remaining <= 0
            then assertEqual "frame count" expected count
            else threadDelay 100000 >> go (remaining - 1)

-- | Waits for the frame count, then for the selector to attach in each child frame.
waitForChildFrames :: Page -> Int -> Text -> Playwright ()
waitForChildFrames page expected selector = do
  waitForFrameCount page expected
  frameList <- frames page
  forM_ (drop 1 frameList) $ \frame ->
    waitForSelectorHandleOnFrame frame (selectorFromText selector) WaitForAttached

-- | Gives stalled and nested frames a moment to finish rendering their content.
settle :: Playwright ()
settle =
  threadDelay 500000

-- | Polls the texts of a locator until they match, since the text matchers are
-- not bound in the Haskell Expect module.
pollTexts :: Locator -> [Text] -> Playwright ()
pollTexts loc expected = go (100 :: Int)
  where
    go :: Int -> Playwright ()
    go remaining = do
      actual <- evaluateOnLocatorAllTexts loc textsExpr
      if actual == expected
        then pure ()
        else
          if remaining <= 0
            then assertEqual "texts" expected actual
            else threadDelay 100000 >> go (remaining - 1)

-- | Runs the action, expecting it to fail with a message containing each needle.
assertErrorContains :: String -> [Text] -> Playwright a -> Playwright ()
assertErrorContains label needles action = do
  result <- try (void action)
  case result of
    Left err -> do
      let message = pack (displayException (err :: SomeException))
      forM_ needles $ \needle ->
        assertBool
          (label <> ": error should contain " <> unpack needle <> ", got " <> unpack message)
          (needle `isInfixOf` message)
    Right () ->
      assertFailure (label <> ": expected an error containing " <> show needles)

-- | Sets the page default timeout in milliseconds.
setTimeoutMs :: Page -> Double -> Playwright ()
setTimeoutMs page ms =
  setDefaultTimeout page =<< requireTimeout ms

-- | The aria ref that follows the given prefix in an aria snapshot.
ariaRefAfter :: Text -> Text -> Maybe Text
ariaRefAfter prefix snapshot =
  case Text.breakOn prefix snapshot of
    (_, rest)
      | Text.null rest -> Nothing
      | otherwise -> Just (Text.takeWhile (/= ']') (Text.drop (Text.length prefix) rest))

-- | Extends a locator with a CSS or internal selector given as text.
childLocator :: Locator -> Text -> Locator
childLocator loc sel =
  PW.childLocator loc (selectorFromText sel)

-- | Enters a frame matched by a selector given as text.
childFrameLocator :: Locator -> Text -> Locator
childFrameLocator loc sel =
  PW.childFrameLocator loc (selectorFromText sel)
