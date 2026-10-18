module Playwright.SelectorsMiscSpec
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
  ( forM_
  )
import Data.Maybe
  ( isNothing
  )
import Data.Text
  ( Text
  , intercalate
  , isInfixOf
  , unpack
  )
import Playwright
  ( Page
  , WaitForState (WaitForAttached, WaitForHidden, WaitForVisible)
  , click
  , evalOnSelector
  , evalOnSelectorAllNumber
  , evalOnSelectorAllTexts
  , evalOnSelectorHandleText
  , evalOnSelectorText
  , evalOnSelectorWith
  , evaluate
  , evaluateNumber
  , evaluateOnElementText
  , isVisible
  , locator
  , locatorCount
  , querySelector
  , querySelectorAll
  , selectorFromText
  , setContent
  , textContent
  , waitForSelector
  , waitForSelectorHandle
  , waitForSelectorHandleWith
  , Playwright
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( requireTimeout
  , assertProtocolContains
  , locate
  , requireHandle
  , withAssetPage
  , withChromiumPage
  , assertBool
  , assertEqual
  , assertFailure
  )
import Playwright.SerializedArgument
  ( SerializedArgument (SerializedArgument, serializedArgumentHandles, serializedArgumentValue)
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadArray, PayloadNumber)
  , SerializedValue (SerializedNode)
  )
import Test.Tasty
  ( TestTree
  , localOption
  , mkTimeout
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

tests :: TestTree
tests =
  testGroup
    "selectors-misc"
    [ testCase "should work for open shadow roots" $
        withAssetPage "deep-shadow.html" $ \page -> do
          assertText page "id=target" "Hello from root2"
          assertText page "data-testid=foo" "Hello from root1"
          assertCount page "data-testid=foo" 3
          assertMissing page "id:light=target"
          assertMissing page "data-testid:light=foo"
          assertEqual "light all" 0 . length
            =<< querySelectorAll page (selectorFromText "data-testid:light=foo")
    , testCase "should click on links in shadow dom" $
        withAssetPage "shadow-dom-link.html" $ \page -> do
          assertEqual "before" 0 =<< evaluateNumber page "() => window.clickCount"
          click (locate page "#inner-link")
          assertEqual "after" 1 =<< evaluateNumber page "() => window.clickCount"
    , testCase "should work with :visible" $
        withChromiumPage $ \page -> do
          setContent page "<section><div id=target1></div><div id=target2></div></section>"
          assertMissing page "div:visible"
          timeout <- requireTimeout 1000
          assertProtocolContains
            "1000ms"
            (waitForSelectorHandleWith page (selectorFromText "div:visible") WaitForVisible timeout)
          found <-
            withAsync (waitForSelectorHandle page (selectorFromText "div:visible") WaitForAttached) $ \pending -> do
              _ <- evalOnSelector page (selectorFromText "#target2") "div => { div.textContent = 'Now visible'; }"
              wait pending
          el <- requireHandle found
          assertEqual "id" "target2" =<< evaluateOnElementText el "e => e.id"
          assertId page "div:visible" "target2"
    , testCase "should work with >> visible=" $
        withChromiumPage $ \page -> do
          setContent page "<section><div id=target1></div><div id=target2></div></section>"
          assertMissing page "div >> visible=true"
          timeout <- requireTimeout 1000
          assertProtocolContains
            "1000ms"
            (waitForSelectorHandleWith page (selectorFromText "div >> visible=true") WaitForVisible timeout)
          found <-
            withAsync (waitForSelectorHandle page (selectorFromText "div >> visible=true") WaitForAttached) $ \pending -> do
              _ <- evalOnSelector page (selectorFromText "#target2") "div => { div.textContent = 'Now visible'; }"
              wait pending
          el <- requireHandle found
          assertEqual "id" "target2" =<< evaluateOnElementText el "e => e.id"
          assertId page "div >> visible=true" "target2"
    , testCase "should work with >> visible=false" $
        withChromiumPage $ \page -> do
          setContent page "<section><div id=target1></div><div id=target2></div></section>"
          assertEqual "hidden 2" 2 =<< locatorCount (locator page (selectorFromText "div >> visible=false"))
          _ <- evalOnSelector page (selectorFromText "#target2") "div => { div.textContent = 'Now visible'; }"
          assertEqual "hidden 1" 1 =<< locatorCount (locator page (selectorFromText "div >> visible=false"))
    , testCase "should work with :nth-match" $
        withChromiumPage $ \page -> do
          setContent page "<section><div id=target1></div><div id=target2></div></section>"
          assertMissing page ":nth-match(div, 3)"
          assertId page ":nth-match(div, 1)" "target1"
          assertId page ":nth-match(div, 2)" "target2"
          assertId page ":nth-match(section > div, 2)" "target2"
          assertId page ":nth-match(section, div, 2)" "target1"
          assertId page ":nth-match(div, section, 3)" "target2"
          assertCount page ":is(:nth-match(div, 1), :nth-match(div, 2))" 2
          assertProtocolContains
            "\"nth-match\" engine expects a one-based index as the last argument"
            (querySelector page (selectorFromText ":nth-match(div, bar, 0)"))
          assertProtocolContains
            "\"nth-match\" engine expects non-empty selector list and an index argument"
            (querySelector page (selectorFromText ":nth-match(2)"))
          assertProtocolContains
            "\"nth-match\" engine expects a one-based index as the last argument"
            (querySelector page (selectorFromText ":nth-match(div, bar, foo)"))
          found <-
            withAsync (waitForSelectorHandle page (selectorFromText ":nth-match(div, 3)") WaitForAttached) $ \pending -> do
              _ <- evalOnSelector page (selectorFromText "section") appendTarget3
              wait pending
          el <- requireHandle found
          assertEqual "id" "target3" =<< evaluateOnElementText el "e => e.id"
    , testCase "should work with nth=" $
        withChromiumPage $ \page -> do
          setContent page "<section><div id=target1></div><div id=target2></div></section>"
          assertMissing page "div >> nth=2"
          assertId page "div >> nth=0" "target1"
          assertId page "div >> nth=1" "target2"
          assertId page "section > div >> nth=1" "target2"
          assertId page "section, div >> nth=1" "target1"
          assertId page "div, section >> nth=2" "target2"
          found <-
            withAsync (waitForSelectorHandle page (selectorFromText "div >> nth=2") WaitForAttached) $ \pending -> do
              _ <- evalOnSelector page (selectorFromText "section") appendTarget3
              wait pending
          el <- requireHandle found
          assertEqual "id" "target3" =<< evaluateOnElementText el "e => e.id"
          setContent page "<div><div><div><span>hi</span><span>hello</span></div></div></div>"
          assertEqual "nth text" (Just "hello")
            =<< textContent (locator page (selectorFromText "div >> div >> span >> nth=1"))
    , testCase "should work with strict mode and chaining" $
        withChromiumPage $ \page -> do
          setContent page "<div><div><div><span>hi</span></div></div></div>"
          assertEqual "text" (Just "hi")
            =<< textContent (locator page (selectorFromText "div >> div >> span"))
    , localOption (mkTimeout 30000000) $
        testCase "should work with layout selectors" $
          withChromiumPage $ \page -> do
            setContent page "<container style=\"width: 500px; height: 500px; position: relative;\"></container>"
            _ <- evalOnSelectorWith page (selectorFromText "container") layoutScript boxesArgument
            assertId page "div:right-of(#id6)" "id7"
            assertId page "div:right-of(#id1)" "id2"
            assertId page "div:right-of(#id3)" "id4"
            assertMissing page "div:right-of(#id4)"
            assertId page "div:right-of(#id0)" "id7"
            assertId page "div:right-of(#id8)" "id9"
            assertIds page "div:right-of(#id3)" "id4,id2,id5,id7,id8,id9"
            assertIds page "div:right-of(#id3, 50)" "id2,id5,id7,id8"
            assertIds page "div:right-of(#id3, 49)" "id7,id8"
            assertId page "div:left-of(#id2)" "id1"
            assertMissing page "div:left-of(#id0)"
            assertId page "div:left-of(#id5)" "id0"
            assertId page "div:left-of(#id9)" "id8"
            assertId page "div:left-of(#id4)" "id3"
            assertIds page "div:left-of(#id5)" "id0,id7,id3,id1,id6,id8"
            assertIds page "div:left-of(#id5, 3)" "id7,id8"
            assertId page "div:above(#id0)" "id3"
            assertId page "div:above(#id5)" "id4"
            assertId page "div:above(#id7)" "id5"
            assertId page "div:above(#id8)" "id0"
            assertId page "div:above(#id9)" "id8"
            assertMissing page "div:above(#id2)"
            assertIds page "div:above(#id5)" "id4,id2,id3,id1"
            assertIds page "div:above(#id5, 20)" "id4,id3"
            assertId page "div:below(#id4)" "id5"
            assertId page "div:below(#id3)" "id0"
            assertId page "div:below(#id2)" "id4"
            assertId page "div:below(#id6)" "id8"
            assertId page "div:below(#id7)" "id8"
            assertId page "div:below(#id8)" "id9"
            assertMissing page "div:below(#id9)"
            assertIds page "div:below(#id3)" "id0,id5,id6,id7,id8,id9"
            assertIds page "div:below(#id3, 105)" "id0,id5,id6,id7"
            assertId page "div:near(#id0)" "id3"
            assertIds page "div:near(#id7)" "id0,id5,id3,id6"
            assertIds page "div:near(#id0)" "id3,id6,id7,id8,id1,id5"
            assertIds page "div:near(#id6)" "id0,id3,id7"
            assertIds page "div:near(#id6, 10)" "id0"
            assertIds page "div:near(#id0, 100)" "id3,id6,id7,id8,id1,id5,id4,id2"
            assertIds page "div:below(#id5):above(#id8)" "id7,id6"
            assertId page "div:below(#id5):above(#id8)" "id7"
            assertIds page "div:right-of(#id0) + div:above(#id8)" "id5,id6,id3"
            assertProtocolContains
              "\"near\" engine expects a selector list and optional maximum distance in pixels"
              (querySelector page (selectorFromText ":near(50)"))
    , testCase "should escape the scope with >>" $
        withChromiumPage $ \page -> do
          setContent page "<div><label>Test</label><input id='myinput'></div>"
          assertId page "label >> xpath=.. >> input" "myinput"
    , testCase "xpath should be relative" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<span class=\"find-me\" id=target1>1</span><div><span class=\"find-me\" id=target2>2</span></div>"
          assertId page "//*[@class=\"find-me\"]" "target1"
          divHandle <- requireHandle =<< querySelector page (selectorFromText "div")
          assertEqual "child" "target2"
            =<< evalOnSelectorHandleText divHandle (selectorFromText "xpath=./*[@class=\"find-me\"]") "e => e.id"
          assertEqual "desc" "target2"
            =<< evalOnSelectorHandleText divHandle (selectorFromText "xpath=.//*[@class=\"find-me\"]") "e => e.id"
          assertEqual "bare" "target2"
            =<< evalOnSelectorHandleText divHandle (selectorFromText "//*[@class=\"find-me\"]") "e => e.id"
          assertEqual "root" "target2"
            =<< evalOnSelectorHandleText divHandle (selectorFromText "xpath=/*[@class=\"find-me\"]") "e => e.id"
          assertId page "div >> xpath=./*[@class=\"find-me\"]" "target2"
          assertId page "div >> xpath=.//*[@class=\"find-me\"]" "target2"
          assertId page "div >> //*[@class=\"find-me\"]" "target2"
          assertId page "div >> xpath=/*[@class=\"find-me\"]" "target2"
    , testCase "should work with pipe in xpath" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<span class=\"find-me\" id=t1>1</span><div><span class=\"find-me\" id=t2>2</span></div><div id=t3>3</span>"
          assertCount page "//*[@id=\"t1\"]|//*[@id=\"t3\"]" 2
          e1 <- requireHandle =<< waitForSelectorHandle page (selectorFromText "//*[@id=\"t1\"]|//*[@id=\"t3\"]") WaitForVisible
          assertEqual "t1" "t1" =<< evaluateOnElementText e1 "e => e.id"
          e2 <- requireHandle =<< waitForSelectorHandle page (selectorFromText "//*[@id=\"unknown\"]|//*[@id=\"t2\"]") WaitForVisible
          assertEqual "t2" "t2" =<< evaluateOnElementText e2 "e => e.id"
          click (locate page "//code|//span[@id=\"t2\"]")
    , testCase "should print original xpath in error" $
        withChromiumPage $ \page -> do
          result <- try (isVisible (locator page (selectorFromText "//*[contains(@Class, 'foo']")))
          case result of
            Left (ProtocolRpc failure) -> do
              let
                message :: Text
                message = protocolFailureMessage failure
              assertBool "original" ("//*[contains(@Class, 'foo']" `isInfixOf` message)
              assertBool "not rewritten" (not (".//*[contains(@Class, 'foo']" `isInfixOf` message))
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected protocol error"
    , testCase "data-testid on the handle should be relative" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<span data-testid=\"find-me\" id=target1>1</span><div><span data-testid=\"find-me\" id=target2>2</span></div>"
          assertId page "data-testid=find-me" "target1"
          divHandle <- requireHandle =<< querySelector page (selectorFromText "div")
          assertEqual "handle" "target2"
            =<< evalOnSelectorHandleText divHandle (selectorFromText "data-testid=find-me") "e => e.id"
          assertId page "div >> data-testid=find-me" "target2"
    , testCase "should properly determine visibility of display:contents elements" $
        withChromiumPage $ \page -> do
          setContent page "<div><p style=\"display:contents\">DISPLAY CONTENTS</p></div>"
          waitForSelector page (selectorFromText "\"DISPLAY CONTENTS\"") WaitForVisible
          setContent page "<div><article style=\"display:contents\"><div>DISPLAY CONTENTS</div></article></div>"
          waitForSelector page (selectorFromText "article") WaitForVisible
          setContent page "<div><article style=\"display:contents\"><div style=\"display:contents\">DISPLAY CONTENTS</div></article></div>"
          waitForSelector page (selectorFromText "article") WaitForVisible
          setContent page "<div><article style=\"display:contents\"><div></div>DISPLAY CONTENTS<span></span></article></div>"
          waitForSelector page (selectorFromText "article") WaitForVisible
          setContent page "<div><article style=\"display:contents\"><div></div></article></div>"
          waitForSelector page (selectorFromText "article") WaitForHidden
    , testCase "should work with internal:has=" $
        withAssetPage "deep-shadow.html" $ \page -> do
          assertCount page "div >> internal:has=\"#target\"" 2
          assertCount page "div >> internal:has=\"[data-testid=foo]\"" 3
          assertCount page "div >> internal:has=\"[attr*=value]\"" 2
          setContent page "<section><span></span><div></div></section><section><br></section>"
          assertCount page "section >> internal:has=\"span, div\"" 1
          assertCount page "section >> internal:has=\"br\"" 1
          assertCount page "section >> internal:has=\"span, br\"" 2
          assertCount page "section >> internal:has=\"span, br, div\"" 2
          setContent page "<div><span>hello</span></div><div><span>world</span></div>"
          assertCount page "div >> internal:has=\"text=world\"" 1
          assertHtml page "div >> internal:has=\"text=world\"" "<div><span>world</span></div>"
          assertCount page "div >> internal:has=\"text=\\\"hello\\\"\"" 1
          assertHtml page "div >> internal:has=\"text=\\\"hello\\\"\"" "<div><span>hello</span></div>"
          assertCount page "div >> internal:has=\"xpath=./span\"" 2
          assertCount page "div >> internal:has=\"span\"" 2
          assertCount page "div >> internal:has=\"span >> text=wor\"" 1
          assertHtml page "div >> internal:has=\"span >> text=wor\"" "<div><span>world</span></div>"
          assertHtml page "div >> internal:has=\"span >> text=wor\" >> span" "<span>world</span>"
          assertProtocolContains "Malformed selector: internal:has=abc" (querySelector page (selectorFromText "div >> internal:has=abc"))
          assertProtocolContains "\"internal:has\" selector cannot be first" (querySelector page (selectorFromText "internal:has=\"div\""))
          assertProtocolContains "Malformed selector: internal:has=33" (querySelector page (selectorFromText "div >> internal:has=33"))
          assertProtocolContains
            "Unexpected token \"!\" while parsing css selector \"span!\""
            (querySelector page (selectorFromText "div >> internal:has=\"span!\""))
    , testCase "should work with internal:has-not=" $
        withChromiumPage $ \page -> do
          setContent page "<section><span></span><div></div></section><section><br></section>"
          assertCount page "section >> internal:has-not=\"span\"" 1
          assertCount page "section >> internal:has-not=\"span, div, br\"" 0
          assertCount page "section >> internal:has-not=\"br\"" 1
          assertCount page "section >> internal:has-not=\"span, div\"" 1
          assertCount page "section >> internal:has-not=\"article\"" 2
    , testCase "should work with internal:and=" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div class=foo>hello</div><div class=bar>world</div><span class=foo>hello2</span><span class=bar>world2</span>"
          assertTexts page "div >> internal:and=\"span\"" []
          assertTexts page "div >> internal:and=\".foo\"" ["hello"]
          assertTexts page "div >> internal:and=\".bar\"" ["world"]
          assertTexts page "span >> internal:and=\"span\"" ["hello2", "world2"]
          assertTexts page ".foo >> internal:and=\"div\"" ["hello"]
          assertTexts page ".bar >> internal:and=\"span\"" ["world2"]
    , testCase "should work with internal:or=" $
        withChromiumPage $ \page -> do
          setContent page "<div>hello</div><span>world</span>"
          assertTexts page "div >> internal:or=\"span\"" ["hello", "world"]
          assertTexts page "span >> internal:or=\"div\"" ["hello", "world"]
          assertCount page "article >> internal:or=\"something\"" 0
          assertEqual "or div" (Just "hello")
            =<< textContent (locator page (selectorFromText "article >> internal:or=\"div\""))
          assertEqual "or span" (Just "world")
            =<< textContent (locator page (selectorFromText "article >> internal:or=\"span\""))
          assertEqual "div or article" (Just "hello")
            =<< textContent (locator page (selectorFromText "div >> internal:or=\"article\""))
          assertEqual "span or article" (Just "world")
            =<< textContent (locator page (selectorFromText "span >> internal:or=\"article\""))
    , testCase "should work with internal:chain=" $
        withChromiumPage $ \page -> do
          setContent page "<div>one <span>two</span> <button>three</button> </div><span>four</span><button>five</button>"
          assertTexts page "div >> internal:chain=\"button\"" ["three"]
          assertTexts page "div >> internal:chain=\"span >> internal:or=\\\"button\\\"\"" ["two", "three"]
    , localOption (mkTimeout 30000000) $
        testCase "chaining should work with large DOM" $
          withChromiumPage $ \page -> do
            _ <- evaluate page largeDomScript
            let
              selectors :: [Text]
              selectors =
                [ "div >> div >> div >> div >> div >> div >> div >> div >> span"
                , "div div div div div div div div span"
                , "div div >> div div >> div div >> div div >> span"
                ]
            forM_ selectors $ \sel ->
              assertCount page sel 1
    ]

assertText :: Page -> Text -> Text -> Playwright ()
assertText page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorText page (selectorFromText sel) "e => e.textContent"

assertHtml :: Page -> Text -> Text -> Playwright ()
assertHtml page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorText page (selectorFromText sel) "e => e.outerHTML"

assertId :: Page -> Text -> Text -> Playwright ()
assertId page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorText page (selectorFromText sel) "e => e.id"

assertIds :: Page -> Text -> Text -> Playwright ()
assertIds page sel expected = do
  ids <- evalOnSelectorAllTexts page (selectorFromText sel) "els => els.map(e => e.id)"
  assertEqual (unpack sel) expected (intercalate "," ids)

assertCount :: Page -> Text -> Double -> Playwright ()
assertCount page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorAllNumber page (selectorFromText sel) "els => els.length"

assertTexts :: Page -> Text -> [Text] -> Playwright ()
assertTexts page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorAllTexts page (selectorFromText sel) "els => els.map(e => e.textContent)"

assertMissing :: Page -> Text -> Playwright ()
assertMissing page sel =
  assertEqual (unpack sel) True . isNothing =<< querySelector page (selectorFromText sel)

appendTarget3 :: Text
appendTarget3 =
  "section => { const div = document.createElement('div'); div.setAttribute('id', 'target3'); section.appendChild(div); }"

largeDomScript :: Text
largeDomScript =
  "() => { let last = document.body; for (let i = 0; i < 100; i++) { const e = document.createElement('div'); last.appendChild(e); last = e; } const target = document.createElement('span'); target.textContent = 'Found me!'; last.appendChild(target); }"

boxesArgument :: SerializedArgument
boxesArgument =
  SerializedArgument
    { serializedArgumentValue = SerializedNode (PayloadArray (fmap rowValue layoutBoxes)) Nothing
    , serializedArgumentHandles = []
    }

rowValue :: [Double] -> SerializedValue
rowValue row =
  SerializedNode (PayloadArray (fmap (\number -> SerializedNode (PayloadNumber number) Nothing) row)) Nothing

layoutBoxes :: [[Double]]
layoutBoxes =
  [ [0, 0, 150, 150]
  , [100, 200, 50, 50]
  , [200, 200, 50, 50]
  , [100, 150, 50, 50]
  , [201, 150, 50, 50]
  , [200, 100, 50, 50]
  , [50, 50, 50, 50]
  , [150, 50, 50, 50]
  , [150, -51, 50, 50]
  , [201, -101, 50, 50]
  ]

layoutScript :: Text
layoutScript =
  "(container, boxes) => { for (let i = 0; i < boxes.length; i++) { const div = document.createElement('div'); div.style.position = 'absolute'; div.style.overflow = 'hidden'; div.style.boxSizing = 'border-box'; div.style.border = '1px solid black'; div.id = 'id' + i; div.textContent = 'id' + i; const box = boxes[i]; div.style.left = box[0] + 'px'; div.style.top = (250 - box[1] - box[3]) + 'px'; div.style.width = box[2] + 'px'; div.style.height = box[3] + 'px'; container.appendChild(div); const span = document.createElement('span'); span.textContent = '' + i; div.appendChild(span); } }"
