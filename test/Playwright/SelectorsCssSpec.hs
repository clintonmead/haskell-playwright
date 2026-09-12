module Playwright.SelectorsCssSpec
  ( tests
  ) where

import Control.Monad
  ( forM_
  )
import Data.Maybe
  ( isJust
  , isNothing
  )
import Data.Text
  ( Text
  , unpack
  )
import Playwright
  ( ElementHandle
  , Page
  , asElement
  , childLocator
  , evalOnSelectorAllHandleNumber
  , evalOnSelectorAllNumber
  , evalOnSelectorAllTexts
  , evalOnSelectorBool
  , evalOnSelectorHandleText
  , evalOnSelectorText
  , evaluate
  , evaluateHandleOnElement
  , evaluateNumberWith
  , locator
  , locatorCount
  , querySelector
  , querySelectorAll
  , querySelectorHandle
  , selectorFromText
  , setContent
  , textArgument
  , textContent
  )
import Playwright.Harness
  ( requireHandle
  , withAssetPage
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , localOption
  , mkTimeout
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "selectors-css"
    [ localOption (mkTimeout 30000000) $
        testCase "should work with large DOM" $
          withChromiumPage $ \page -> do
            _ <- evaluate page largeDomScript
            forM_ largeDomSelectors $ \sel -> do
              pw <- evalOnSelectorAllNumber page (selectorFromText sel) "els => els.length"
              qs <-
                evaluateNumberWith
                  page
                  "selector => document.querySelectorAll(selector).length"
                  (textArgument sel)
              assertEqual (unpack sel) pw qs
    , testCase "should work for open shadow roots" $
        withAssetPage "deep-shadow.html" $ \page -> do
          assertText page "css=span" "Hello from root1"
          assertText page "css=[attr=\"value\\ space\"]" "Hello from root3 #2"
          assertText page "css=[attr='value\\ \\space']" "Hello from root3 #2"
          assertText page "css=div div span" "Hello from root2"
          assertText page "css=div span + span" "Hello from root3 #2"
          assertText page "css=span + [attr*=\"value\"]" "Hello from root3 #2"
          assertText page "css=[data-testid=\"foo\"] + [attr*=\"value\"]" "Hello from root3 #2"
          assertText page "css=#target" "Hello from root2"
          assertText page "css=div #target" "Hello from root2"
          assertText page "css=div div #target" "Hello from root2"
          missing <- querySelector page (selectorFromText "css=div div div #target")
          assertEqual "deep missing" True (isNothing missing)
          assertText page "css=section > div div span" "Hello from root2"
          assertText page "css=section > div div span:nth-child(2)" "Hello from root3 #2"
          missingSection <- querySelector page (selectorFromText "css=section div div div div")
          assertEqual "section missing" True (isNothing missingSection)
          root2 <- requireHandle =<< querySelector page (selectorFromText "css=div div")
          assertEqual "root2 target" "Hello from root2"
            =<< evalOnSelectorHandleText root2 (selectorFromText "css=#target") "e => e.textContent"
          light <- querySelectorHandle root2 (selectorFromText "css:light=#target")
          assertEqual "light missing" True (isNothing light)
          shadowHandle <- evaluateHandleOnElement root2 "r => r.shadowRoot"
          shadowRoot <- requireHandle =<< asElement shadowHandle
          assertEqual "shadow target" "Hello from root2"
            =<< evalOnSelectorHandleText shadowRoot (selectorFromText "css:light=#target") "e => e.textContent"
          divs <- querySelectorAll page (selectorFromText "css=div div")
          root3 <- requireNth 1 divs
          assertEqual "root3 text" "Hello from root3"
            =<< evalOnSelectorHandleText root3 (selectorFromText "text=root3") "e => e.textContent"
          assertEqual "root3 attr" "Hello from root3 #2"
            =<< evalOnSelectorHandleText root3 (selectorFromText "css=[attr*=\"value\"]") "e => e.textContent"
          lightAttr <- querySelectorHandle root3 (selectorFromText "css:light=[attr*=\"value\"]")
          assertEqual "root3 light" True (isNothing lightAttr)
    , testCase "should work with > combinator and spaces" $
        withChromiumPage $ \page -> do
          setContent page "<div foo=\"bar\" bar=\"baz\"><span></span></div>"
          forM_
            [ "div[foo=\"bar\"] > span"
            , "div[foo=\"bar\"]> span"
            , "div[foo=\"bar\"] >span"
            , "div[foo=\"bar\"]>span"
            , "div[foo=\"bar\"]   >    span"
            , "div[foo=\"bar\"]>    span"
            , "div[foo=\"bar\"]     >span"
            , "div[foo=\"bar\"][bar=\"baz\"] > span"
            , "div[foo=\"bar\"][bar=\"baz\"]> span"
            , "div[foo=\"bar\"][bar=\"baz\"] >span"
            , "div[foo=\"bar\"][bar=\"baz\"]>span"
            , "div[foo=\"bar\"][bar=\"baz\"]   >    span"
            , "div[foo=\"bar\"][bar=\"baz\"]>    span"
            , "div[foo=\"bar\"][bar=\"baz\"]     >span"
            ]
            $ \sel ->
              assertEqual (unpack sel) "<span></span>"
                =<< evalOnSelectorText page (selectorFromText sel) "e => e.outerHTML"
    , testCase "should work with comma separated list" $
        withAssetPage "deep-shadow.html" $ \page -> do
          assertCount page "css=span,section #root1" 5
          assertCount page "css=section #root1, div span" 5
          assertEqual "id" "root1" =<< evalOnSelectorText page (selectorFromText "css=doesnotexist , section #root1") "e => e.id"
          assertCount page "css=doesnotexist ,section #root1" 1
          assertCount page "css=span,div span" 4
          assertCount page "css=span,div span,div div span" 4
          assertCount page "css=#target,[attr=\"value\\ space\"]" 2
          assertCount page "css=#target,[data-testid=\"foo\"],[attr=\"value\\ space\"]" 4
          assertCount page "css=#target,[data-testid=\"foo\"],[attr=\"value\\ space\"],span" 4
    , testCase "should keep dom order with comma separated list" $
        withChromiumPage $ \page -> do
          setContent page "<section><span><div><x></x><y></y></div></span></section>"
          assertNames page "css=span,div" ["SPAN", "DIV"]
          assertNames page "css=div,span" ["SPAN", "DIV"]
          assertNames page "css=span div, div" ["DIV"]
          assertNames page "*css=section >> css=div,span" ["SECTION"]
          assertNames page "css=section >> *css=div >> css=x,y" ["DIV"]
          assertNames page "css=section >> *css=div,span >> css=x,y" ["SPAN", "DIV"]
          assertNames page "css=section >> *css=div,span >> css=y" ["SPAN", "DIV"]
    , testCase "should return multiple captures for the same node" $
        withChromiumPage $ \page -> do
          setContent page "<div><div><div><span></span></div></div></div>"
          assertNames page "*css=div >> span" ["DIV", "DIV", "DIV"]
    , testCase "should return multiple captures when going up the hierarchy" $
        withChromiumPage $ \page -> do
          setContent page "<section>Hello<ul><li></li><li></li></ul></section>"
          assertNames page "*css=li >> ../.. >> text=Hello" ["LI", "LI"]
    , testCase "should work with comma separated list in various positions" $
        withChromiumPage $ \page -> do
          setContent page "<section><span><div><x></x><y></y></div></span></section>"
          assertNames page "css=span,div >> css=x,y" ["X", "Y"]
          assertNames page "css=span,div >> css=x" ["X"]
          assertNames page "css=div >> css=x,y" ["X", "Y"]
          assertNames page "css=div >> css=x" ["X"]
          assertNames page "css=section >> css=div >> css=x" ["X"]
          assertNames page "css=section >> css=span >> css=div >> css=y" ["Y"]
          assertNames page "css=section >> css=div >> css=x,y" ["X", "Y"]
          assertNames page "css=section >> css=div,span >> css=x,y" ["X", "Y"]
          assertNames page "css=section >> css=span >> css=x,y" ["X", "Y"]
    , testCase "should work with comma inside text" $
        withChromiumPage $ \page -> do
          setContent page "<span></span><div attr=\"hello,world!\"></div>"
          assertEqual "quoted" "<div attr=\"hello,world!\"></div>"
            =<< evalOnSelectorText page (selectorFromText "css=div[attr=\"hello,world!\"]") "e => e.outerHTML"
          assertEqual "bare" "<div attr=\"hello,world!\"></div>"
            =<< evalOnSelectorText page (selectorFromText "css=[attr=\"hello,world!\"]") "e => e.outerHTML"
          assertEqual "single" "<div attr=\"hello,world!\"></div>"
            =<< evalOnSelectorText page (selectorFromText "css=div[attr='hello,world!']") "e => e.outerHTML"
          assertEqual "bare single" "<div attr=\"hello,world!\"></div>"
            =<< evalOnSelectorText page (selectorFromText "css=[attr='hello,world!']") "e => e.outerHTML"
          assertEqual "comma list" "<span></span>"
            =<< evalOnSelectorText page (selectorFromText "css=div[attr=\"hello,world!\"],span") "e => e.outerHTML"
    , testCase "should work with attribute selectors" $
        withChromiumPage $ \page -> do
          setContent page "<div attr=\"hello world\" attr2=\"hello-''>>foo=bar[]\" attr3=\"] span\"><span></span></div>"
          _ <- evaluate page "() => { window.div = document.querySelector('div'); }"
          forM_
            [ "[attr=\"hello world\"]"
            , "[attr = \"hello world\"]"
            , "[attr ~= world]"
            , "[attr ^=hello ]"
            , "[attr $= world ]"
            , "[attr *= \"llo wor\" ]"
            , "[attr2 |= hello]"
            , "[attr = \"Hello World\" i ]"
            , "[attr *= \"llo WOR\"i]"
            , "[attr $= woRLD i]"
            , "[attr2 = \"hello-''>>foo=bar[]\"]"
            , "[attr2 $=\"foo=bar[]\"]"
            ]
            $ \sel ->
              assertEqual (unpack sel) True
                =<< evalOnSelectorBool page (selectorFromText sel) "e => e === window.div"
          assertEqual "child" True
            =<< evalOnSelectorBool page (selectorFromText "[attr*=hello] span") "e => e.parentNode === window.div"
          assertEqual "pierce" True
            =<< evalOnSelectorBool page (selectorFromText "[attr*=hello] >> span") "e => e.parentNode === window.div"
          assertEqual "attr3" True
            =<< evalOnSelectorBool page (selectorFromText "[attr3=\"] span\"] >> span") "e => e.parentNode === window.div"
    , testCase "should not match root after >>" $
        withChromiumPage $ \page -> do
          setContent page "<section><div>test</div></section>"
          found <- querySelector page (selectorFromText "css=section >> css=section")
          assertEqual "missing" True (isNothing found)
    , testCase "should work with numerical id" $
        withChromiumPage $ \page -> do
          setContent page "<section id=\"123\"></section>"
          found <- querySelector page (selectorFromText "#\\31\\32\\33")
          assertEqual "found" True (isJust found)
    , testCase "should work with wrong-case id" $
        withChromiumPage $ \page -> do
          setContent page "<section id=\"Hello\"></section>"
          assertEqual "Hello" "SECTION" =<< evalOnSelectorText page (selectorFromText "#Hello") "e => e.tagName"
          assertEqual "hello" "SECTION" =<< evalOnSelectorText page (selectorFromText "#hello") "e => e.tagName"
          assertEqual "HELLO" "SECTION" =<< evalOnSelectorText page (selectorFromText "#HELLO") "e => e.tagName"
          assertEqual "helLO" "SECTION" =<< evalOnSelectorText page (selectorFromText "#helLO") "e => e.tagName"
    , testCase "should work with *" $
        withChromiumPage $ \page -> do
          setContent page "<div id=div1></div><div id=div2><span><span></span></span></div>"
          assertCount page "*" 7
          assertCount page "*#div1" 1
          assertCount page "*:not(#div1)" 6
          assertCount page "*:not(div)" 5
          assertCount page "*:not(span)" 5
          assertCount page "*:not(*)" 0
          assertCount page "*:is(*)" 7
          assertCount page "* *" 6
          assertCount page "* *:not(span)" 4
          assertCount page "div > *" 1
          assertCount page "div *" 2
          assertCount page "* > *" 6
          body <- requireHandle =<< querySelector page (selectorFromText "body")
          assertHandleCount body "*" 4
          assertHandleCount body "*#div1" 1
          assertHandleCount body "*:not(#div1)" 3
          assertHandleCount body "*:not(div)" 2
          assertHandleCount body "*:not(span)" 2
          assertHandleCount body "*:not(*)" 0
          assertHandleCount body "*:is(*)" 4
          assertHandleCount body "div > *" 1
          assertHandleCount body "div *" 2
          assertHandleCount body "* > *" 2
          assertHandleCount body ":scope * > *" 2
          assertHandleCount body "* *" 2
          assertHandleCount body "* *:not(span)" 0
    , testCase "should work with :nth-child" $
        withAssetPage "deep-shadow.html" $ \page -> do
          assertCount page "css=span:nth-child(odd)" 3
          assertCount page "css=span:nth-child(even)" 1
          assertCount page "css=span:nth-child(n+1)" 4
          assertCount page "css=span:nth-child(n+2)" 1
          assertCount page "css=span:nth-child(2n)" 1
          assertCount page "css=span:nth-child(2n+1)" 3
          assertCount page "css=span:nth-child(-n)" 0
          assertCount page "css=span:nth-child(-n+1)" 3
          assertCount page "css=span:nth-child(-n+2)" 4
          assertCount page "css=span:nth-child(23n+2)" 1
    , testCase "should work with :nth-child(of) notation with nested functions" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div><span>span1</span><span class=foo>span2<dd></dd></span><span class=foo>span3<dd class=marker></dd></span><span class=foo>span4<dd class=marker></dd></span><span class=foo>span5<dd></dd></span><span>span6</span></div>"
          assertContents page "css=span:nth-child(1)" ["span1"]
          assertContents page "css=span:nth-child(1 of .foo)" ["span2"]
          assertContents page "css=span:nth-child(1 of .foo:has(dd.marker))" ["span3"]
          assertContents page "css=span:nth-last-child(1 of .foo:has(dd.marker))" ["span4"]
          assertContents page "css=span:nth-last-child(1 of .foo)" ["span5"]
          assertContents page "css=span:nth-last-child(  1  )" ["span6"]
          assertContents page "css=span:nth-child(1 of .foo:nth-child(3))" ["span3"]
          assertContents page "css=span:nth-child(1 of .foo:nth-child(6))" []
    , testCase "should work with :not" $
        withAssetPage "deep-shadow.html" $ \page -> do
          assertCount page "css=div:not(#root1)" 2
          assertCount page "css=body :not(span)" 4
          assertCount page "css=div > :not(span):not(div)" 0
    , testCase "should work with ~" $
        withChromiumPage $ \page -> do
          setContent page "<div id=div1></div><div id=div2></div><div id=div3></div><div id=div4></div><div id=div5></div><div id=div6></div>"
          assertIds page "#div3 >> :scope ~ div" ["div4", "div5", "div6"]
          assertIds page "#div3 >> :scope ~ *" ["div4", "div5", "div6"]
          assertIds page "#div3 >> ~ div" ["div4", "div5", "div6"]
          assertIds page "#div3 >> ~ *" ["div4", "div5", "div6"]
          assertIds page "#div3 >> #div1 ~ :scope" ["div3"]
          assertIds page "#div3 >> #div4 ~ :scope" []
          assertCount page "css=#div1 ~ div ~ #div6" 1
          assertCount page "css=#div1 ~ div ~ div" 4
          assertCount page "css=#div3 ~ div ~ div" 2
          assertCount page "css=#div4 ~ div ~ div" 1
          assertCount page "css=#div5 ~ div ~ div" 0
          assertCount page "css=#div3 ~ #div2 ~ #div6" 0
          assertCount page "css=#div3 ~ #div4 ~ #div5" 1
    , testCase "should work with +" $
        withChromiumPage $ \page -> do
          setContent page "<section><div id=div1></div><div id=div2></div><div id=div3></div><div id=div4></div><div id=div5></div><div id=div6></div></section>"
          assertIds page "#div1 >> :scope+div" ["div2"]
          assertIds page "#div1 >> :scope+*" ["div2"]
          assertIds page "#div1 >> + div" ["div2"]
          assertIds page "#div1 >> + *" ["div2"]
          assertIds page "#div3 >> div + :scope" ["div3"]
          assertIds page "#div3 >> #div1 + :scope" []
          assertCount page "css=#div1 ~ div + #div6" 1
          assertCount page "css=#div1 ~ div + div" 4
          assertCount page "css=#div3 + div + div" 1
          assertCount page "css=#div4 ~ #div5 + div" 1
          assertCount page "css=#div5 + div + div" 0
          assertCount page "css=#div3 ~ #div2 + #div6" 0
          assertCount page "css=#div3 + #div4 + #div5" 1
          assertCount page "css=div + #div1" 0
          assertCount page "css=section > div + div ~ div" 4
          assertCount page "css=section > div + #div4 ~ div" 2
          assertCount page "css=section:has(:scope > div + #div2)" 1
          assertCount page "css=section:has(:scope > div + #div1)" 0
          assertEqual "has + div5" "div4" =<< evalOnSelectorText page (selectorFromText "css=div:has(:scope + #div5)") "e => e.id"
    , testCase "should work with spaces in :nth-child and :not" $
        withAssetPage "deep-shadow.html" $ \page -> do
          assertCount page "css=span:nth-child(23n +2)" 1
          assertCount page "css=span:nth-child(23n+ 2)" 1
          assertCount page "css=span:nth-child( 23n + 2 )" 1
          assertCount page "css=span:not(#root1 #target)" 3
          assertCount page "css=span:not(:not(#root1 #target))" 1
          assertCount page "css=span:not(span:not(#root1 #target))" 1
          assertCount page "css=div > :not(span)" 2
          assertCount page "css=body :not(span, div)" 1
          assertCount page "css=span, section:not(span, div)" 5
          assertCount page "span:nth-child(23n+ 2) >> xpath=." 1
    , testCase "should work with :is" $
        withAssetPage "deep-shadow.html" $ \page -> do
          assertCount page "css=div:is(#root1)" 1
          assertCount page "css=div:is(#root1, #target)" 1
          assertCount page "css=div:is(span, #target)" 0
          assertCount page "css=div:is(span, #root1 > *)" 2
          assertCount page "css=div:is(section div)" 3
          assertCount page "css=:is(div, span)" 7
          assertCount page "css=section:is(section) div:is(section div)" 3
          assertCount page "css=:is(div, span) > *" 6
          assertCount page "css=#root1:has(:is(#root1))" 0
          assertCount page "css=#root1:has(:is(:scope, #root1))" 1
    , testCase "should work with :has" $
        withAssetPage "deep-shadow.html" $ \page -> do
          assertCount page "css=div:has(#target)" 2
          assertCount page "css=div:has([data-testid=foo])" 3
          assertCount page "css=div:has([attr*=value])" 2
          setContent page "<section><span></span><div></div></section><section><br></section>"
          assertCount page "section:has(span, div)" 1
          assertCount page "section:has(br)" 1
          assertCount page "section:has(span, br)" 2
          assertCount page "section:has(span, br, div)" 2
    , testCase "should work with :scope" $
        withAssetPage "deep-shadow.html" $ \page -> do
          assertCount page "css=div:is(:scope#root1)" 0
          assertCount page "css=div:is(:scope #root1)" 1
          assertCount page "css=div:has(:scope > #target)" 1
          spanHandle <- requireHandle =<< querySelector page (selectorFromText "css=span")
          assertCount page "css=:scope" 1
          assertHandleCount spanHandle "css=:scope" 1
          assertCount page "css=* :scope" 0
          assertHandleCount spanHandle "css=* :scope" 0
          assertCount page "css=* + :scope" 0
          assertHandleCount spanHandle "css=* + :scope" 0
          assertCount page "css=* > :scope" 0
          assertHandleCount spanHandle "css=* > :scope" 0
          assertCount page "css=* ~ :scope" 0
          assertHandleCount spanHandle "css=* ~ :scope" 0
          setContent page "<article><div class=target>hello<span></span></div></article>"
          assertText page "div >> :scope.target" "hello"
          assertText page "div >> :scope:nth-child(1)" "hello"
          assertText page "div >> :scope.target:has(span)" "hello"
          assertEqual "html" "HTML" =<< evalOnSelectorText page (selectorFromText "html:scope") "e => e.nodeName"
          setContent page "<section><span id=span1><span id=inner></span></span><span id=span2></span></section>"
          assertIds page "#span1 >> span:not(:has(:scope > div))" ["inner"]
          assertIds page "#span1 >> #inner,:scope" ["span1", "inner"]
          assertIds page "#span1 >> span,:scope" ["span1", "inner"]
          assertIds page "#span1 >> span:not(:scope)" ["inner"]
    , testCase "should work with :scope and class" $
        withChromiumPage $ \page -> do
          setContent page "<div class=\"apple\"></div><div class=\"apple selected\"></div>"
          count <-
            locatorCount
              ( childLocator
                  (locator page (selectorFromText ".apple"))
                  (selectorFromText ":scope.selected")
              )
          assertEqual "selected" 1 count
    , testCase "should absolutize relative selectors" $
        withChromiumPage $ \page -> do
          setContent page "<div><span>Hi</span></div>"
          assertText page "div >> >span" "Hi"
          assertEqual "locator"
            (Just "Hi")
            =<< textContent
              ( childLocator
                  (locator page (selectorFromText "div"))
                  (selectorFromText ">span")
              )
          assertEqual "has" "<div><span>Hi</span></div>"
            =<< evalOnSelectorText page (selectorFromText "div:has(> span)") "e => e.outerHTML"
          missing <- querySelector page (selectorFromText "div:has(> div)")
          assertEqual "no nested" True (isNothing missing)
    , testCase "css on the handle should be relative" $
        withChromiumPage $ \page -> do
          setContent page "<span class=\"find-me\" id=target1>1</span><div><span class=\"find-me\" id=target2>2</span></div>"
          assertEqual "page" "target1" =<< evalOnSelectorText page (selectorFromText ".find-me") "e => e.id"
          divHandle <- requireHandle =<< querySelector page (selectorFromText "div")
          assertEqual "handle" "target2"
            =<< evalOnSelectorHandleText divHandle (selectorFromText ".find-me") "e => e.id"
          assertEqual "combined" "target2" =<< evalOnSelectorText page (selectorFromText "div >> .find-me") "e => e.id"
    , testCase "should use light DOM structure for child combinator with slotted content" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<my-button><template shadowrootmode=\"open\"><button><slot></slot></button></template><div class=\"content\">Foo</div></my-button>"
          assertEqual "class" "content" =<< evalOnSelectorText page (selectorFromText "my-button > div") "e => e.className"
          assertEqual "text" "Foo" =<< evalOnSelectorText page (selectorFromText "my-button > .content") "e => e.textContent"
    ]

assertText :: Page -> Text -> Text -> IO ()
assertText page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorText page (selectorFromText sel) "e => e.textContent"

assertCount :: Page -> Text -> Double -> IO ()
assertCount page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorAllNumber page (selectorFromText sel) "els => els.length"

assertHandleCount :: ElementHandle -> Text -> Double -> IO ()
assertHandleCount handle sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorAllHandleNumber handle (selectorFromText sel) "els => els.length"

assertNames :: Page -> Text -> [Text] -> IO ()
assertNames page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorAllTexts page (selectorFromText sel) "els => els.map(e => e.nodeName)"

assertIds :: Page -> Text -> [Text] -> IO ()
assertIds page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorAllTexts page (selectorFromText sel) "els => els.map(e => e.id)"

assertContents :: Page -> Text -> [Text] -> IO ()
assertContents page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorAllTexts page (selectorFromText sel) "els => els.map(e => e.textContent)"

requireNth :: Int -> [ElementHandle] -> IO ElementHandle
requireNth n handles =
  case drop n handles of
    handle : _ ->
      pure handle
    [] ->
      assertFailure ("expected handle at index " <> show n)

largeDomSelectors :: [Text]
largeDomSelectors =
  [ "div div div span"
  , "div > div div > span"
  , "div + div div div span + span"
  , "div ~ div div > span ~ span"
  , "div > div > div + div > div + div > span ~ span"
  , "div div div div div div div div div div span"
  , "div > div > div > div > div > div > div > div > div > div > span"
  , "div ~ div div ~ div div ~ div div ~ div div ~ div span"
  , "span"
  ]

largeDomScript :: Text
largeDomScript =
  "() => { let id = 0; const next = (tag) => { const e = document.createElement(tag); const eid = ++id; e.textContent = 'id' + eid; e.id = '' + eid; return e; }; const generate = (depth) => { const div = next('div'); const span1 = next('span'); const span2 = next('span'); div.appendChild(span1); div.appendChild(span2); if (depth > 0) { div.appendChild(generate(depth - 1)); div.appendChild(generate(depth - 1)); } return div; }; document.body.appendChild(generate(12)); }"
