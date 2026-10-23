module Playwright.SelectorsTextSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Control.Monad
  ( forM_
  )
import Data.Maybe
  ( isJust
  , isNothing
  )
import Data.Text
  ( Text
  , intercalate
  , isInfixOf
  , unpack
  )
import Playwright
  ( ElementHandle
  , Page
  , Exactness (Exact, Inexact)
  , WaitForState (WaitForVisible)
  , childLocator
  , evalOnSelectorAllHandleNumber
  , evalOnSelectorAllNumber
  , evalOnSelectorAllTexts
  , evalOnSelectorText
  , evaluate
  , evaluateOnElement
  , evaluateOnLocatorAllTexts
  , evaluateOnLocatorText
  , first
  , filterHasText
  , filterHasTextRegex
  , getAttribute
  , getByText
  , isVisible
  , locator
  , nth
  , querySelector
  , querySelectorHandle
  , selectorFromText
  , setContent
  , textContent
  , textContentHandle
  , waitForSelectorHandle
  , Playwright
  )
import Playwright.Harness
  ( assertProtocolContains
  , requireHandle
  , withAssetPage
  , withChromiumPage
  , assertBool
  , assertEqual
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
    "selectors-text"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          setContent page "<div>yo</div><div>ya</div><div>\nye  </div>"
          assertHtml page "text=ya" "<div>ya</div>"
          assertHtml page "text=\"ya\"" "<div>ya</div>"
          assertHtml page "text=/^[ay]+$/" "<div>ya</div>"
          assertHtml page "text=/Ya/i" "<div>ya</div>"
          assertHtml page "text=ye" "<div>\nye  </div>"
          yeHtml <- evaluateOnLocatorText (getByText page "ye" Inexact) "e => e.outerHTML"
          assertBool "getByText ye" (">\nye  </div>" `isInfixOf` yeHtml)
          setContent page "<div> ye </div><div>ye</div>"
          assertHtml page "text=\"ye\"" "<div> ye </div>"
          padded <- evaluateOnLocatorText (first (getByText page "ye" Exact)) "e => e.outerHTML"
          assertBool "exact ye" ("> ye </div>" `isInfixOf` padded)
          setContent page "<div>yo</div><div>\"ya</div><div> hello world! </div>"
          assertHtml page "text=\"\\\"ya\"" "<div>\"ya</div>"
          assertHtml page "text=/hello/" "<div> hello world! </div>"
          assertHtml page "text=/^\\s*heLLo/i" "<div> hello world! </div>"
          setContent page "<div>yo<div>ya</div>hey<div>hey</div></div>"
          assertHtml page "text=hey" "<div>hey</div>"
          assertHtml page "text=yo>>text=\"ya\"" "<div>ya</div>"
          assertHtml page "text=yo>> text=\"ya\"" "<div>ya</div>"
          assertHtml page "text=yo >>text='ya'" "<div>ya</div>"
          assertHtml page "text=yo >> text='ya'" "<div>ya</div>"
          assertHtml page "'yo'>>\"ya\"" "<div>ya</div>"
          assertHtml page "\"yo\" >> 'ya'" "<div>ya</div>"
          setContent page "<div>yo<span id=\"s1\"></span></div><div>yo<span id=\"s2\"></span><span id=\"s3\"></span></div>"
          assertEqual "two yo"
            "<div>yo<span id=\"s1\"></span></div>\n<div>yo<span id=\"s2\"></span><span id=\"s3\"></span></div>"
            =<< htmlsJoined page "text=yo" "\n"
          setContent page "<div>'</div><div>\"</div><div>\\</div><div>x</div>"
          assertHtml page "text='\\''" "<div>'</div>"
          assertHtml page "text='\"'" "<div>\"</div>"
          assertHtml page "text=\"\\\"\"" "<div>\"</div>"
          assertHtml page "text=\"'\"" "<div>'</div>"
          assertHtml page "text=\"\\x\"" "<div>x</div>"
          assertHtml page "text='\\x'" "<div>x</div>"
          assertHtml page "text='\\\\'" "<div>\\</div>"
          assertHtml page "text=\"\\\\\"" "<div>\\</div>"
          assertHtml page "text=\"" "<div>\"</div>"
          assertHtml page "text='" "<div>'</div>"
          assertHtml page "\"x\"" "<div>x</div>"
          assertHtml page "'x'" "<div>x</div>"
          assertProtocolContains "parsing css selector" (querySelector page (selectorFromText "\""))
          assertProtocolContains "parsing css selector" (querySelector page (selectorFromText "'"))
          setContent page "<div> ' </div><div> \" </div>"
          assertHtml page "text=\"" "<div> \" </div>"
          assertHtml page "text='" "<div> ' </div>"
          setContent page "<div>Hi''&gt;&gt;foo=bar</div>"
          assertHtml page "text=\"Hi''>>foo=bar\"" "<div>Hi''&gt;&gt;foo=bar</div>"
          setContent page "<div>Hi'\"&gt;&gt;foo=bar</div>"
          assertHtml page "text=\"Hi'\\\">>foo=bar\"" "<div>Hi'\"&gt;&gt;foo=bar</div>"
          setContent page "<div>Hi&gt;&gt;<span></span></div>"
          assertHtml page "text=\"Hi>>\">>span" "<span></span>"
          assertHtml page "text=/Hi\\>\\>/ >> span" "<span></span>"
          setContent page "<div>a<br>b</div><div>a</div>"
          assertHtml page "text=a" "<div>a<br>b</div>"
          assertHtml page "text=b" "<div>a<br>b</div>"
          assertHtml page "text=ab" "<div>a<br>b</div>"
          missingAbc <- querySelector page (selectorFromText "text=abc")
          assertEqual "abc" True (isNothing missingAbc)
          assertCount page "text=a" 2
          assertCount page "text=b" 1
          assertCount page "text=ab" 1
          assertCount page "text=abc" 0
          setContent page "<div></div><span></span>"
          _ <- evalOnSelectorText page (selectorFromText "div") "div => { div.appendChild(document.createTextNode('hello')); div.appendChild(document.createTextNode('world')); return div.outerHTML; }"
          _ <- evalOnSelectorText page (selectorFromText "span") "span => { span.appendChild(document.createTextNode('hello')); span.appendChild(document.createTextNode('world')); return span.outerHTML; }"
          assertHtml page "text=lowo" "<div>helloworld</div>"
          assertEqual "lowo both"
            "<div>helloworld</div><span>helloworld</span>"
            =<< htmlsJoined page "text=lowo" ""
          setContent page "<span>Sign&nbsp;in</span><span>Hello\n \nworld</span>"
          assertHtml page "text=Sign in" "<span>Sign&nbsp;in</span>"
          assertCount page "text=Sign \tin" 1
          assertCount page "text=\"Sign in\"" 1
          assertHtml page "text=lo wo" "<span>Hello\n \nworld</span>"
          assertHtml page "text=\"Hello world\"" "<span>Hello\n \nworld</span>"
          missingLoWo <- querySelector page (selectorFromText "text=\"lo wo\"")
          assertEqual "quoted lo wo" True (isNothing missingLoWo)
          assertCount page "text=lo \nwo" 1
          assertCount page "text=\"lo \nwo\"" 0
          setContent page "<div>let's<span>hello</span></div>"
          assertHtml page "text=/let's/i >> span" "<span>hello</span>"
          assertHtml page "text=/let\\'s/i >> span" "<span>hello</span>"
    , testCase "should work with :text" $
        withChromiumPage $ \page -> do
          setContent page "<div>yo</div><div>ya</div><div>\nHELLO   \n world  </div>"
          assertHtml page ":text(\"ya\")" "<div>ya</div>"
          assertHtml page ":text-is(\"ya\")" "<div>ya</div>"
          assertHtml page ":text(\"y\")" "<div>yo</div>"
          missingY <- querySelector page (selectorFromText ":text-is(\"Y\")")
          assertEqual "Y" True (isNothing missingY)
          assertHtml page ":text(\"hello world\")" "<div>\nHELLO   \n world  </div>"
          assertHtml page ":text-is(\"HELLO world\")" "<div>\nHELLO   \n world  </div>"
          assertHtml page ":text(\"lo wo\")" "<div>\nHELLO   \n world  </div>"
          missingLo <- querySelector page (selectorFromText ":text-is(\"lo wo\")")
          assertEqual "lo wo" True (isNothing missingLo)
          assertHtml page ":text-matches(\"^[ay]+$\")" "<div>ya</div>"
          assertHtml page ":text-matches(\"y\", \"g\")" "<div>yo</div>"
          assertHtml page ":text-matches(\"Y\", \"i\")" "<div>yo</div>"
          missingMatch <- querySelector page (selectorFromText ":text-matches(\"^y$\")")
          assertEqual "^y$" True (isNothing missingMatch)
          assertProtocolContains "\"text\" engine expects a single string" (querySelector page (selectorFromText ":text(\"foo\", \"bar\")"))
          assertProtocolContains "\"text\" engine expects a single string" (querySelector page (selectorFromText ":text(foo > bar)"))
    , testCase "should support empty string" $
        withChromiumPage $ \page -> do
          setContent page "<div></div><div>ya</div><div>\nHELLO   \n world  </div>"
          assertHtml page "div:text-is(\"\")" "<div></div>"
          assertCount page "div:text-is(\"\")" 1
          assertHtml page "div:text(\"\")" "<div></div>"
          assertCount page "div:text(\"\")" 3
          assertHtml page "div >> text=\"\"" "<div></div>"
          assertCount page "div >> text=\"\"" 1
          assertHtml page "div >> text=/^$/" "<div></div>"
          assertCount page "div >> text=/^$/" 1
          assertHtml page "div:text-matches(\"\")" "<div></div>"
          assertCount page "div:text-matches(\"\")" 3
    , testCase "should work across nodes" $
        withChromiumPage $ \page -> do
          setContent page "<div id=target1>Hello<i>,</i> <span id=target2>world</span><b>!</b></div>"
          assertId page ":text(\"Hello, world!\")" "target1"
          assertId page ":text(\"Hello\")" "target1"
          assertId page ":text(\"world\")" "target2"
          assertCount page ":text(\"world\")" 1
          missingHelloWorld <- querySelector page (selectorFromText ":text(\"hello world\")")
          assertEqual "hello world" True (isNothing missingHelloWorld)
          missingDivWorld <- querySelector page (selectorFromText "div:text(\"world\")")
          assertEqual "div world" True (isNothing missingDivWorld)
          assertId page "text=Hello, world!" "target1"
          assertId page "text=Hello" "target1"
          assertId page "text=world" "target2"
          assertCount page "text=world" 1
          missingTextHello <- querySelector page (selectorFromText "text=hello world")
          assertEqual "text hello world" True (isNothing missingTextHello)
          missingIs <- querySelector page (selectorFromText ":text-is(\"Hello, world!\")")
          assertEqual "text-is full" True (isNothing missingIs)
          assertId page ":text-is(\"Hello\")" "target1"
          assertId page ":text-is(\"world\")" "target2"
          assertCount page ":text-is(\"world\")" 1
          missingQuoted <- querySelector page (selectorFromText "text=\"Hello, world!\"")
          assertEqual "quoted full" True (isNothing missingQuoted)
          assertId page "text=\"Hello\"" "target1"
          assertId page "text=\"world\"" "target2"
          assertCount page "text=\"world\"" 1
          assertEqual ".*" "I" =<< evalOnSelectorText page (selectorFromText ":text-matches(\".*\")") "e => e.nodeName"
          assertId page ":text-matches(\"world?\")" "target2"
          assertCount page ":text-matches(\"world\")" 1
          missingDivStar <- querySelector page (selectorFromText "div:text(\".*\")")
          assertEqual "div .*" True (isNothing missingDivStar)
          assertEqual "text .*" "I" =<< evalOnSelectorText page (selectorFromText "text=/.*/") "e => e.nodeName"
          assertId page "text=/world?/" "target2"
          assertCount page "text=/world/" 1
    , testCase "text-is() should ignore comments" $
        withChromiumPage $ \page -> do
          setContent page "<div id=me>hel<!-- comment -->lo\n  <!-- comment -->\n  world</div>"
          assertId page ":text-is(\"hello world\")" "me"
          attr <- getAttribute (getByText page "hello world" Exact) "id"
          assertEqual "getByText" (Just "me") attr
          hasTextAttr <- getAttribute (filterHasText (locator page (selectorFromText "div")) "hello world") "id"
          assertEqual "hasText" (Just "me") hasTextAttr
    , testCase "should work with text nodes in quoted mode" $
        withChromiumPage $ \page -> do
          setContent page "<div id=target1>Hello<span id=target2>wo  rld  </span>  Hi again  </div>"
          assertId page "text=\"Hello\"" "target1"
          assertId page "text=\"Hi again\"" "target1"
          assertId page "text=\"wo rld\"" "target2"
          forM_
            [ "text=\"Hellowo rld Hi again\""
            , "text=\"Hellowo\""
            , "text=\"Hellowo rld\""
            , "text=\"wo rld Hi ag\""
            , "text=\"again\""
            , "text=\"hi again\""
            ]
            $ \sel -> do
              found <- querySelector page (selectorFromText sel)
              assertEqual (unpack sel) True (isNothing found)
          assertId page "text=hi again" "target1"
    , testCase "should clear caches" $
        withChromiumPage $ \page -> do
          setContent page "<div id=target1>text</div><div id=target2>text</div>"
          divHandle <- requireHandle =<< querySelector page (selectorFromText "#target1")
          setText divHandle "text"
          assertId page "text=text" "target1"
          setText divHandle "foo"
          assertId page "text=text" "target2"
          setText divHandle "text"
          assertId page ":text(\"text\")" "target1"
          setText divHandle "foo"
          assertId page ":text(\"text\")" "target2"
          setText divHandle "text"
          assertCount page "text=text" 2
          setText divHandle "foo"
          assertCount page "text=text" 1
          setText divHandle "text"
          assertCount page ":text(\"text\")" 2
          setText divHandle "foo"
          assertCount page ":text(\"text\")" 1
    , testCase "should work with :has-text" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<input id=input2>\n    <div id=div1>\n      <span>  Find me  </span>\n      or\n      <wrap><span id=span2>maybe me  </span></wrap>\n      <div><input id=input1></div>\n    </div>"
          assertEqual "html" "HTML" =<< evalOnSelectorText page (selectorFromText ":has-text(\"find me\")") "e => e.tagName"
          assertHtml page "span:has-text(\"find me\")" "<span>  Find me  </span>"
          assertId page "div:has-text(\"find me\")" "div1"
          assertId page "div:has-text(\"find me\") input" "input1"
          assertId page ":has-text(\"find me\") input" "input2"
          assertId page "div:has-text(\"find me or maybe me\")" "div1"
          missing <- querySelector page (selectorFromText "div:has-text(\"find noone\")")
          assertEqual "noone" True (isNothing missing)
          assertEqual "is" ["div1", "span2"]
            =<< evalOnSelectorAllTexts page (selectorFromText ":is(div,span):has-text(\"maybe\")") "els => els.map(e => e.id)"
          assertEqual "wrap" "WRAP"
            =<< evalOnSelectorText page (selectorFromText "div:has-text(\"find me\") :has-text(\"maybe me\")") "e => e.tagName"
          assertId page "div:has-text(\"find me\") span:has-text(\"maybe me\")" "span2"
          setContent page "<div id=me>hello\n  wo\"r>>ld</div>"
          assertId page "div:has-text(\"hello wo\\\"r>>ld\")" "me"
          assertId page "div:has-text(\"hello\\a wo\\\"r>>ld\")" "me"
          newlineAttr <-
            getAttribute (filterHasText (locator page (selectorFromText "div")) "hello\nwo\"r>>ld") "id"
          assertEqual "hasText newline" (Just "me") newlineAttr
          assertProtocolContains "\"has-text\" engine expects a single string" (querySelector page (selectorFromText ":has-text(\"foo\", \"bar\")"))
          assertProtocolContains "\"has-text\" engine expects a single string" (querySelector page (selectorFromText ":has-text(foo > bar)"))
    , localOption (mkTimeout 30000000) $
        testCase "should work with large DOM" $
          withChromiumPage $ \page -> do
            _ <- evaluate page largeDomScript
            forM_
              [ ":has-text(\"id18\")"
              , ":has-text(\"id12345\")"
              , ":has-text(\"id\")"
              , ":text(\"id18\")"
              , ":text(\"id12345\")"
              , ":text(\"id\")"
              , ":text-matches(\"id12345\", \"i\")"
              , "text=id18"
              , "text=id12345"
              , "text=id"
              , "#id18"
              , "#id12345"
              , "*"
              ]
              $ \sel -> do
                _ <- evalOnSelectorAllNumber page (selectorFromText sel) "els => els.length"
                pure ()
    , testCase "should be case sensitive if quotes are specified" $
        withChromiumPage $ \page -> do
          setContent page "<div>yo</div><div>ya</div><div>\nye  </div>"
          assertHtml page "text=yA" "<div>ya</div>"
          missingQuoted <- querySelector page (selectorFromText "text=\"yA\"")
          assertEqual "quoted yA" True (isNothing missingQuoted)
          missingSpace <- querySelector page (selectorFromText "text= \"ya\"")
          assertEqual "space quoted" True (isNothing missingSpace)
    , testCase "should search for a substring without quotes" $
        withChromiumPage $ \page -> do
          setContent page "<div>textwithsubstring</div>"
          assertHtml page "text=with" "<div>textwithsubstring</div>"
          missing <- querySelector page (selectorFromText "text=\"with\"")
          assertEqual "quoted with" True (isNothing missing)
    , testCase "should skip head, script and style" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<head><title>title</title><script>var script</script><style>.style {}</style></head><body><script>var script</script><style>.style {}</style><div>title script style</div></body>"
          headHandle <- requireHandle =<< querySelector page (selectorFromText "head")
          titleHandle <- requireHandle =<< querySelector page (selectorFromText "title")
          scriptHandle <- requireHandle =<< querySelector page (selectorFromText "body script")
          styleHandle <- requireHandle =<< querySelector page (selectorFromText "body style")
          forM_ ["title", "script", "style"] $ \text -> do
            assertEqual (unpack text) "DIV"
              =<< evalOnSelectorText page (selectorFromText ("text=" <> text)) "e => e.nodeName"
            assertEqual (unpack (text <> " all")) ["DIV"]
              =<< evalOnSelectorAllTexts page (selectorFromText ("text=" <> text)) "els => els.map(e => e.nodeName)"
            forM_ [headHandle, titleHandle, scriptHandle, styleHandle] $ \root -> do
              found <- querySelectorHandle root (selectorFromText ("text=" <> text))
              assertEqual (unpack text <> " root") True (isNothing found)
              assertEqual (unpack text <> " root count") (0 :: Double)
                =<< evalOnSelectorAllHandleNumber root (selectorFromText ("text=" <> text)) "els => els.length"
    , testCase "should match input[type=button|submit|reset]" $
        withChromiumPage $ \page -> do
          setContent page "<input type=\"submit\" value=\"hello\"><input type=\"button\" value=\"world\"><input type=\"reset\" value=\"clear\">"
          assertHtml page "text=hello" "<input type=\"submit\" value=\"hello\">"
          assertHtml page "text=world" "<input type=\"button\" value=\"world\">"
          assertHtml page "text=clear" "<input type=\"reset\" value=\"clear\">"
    , testCase "should work for open shadow roots" $
        withAssetPage "deep-shadow.html" $ \page -> do
          assertText page "text=root1" "Hello from root1"
          assertText page "text=root2" "Hello from root2"
          assertText page "text=root3" "Hello from root3"
          assertText page "#root1 >> text=from root3" "Hello from root3"
          assertText page "#target >> text=from root2" "Hello from root2"
          forM_ ["text:light=root1", "text:light=root2", "text:light=root3"] $ \sel -> do
            found <- querySelector page (selectorFromText sel)
            assertEqual (unpack sel) True (isNothing found)
    , testCase "should prioritize light dom over shadow dom in the same parent" $
        withChromiumPage $ \page -> do
          _ <-
            evaluate
              page
              "() => { const div = document.createElement('div'); document.body.appendChild(div); div.attachShadow({ mode: 'open' }); const shadowSpan = document.createElement('span'); shadowSpan.textContent = 'Hello from shadow'; div.shadowRoot.appendChild(shadowSpan); const lightSpan = document.createElement('span'); lightSpan.textContent = 'Hello from light'; div.appendChild(lightSpan); }"
          assertText page "div >> text=Hello" "Hello from light"
    , testCase "should waitForSelector with distributed elements" $
        withChromiumPage $ \page ->
          withAsync (waitForSelectorHandle page (selectorFromText "div >> text=Hello") WaitForVisible) $ \pending -> do
            _ <-
              evaluate
                page
                "() => { const div = document.createElement('div'); document.body.appendChild(div); div.attachShadow({ mode: 'open' }); const shadowSpan = document.createElement('span'); shadowSpan.textContent = 'Hello from shadow'; div.shadowRoot.appendChild(shadowSpan); div.shadowRoot.appendChild(document.createElement('slot')); const lightSpan = document.createElement('span'); lightSpan.textContent = 'Hello from light'; div.appendChild(lightSpan); }"
            handle <- requireHandle =<< wait pending
            assertEqual "light" (Just "Hello from light") =<< textContentHandle handle
    , testCase "should match root after >>" $
        withChromiumPage $ \page -> do
          setContent page "<section>test</section>"
          found <- querySelector page (selectorFromText "css=section >> text=test")
          assertEqual "css" True (isJust found)
          foundText <- querySelector page (selectorFromText "text=test >> text=test")
          assertEqual "text" True (isJust foundText)
    , testCase "should match root after >> with *" $
        withChromiumPage $ \page -> do
          setContent page "<button> hello world </button> <button> hellow <span> world </span> </button>"
          assertCount page "*css=button >> text=hello >> text=world" 2
    , testCase "should work with leading and trailing spaces" $
        withChromiumPage $ \page -> do
          setContent page "<button> Add widget </button>"
          assertEqual "plain" True =<< isVisible (locator page (selectorFromText "text=Add widget"))
          assertEqual "padded" True =<< isVisible (locator page (selectorFromText "text= Add widget "))
    , testCase "should work with unpaired quotes when not at the start" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div>hello\"world<span>yay</span></div><div>hello'world<span>nay</span></div><div>hello`world<span>oh</span></div><div>hello`world<span>oh2</span></div>"
          assertHtml page "text=lo\" >> span" "<span>yay</span>"
          assertHtml page "  text=lo\" >> span" "<span>yay</span>"
          assertHtml page "text  =lo\" >> span" "<span>yay</span>"
          assertHtml page "text=  lo\" >> span" "<span>yay</span>"
          assertHtml page " text = lo\" >> span" "<span>yay</span>"
          assertHtml page "text=o\"wor >> span" "<span>yay</span>"
          assertHtml page "text=lo'wor >> span" "<span>nay</span>"
          assertHtml page "text=o' >> span" "<span>nay</span>"
          assertHtml page "text=ello`wor >> span" "<span>oh</span>"
          assertEqual "first"
            (Just "oh")
            =<< textContent
              ( first
                  ( childLocator
                      (locator page (selectorFromText "text=ello`wor"))
                      (selectorFromText "span")
                  )
              )
          assertEqual "second"
            (Just "oh2")
            =<< textContent
              ( nth
                  ( childLocator
                      (locator page (selectorFromText "text=ello`wor"))
                      (selectorFromText "span")
                  )
                  1
              )
          forM_ ["text='wor >> span", "text=\" >> span", "text=` >> span"] $ \sel -> do
            found <- querySelector page (selectorFromText sel)
            assertEqual (unpack sel) True (isNothing found)
    , testCase "should work with paired quotes in the middle of selector" $
        withChromiumPage $ \page -> do
          setContent page "<div>pattern \"^-?\\d+$\"</div>"
          assertEqual "unclosed" True
            =<< isVisible (locator page (selectorFromText "div >> text=pattern \"^-?\\d+$"))
          assertEqual "closed" True
            =<< isVisible (locator page (selectorFromText "div >> text=pattern \"^-?\\d+$\""))
          assertEqual "quoted" True
            =<< isVisible (locator page (selectorFromText "div >> text='pattern \"^-?\\\\d+$\"'"))
    , testCase "hasText and internal:text should match full node text in strict mode" $
        withChromiumPage $ \page -> do
          setContent page "<div id=div1>hello<span>world</span></div><div id=div2>hello</div>"
          assertEqual "helloworld" (Just "div1") =<< getAttribute (getByText page "helloworld" Exact) "id"
          assertEqual "hello" (Just "div2") =<< getAttribute (getByText page "hello" Exact) "id"
          assertEqual "regex helloworld" (Just "div1")
            =<< getAttribute (filterHasTextRegex (locator page (selectorFromText "div")) "/^helloworld$/") "id"
          assertEqual "regex hello" (Just "div2")
            =<< getAttribute (filterHasTextRegex (locator page (selectorFromText "div")) "/^hello$/") "id"
          setContent page "<div id=div1><span id=span1>hello</span>world</div><div id=div2><span id=span2>hello</span></div>"
          assertEqual "helloworld2" (Just "div1") =<< getAttribute (getByText page "helloworld" Exact) "id"
          ids <- evaluateOnLocatorAllTexts (getByText page "hello" Exact) "els => els.map(e => e.id)"
          assertEqual "spans" ["span1", "span2"] ids
          assertEqual "regex helloworld2" (Just "div1")
            =<< getAttribute (filterHasTextRegex (locator page (selectorFromText "div")) "/^helloworld$/") "id"
          assertEqual "regex hello2" (Just "div2")
            =<< getAttribute (filterHasTextRegex (locator page (selectorFromText "div")) "/^hello$/") "id"
    ]

assertHtml :: Page -> Text -> Text -> Playwright ()
assertHtml page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorText page (selectorFromText sel) "e => e.outerHTML"

assertText :: Page -> Text -> Text -> Playwright ()
assertText page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorText page (selectorFromText sel) "e => e.textContent"

assertId :: Page -> Text -> Text -> Playwright ()
assertId page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorText page (selectorFromText sel) "e => e.id"

assertCount :: Page -> Text -> Double -> Playwright ()
assertCount page sel expected =
  assertEqual (unpack sel) expected
    =<< evalOnSelectorAllNumber page (selectorFromText sel) "els => els.length"

htmlsJoined :: Page -> Text -> Text -> Playwright Text
htmlsJoined page sel sep = do
  parts <- evalOnSelectorAllTexts page (selectorFromText sel) "els => els.map(e => e.outerHTML)"
  pure (intercalate sep parts)

setText :: ElementHandle -> Text -> Playwright ()
setText handle value = do
  _ <- evaluateOnElement handle ("div => { div.textContent = '" <> value <> "'; }")
  pure ()

largeDomScript :: Text
largeDomScript =
  "() => { let id = 0; const next = (tag) => { const e = document.createElement(tag); const eid = ++id; e.textContent = 'id' + eid; e.id = 'id' + eid; return e; }; const generate = (depth) => { const div = next('div'); const span1 = next('span'); const span2 = next('span'); div.appendChild(span1); div.appendChild(span2); if (depth > 0) { div.appendChild(generate(depth - 1)); div.appendChild(generate(depth - 1)); } return div; }; document.body.appendChild(generate(12)); }"
