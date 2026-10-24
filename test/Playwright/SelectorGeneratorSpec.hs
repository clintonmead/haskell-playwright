{-# LANGUAGE OverloadedStrings #-}

module Playwright.SelectorGeneratorSpec
  ( tests
  ) where

import Data.Aeson
  ( Value
  , object
  , (.=)
  )
import Data.Text
  ( Text
  , isInfixOf
  , replace
  )
import Playwright
  ( Frame
  , Locator
  , Page
  , Playwright
  , childFrames
  , evaluateOnFrameText
  , evaluateOnLocatorText
  , evaluateText
  , hover
  , locator
  , mainFrame
  , newContext
  , waitForFunctionOnFrame
  , newPage
  , selectorFromText
  , setContent
  )
import Playwright.BrowserContext
  ( BrowserContext
  , browserContextChannel
  )
import Playwright.Channel
  ( send
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , assertProtocolContainsAll
  , withChromiumBrowser
  , withChromiumContext
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

-- | Encodes a value as a double-quoted JavaScript string literal.
jsString :: Text -> Text
jsString value =
  "\"" <> replace "\"" "\\\"" (replace "\\" "\\\\" value) <> "\""

-- | The element-to-selector function used by the upstream `generate` helper.
-- When an expected target is given, the generated selector must resolve to it.
generatorFn :: Maybe Text -> Text
generatorFn expected =
  "(e) => { const playwright = window.playwright; const selector = playwright.selector(e); const expectedTarget = "
    <> maybe "e" (\x -> "playwright.$(" <> jsString x <> ")") expected
    <> "; if (playwright.$(selector) === expectedTarget) return selector; return 'FAILED: ' + selector; }"

noTextFn :: Text
noTextFn =
  "(e) => window.__injectedScript.generateSelector(e, { noText: true, testIdAttributeName: 'data-testid' }).selector"

-- | Upstream `$eval` takes the first match, so every target is narrowed to its first match.
firstOf :: Page -> Text -> Locator
firstOf page target =
  locator page (selectorFromText (target <> " >> nth=0"))

generateFor :: Page -> Text -> Maybe Text -> Playwright Text
generateFor page target expected =
  evaluateOnLocatorText (firstOf page target) (generatorFn expected)

generateNoText :: Page -> Text -> Playwright Text
generateNoText page target =
  evaluateOnLocatorText (firstOf page target) noTextFn

generateInFrame :: Frame -> Text -> Playwright Text
generateInFrame frame target =
  evaluateOnFrameText frame $
    "() => { const e = document.querySelector("
      <> jsString target
      <> "); return ("
      <> generatorFn Nothing
      <> ")(e); }"

-- | Runs a mutation on the first match of a target, like upstream `page.$eval`.
mutate :: Page -> Text -> Text -> Playwright ()
mutate page target fn =
  () <$ evaluateOnLocatorText (firstOf page target) fn

expectSelector :: Page -> Text -> Text -> Playwright ()
expectSelector page target expected =
  assertEqual "selector" expected =<< generateFor page target Nothing

expectSelectorFor :: Page -> Text -> Text -> Text -> Playwright ()
expectSelectorFor page target expectedTarget expected =
  assertEqual "selector" expected =<< generateFor page target (Just expectedTarget)

expectNoText :: Page -> Text -> Text -> Playwright ()
expectNoText page target expected =
  assertEqual "selector" expected =<< generateNoText page target

-- | Upstream `context._showRecorder({ language: 'javascript' })`. Enables the
-- main-world `window.playwright` helpers used by the generate functions.
showRecorder :: BrowserContext -> Playwright ()
showRecorder ctx = do
  _ <- send (browserContextChannel ctx) (MethodName "showRecorder") (object ["language" .= ("javascript" :: Text)]) :: Playwright Value
  pure ()

-- | Upstream `playwright.selectors.setTestIdAttribute`, applied per context.
setContextTestIdAttributeName :: BrowserContext -> Text -> Playwright ()
setContextTestIdAttributeName ctx name = do
  _ <- send (browserContextChannel ctx) (MethodName "setTestIdAttributeName") (object ["testIdAttributeName" .= name]) :: Playwright Value
  pure ()

withRecorderPage :: (Page -> Playwright a) -> IO a
withRecorderPage action =
  withChromiumContext $ \ctx page -> do
    showRecorder ctx
    action page

-- | True when a selector pairs a case-insensitive name with a case-sensitive description, or the reverse.
mixedExactness :: Text -> Bool
mixedExactness selector =
  ("\"i][description=" `isInfixOf` selector && "\"s]" `isInfixOf` selector)
    || ("\"s][description=" `isInfixOf` selector && "\"i]" `isInfixOf` selector)

strictContent :: Text
strictContent =
  "<div>\n  <div></div>\n  <div>\n    <div></div>\n    <div></div>\n  </div>\n</div>\n<div>\n  <div class='foo bar:0' data-custom-id='One'>\n  </div>\n  <div class='foo bar:1' data-custom-id='Two'>\n  </div>\n</div>"

checkStrictErrors :: Page -> Playwright ()
checkStrictErrors page = do
  setContent page strictContent
  assertProtocolContainsAll
    [ "strict mode violation"
    , "<div class=\"foo bar:0"
    , "<div class=\"foo bar:1"
    , "aka getByTestId('One')"
    , "aka getByTestId('Two')"
    ]
    (hover (locator page (selectorFromText ".foo")))

tests :: TestTree
tests =
  testGroup
    "selector-generator"
    [ testCase "should prefer button over inner span" $
        withRecorderPage $ \page -> do
          setContent page "<button><span>text</span></button>"
          expectSelectorFor page "span" "button" "internal:role=button[name=\"text\"i]"
    , testCase "should prefer role=button over inner span" $
        withRecorderPage $ \page -> do
          setContent page "<div role=button><span>text</span></div>"
          expectSelectorFor page "span" "div" "internal:role=button[name=\"text\"i]"
    , testCase "should not prefer zero-sized button over inner span" $
        withRecorderPage $ \page -> do
          setContent page "\n      <button style=\"width:0;height:0;padding:0;border:0;overflow:visible;\">\n        <span style=\"width:100px;height:100px;\">text</span>\n      </button>\n    "
          expectSelector page "span" "internal:text=\"text\"i"
    , testCase "should generate text and normalize whitespace" $
        withRecorderPage $ \page -> do
          setContent page "<div>Text  some\n\n\n more \t text   </div>"
          expectSelector page "div" "internal:text=\"Text some more text\"i"
    , testCase "should not escape spaces inside named attr selectors" $
        withRecorderPage $ \page -> do
          setContent page "<input placeholder=\"Foo b ar\"/>"
          expectSelector page "input" "internal:role=textbox[name=\"Foo b ar\"i]"
    , testCase "should generate text for <input type=button>" $
        withRecorderPage $ \page -> do
          setContent page "<input type=button value=\"Click me\">"
          expectSelector page "input" "internal:role=button[name=\"Click me\"i]"
    , testCase "should trim text" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div>Text0123456789Text0123456789Text0123456789Text0123456789Text0123456789Text0123456789Text0123456789Text0123456789Text0123456789Text0123456789</div>\n      <div>Text0123456789Text0123456789Text0123456789Text0123456789Text0123456789!Text0123456789Text0123456789Text0123456789Text0123456789Text0123456789</div>\n    "
          expectSelector page "div" "internal:text=\"Text0123456789Text0123456789Text0123456789Text0123456789Text0123456789Text012345\"i"
    , testCase "should try to improve role name" $
        withRecorderPage $ \page -> do
          setContent page "<div role=button>Issues 23</div>"
          expectSelector page "div" "internal:role=button[name=\"Issues\"i]"
    , testCase "should use description when name is not unique" $
        withRecorderPage $ \page -> do
          setContent page "\n      <button aria-description=\"Upload report\">Submit</button>\n      <button aria-description=\"Upload photo\">Submit</button>\n    "
          expectSelector page "button[aria-description=\"Upload report\"]" "internal:role=button[name=\"Submit\"i][description=\"Upload report\"i]"
          expectSelector page "button[aria-description=\"Upload photo\"]" "internal:role=button[name=\"Submit\"i][description=\"Upload photo\"i]"
    , testCase "should not use description when name is unique" $
        withRecorderPage $ \page -> do
          setContent page "\n      <button aria-description=\"Some description\">Submit</button>\n      <button aria-description=\"Other description\">Cancel</button>\n    "
          expectSelector page "button[aria-description=\"Some description\"]" "internal:role=button[name=\"Submit\"i]"
    , testCase "should use description from aria-describedby" $
        withRecorderPage $ \page -> do
          setContent page "\n      <span id=\"desc1\">Save form data</span>\n      <span id=\"desc2\">Save as draft</span>\n      <button aria-describedby=\"desc1\">Submit</button>\n      <button aria-describedby=\"desc2\">Submit</button>\n    "
          expectSelector page "button[aria-describedby=\"desc1\"]" "internal:role=button[name=\"Submit\"i][description=\"Save form data\"i]"
          expectSelector page "button[aria-describedby=\"desc2\"]" "internal:role=button[name=\"Submit\"i][description=\"Save as draft\"i]"
    , testCase "should fall back to nth when name and description are both not unique" $
        withRecorderPage $ \page -> do
          setContent page "\n      <button aria-description=\"Same desc\">Submit</button>\n      <button aria-description=\"Same desc\">Submit</button>\n    "
          expectSelector page "button:first-of-type" "internal:role=button[name=\"Submit\"i] >> nth=0"
    , testCase "should use consistent exactness for name and description" $
        withRecorderPage $ \page -> do
          setContent page "\n      <table>\n        <tr title=\"table row 1\">\n          <td>foo</td><td>bar</td><td>baz</td><td>foo</td><td>bar</td><td>baz</td><td>foo</td><td>bar</td><td>baz</td>\n        </tr>\n        <tr title=\"table row 2\">\n          <td>foo</td><td>bar</td><td>baz</td><td>foo</td><td>bar</td><td>baz</td><td>foo</td><td>bar</td><td>baz</td>\n        </tr>\n      </table>\n    "
          selector <- generateFor page "tr[title=\"table row 1\"]" Nothing
          assertBool "name and description share one exactness flag" (not (mixedExactness selector))
    , testCase "should use description when role has no name" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div role=\"alert\" aria-description=\"Error in field A\"></div>\n      <div role=\"alert\" aria-description=\"Error in field B\"></div>\n    "
          expectSelector page "div[aria-description=\"Error in field A\"]" "internal:role=alert[description=\"Error in field A\"s]"
          expectSelector page "div[aria-description=\"Error in field B\"]" "internal:role=alert[description=\"Error in field B\"s]"
    , testCase "should try to improve text" $
        withRecorderPage $ \page -> do
          setContent page "<div>23 Issues</div>"
          expectSelector page "div" "internal:text=\"Issues\"i"
    , testCase "should try to improve text by shortening" $
        withRecorderPage $ \page -> do
          setContent page "<div>Longest verbose description of the item</div>"
          expectSelector page "div" "internal:text=\"Longest verbose description\"i"
    , testCase "should try to improve label text by shortening" $
        withRecorderPage $ \page -> do
          setContent page "<label>Longest verbose description of the item<input></label>"
          expectSelector page "input" "internal:role=textbox[name=\"Longest verbose description\"i]"
    , testCase "should not improve guid text" $
        withRecorderPage $ \page -> do
          setContent page "<div>91b1b23</div>"
          expectSelector page "div" "internal:text=\"91b1b23\"i"
    , testCase "should not escape text with >>" $
        withRecorderPage $ \page -> do
          setContent page "<div>text&gt;&gt;text</div>"
          expectSelector page "div" "internal:text=\"text>>text\"i"
    , testCase "should escape text with quote" $
        withRecorderPage $ \page -> do
          setContent page "<div>text\"text</div>"
          expectSelector page "div" "internal:text=\"text\\\"text\"i"
    , testCase "should escape text with slash" $
        withRecorderPage $ \page -> do
          setContent page "<div>/text</div>"
          expectSelector page "div" "internal:text=\"/text\"i"
    , testCase "should not use text for select" $
        withRecorderPage $ \page -> do
          setContent page "\n      <select><option>foo</option></select>\n      <select mark=1><option>bar</option></select>\n    "
          expectSelector page "[mark=\"1\"]" "internal:role=combobox >> nth=1"
    , testCase "should use ordinal for identical nodes" $
        withRecorderPage $ \page -> do
          setContent page "<div>Text</div><div>Text</div><div mark=1>Text</div><div>Text</div>"
          expectSelector page "div[mark=\"1\"]" "internal:text=\"Text\"i >> nth=2"
    , testCase "should prefer data-testid" $
        withRecorderPage $ \page -> do
          setContent page "<div>Text</div><div>Text</div><div data-testid=a>Text</div><div>Text</div>"
          expectSelector page "[data-testid=\"a\"]" "internal:testid=[data-testid=\"a\"s]"
    , testCase "should use data-testid in strict errors" $
        withChromiumBrowser $ \browser -> do
          ctx1 <- newContext browser
          showRecorder ctx1
          page1 <- newPage ctx1
          setContextTestIdAttributeName ctx1 "data-custom-id"
          checkStrictErrors page1
          ctx2 <- newContext browser
          setContextTestIdAttributeName ctx2 "data-custom-id"
          page2 <- newPage ctx2
          checkStrictErrors page2
    , testCase "should handle first non-unique data-testid" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div data-testid=a mark=1>\n        Text\n      </div>\n      <div data-testid=a>\n        Text\n      </div>"
          expectSelector page "div[mark=\"1\"]" "internal:testid=[data-testid=\"a\"s] >> nth=0"
    , testCase "should handle second non-unique data-testid" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div data-testid=a>\n        Text\n      </div>\n      <div data-testid=a mark=1>\n        Text\n      </div>"
          expectSelector page "div[mark=\"1\"]" "internal:testid=[data-testid=\"a\"s] >> nth=1"
    , testCase "should use readable id" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div></div>\n      <div id=first-item mark=1></div>\n    "
          expectSelector page "div[mark=\"1\"]" "#first-item"
    , testCase "should not use generated id" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div></div>\n      <div id=aAbBcCdDeE mark=1></div>\n    "
          expectSelector page "div[mark=\"1\"]" "div >> nth=1"
    , testCase "should use internal:has-text" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div>Hello world</div>\n      <a>Hello <span>world</span></a>\n      <a>Goodbye <span>world</span></a>\n    "
          expectSelector page "a:has-text(\"Hello\")" "a >> internal:has-text=\"Hello world\"i"
    , testCase "should use internal:has-text with regexp" $
        withRecorderPage $ \page -> do
          setContent page "\n      <span>Hello world</span>\n      <div><div>Hello <span>world</span></div>extra</div>\n      <a>Goodbye <span>world</span></a>\n    "
          expectSelector page "div div" "div >> internal:has-text=/^Hello world$/"
    , testCase "should use internal:has-text with regexp with a quote" $
        withRecorderPage $ \page -> do
          setContent page "\n      <span>Hello'world</span>\n      <div><div>Hello'<span>world</span></div>extra</div>\n      <a>Goodbye'<span>world</span></a>\n    "
          expectSelector page "div div" "div >> internal:has-text=/^Hello\\'world$/"
    , testCase "should chain text after parent" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div>Hello <span>world</span></div>\n      <b>Hello <span mark=1>world</span></b>\n    "
          expectSelector page "[mark=\"1\"]" "b >> internal:text=\"world\"i"
    , testCase "should use parent text" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div>Hello <span>world</span></div>\n      <div>Goodbye <span mark=1>world</span></div>\n    "
          expectSelector page "[mark=\"1\"]" "div >> internal:has-text=\"Goodbye world\"i >> span"
    , testCase "should separate selectors by >>" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div>\n        <div>Text</div>\n      </div>\n      <div id=\"id\">\n        <div>Text</div>\n      </div>\n    "
          expectSelector page "#id > div" "#id >> internal:text=\"Text\"i"
    , testCase "should trim long text" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div>\n        <div>Text that goes on and on and on and on and on and on and on and on and on and on and on and on and on and on and on</div>\n      </div>\n      <div id=\"id\">\n      <div>Text that goes on and on and on and on and on and on and on and on and on and on and on and on and on and on and on</div>\n      <div>Text that goes on and on and on and on and on and on and on and on and X on and on and on and on and on and on and on</div>\n      </div>\n    "
          expectSelector page "#id > div" "#id >> internal:text=\"Text that goes on and on and on and on and on and on and on and on and on and\"i"
    , testCase "should use nested ordinals" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div><c></c><c></c><c></c><c></c><c></c><b></b></div>\n      <div>\n        <b>\n          <c>\n          </c>\n        </b>\n        <b>\n          <c mark=1></c>\n        </b>\n      </div>\n      <div><b></b></div>\n    "
          expectSelector page "c[mark=\"1\"]" "b:nth-child(2) > c"
    , testCase "should prefer class to ordinal" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div><c></c><c></c><c></c><c></c><c></c><b></b></div>\n      <div>\n        <b class=\"foo\">\n          <c>\n          </c>\n        </b>\n        <b>\n          <c mark=1></c>\n        </b>\n      </div>\n      <div><b class=\"foo\"></b></div>\n    "
          mutate page "[mark=\"1\"]" "(c) => { c.parentElement.className = 'foo 12.bar.baz[&x]-_?\"\\''; return ''; }"
          expectSelector page "c[mark=\"1\"]" ".foo.\\31 2\\.bar\\.baz\\[\\&x\\]-_\\?\\\"\\' > c"
    , testCase "should properly join child selectors under nested ordinals" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div><c></c><c></c><c></c><c></c><c></c><b></b></div>\n      <div>\n        <b>\n          <div>\n            <c>\n            </c>\n          </div>\n        </b>\n        <b>\n          <div>\n            <c mark=1></c>\n          </div>\n        </b>\n      </div>\n      <div><b></b></div>\n    "
          expectSelector page "c[mark=\"1\"]" "b:nth-child(2) > div > c"
    , testCase "should not use input[value]" $
        withRecorderPage $ \page -> do
          setContent page "\n      <input value=\"one\">\n      <input value=\"two\" mark=\"1\">\n      <input value=\"three\">\n    "
          expectSelector page "input[mark=\"1\"]" "internal:role=textbox >> nth=1"
    , testGroup
        "should prioritize attributes correctly"
        [ testCase "role" $
            withRecorderPage $ \page -> do
              setContent page "<input name=\"foobar\" type=\"text\"/>"
              expectSelector page "input" "internal:role=textbox"
        , testCase "placeholder" $
            withRecorderPage $ \page -> do
              setContent page "<input placeholder=\"foobar\" type=\"text\"/>"
              expectSelector page "input" "internal:role=textbox[name=\"foobar\"i]"
        , testCase "name" $
            withRecorderPage $ \page -> do
              setContent page "\n        <input aria-hidden=\"false\" name=\"foobar\" type=\"date\"/>\n        <div role=\"textbox\"/>content</div>\n      "
              expectSelector page "input" "input[name=\"foobar\"]"
        , testCase "type" $
            withRecorderPage $ \page -> do
              setContent page "\n        <input aria-hidden=\"false\" type=\"checkbox\"/>\n        <div role=\"checkbox\"/>content</div>\n      "
              expectSelector page "input" "input[type=\"checkbox\"]"
        ]
    , testCase "should find text in shadow dom" $
        withRecorderPage $ \page -> do
          setContent page "<div></div>"
          mutate page "div" "(div) => { const shadowRoot = div.attachShadow({ mode: 'open' }); const span = document.createElement('span'); span.textContent = 'Target'; shadowRoot.appendChild(span); return ''; }"
          expectSelector page "span" "internal:text=\"Target\"i"
    , testCase "should match in shadow dom" $
        withRecorderPage $ \page -> do
          setContent page "<div></div>"
          mutate page "div" "(div) => { const shadowRoot = div.attachShadow({ mode: 'open' }); const input = document.createElement('input'); shadowRoot.appendChild(input); return ''; }"
          expectSelector page "input" "internal:role=textbox"
    , testCase "should match in deep shadow dom" $
        withRecorderPage $ \page -> do
          setContent page "<div></div><div></div><div><input></div>"
          mutate page "div" "(div1) => { const shadowRoot1 = div1.attachShadow({ mode: 'open' }); const input1 = document.createElement('input'); shadowRoot1.appendChild(input1); const divExtra3 = document.createElement('div'); shadowRoot1.append(divExtra3); const div2 = document.createElement('div'); shadowRoot1.append(div2); const shadowRoot2 = div2.attachShadow({ mode: 'open' }); const input2 = document.createElement('input'); input2.setAttribute('value', 'foo'); shadowRoot2.appendChild(input2); return ''; }"
          expectSelector page "input[value=foo]" "internal:role=textbox >> nth=2"
    , testCase "should work in dynamic iframes without navigation" $
        withChromiumContext $ \ctx page -> do
          showRecorder ctx
          setContent page "<div></div>"
          _ <- evaluateText page "async () => { await new Promise(f => { const iframe = document.createElement('iframe'); iframe.onload = () => { iframe.contentDocument.body.innerHTML = '<div>Target</div>'; f(); }; document.body.appendChild(iframe); }); return 'ok'; }"
          children <- childFrames (mainFrame page)
          case children of
            frame : _ -> do
              -- The recorder helpers are installed asynchronously in a newly attached frame.
              _ <- waitForFunctionOnFrame frame "() => !!window.playwright"
              assertEqual "selector" "internal:text=\"Target\"i" =<< generateInFrame frame "div"
            [] ->
              assertFailure "expected a dynamic iframe"
    , testCase "should use the name attributes for elements that can have it" $
        withRecorderPage $ \page -> do
          mapM_
            ( \tagName -> do
                setContent page ("<form><" <> tagName <> " name=\"foo\"></" <> tagName <> "><" <> tagName <> " name=\"bar\"></" <> tagName <> "></form>")
                expectSelector page "[name=bar]" (tagName <> "[name=\"bar\"]")
            )
            ["button", "input", "textarea"]
          setContent page "<iframe name=\"foo\"></iframe><iframe name=\"bar\"></iframe>"
          expectSelector page "[name=bar]" "iframe[name=\"bar\"]"
          setContent page "<frameset><frame name=\"foo\"></frame><frame name=\"bar\"></frame></frameset>"
          expectSelector page "[name=bar]" "frame[name=\"bar\"]"
    , testCase "should work with tricky attributes" $
        withRecorderPage $ \page -> do
          setContent page "<button id=\"this:is-my-tricky.id\"><span></span></button>"
          expectSelector page "button" "[id=\"this:is-my-tricky.id\"]"
          setContent page "<ng:switch><span></span></ng:switch>"
          expectSelector page "ng\\:switch" "ng\\:switch"
          setContent page "<button><span></span></button><button></button>"
          mutate page "span" "(span) => { span.textContent = '!#\\'!?\"\\\\:'; return ''; }"
          expectSelector page "button" "internal:role=button[name=\"!#'!?\\\"\\\\:\"i]"
          setContent page "<div><span></span></div>"
          mutate page "div" "(div) => { div.id = '!#\\'!?\"\\\\:'; return ''; }"
          expectSelector page "div" "[id=\"!#'!?\\\"\\\\:\"]"
    , testCase "should work without CSS.escape" $
        withRecorderPage $ \page -> do
          setContent page "<button aria-hidden=\"false\"></button><div role=\"button\"></div>"
          mutate page "button" "(button) => { delete window.CSS.escape; button.setAttribute('name', '-tricky\\u0001name'); return ''; }"
          expectSelector page "button" "button[name=\"-tricky\SOHname\"]"
    , testCase "should not over-escape for CSS syntax" $
        withRecorderPage $ \page -> do
          setContent page "<button aria-hidden=\"false\" name=\"123\"></button><div role=\"button\"></div>"
          expectSelector page "button" "button[name=\"123\"]"
    , testCase "should ignore empty aria-label for candidate consideration" $
        withRecorderPage $ \page -> do
          setContent page "<button aria-label=\"\" id=\"buttonId\"></button>"
          expectSelector page "button" "#buttonId"
    , testCase "should accept valid aria-label for candidate consideration" $
        withRecorderPage $ \page -> do
          setContent page "<button aria-label=\"ariaLabel\" id=\"buttonId\"></button>"
          expectSelector page "button" "internal:role=button[name=\"ariaLabel\"i]"
    , testCase "should generate title selector" $
        withRecorderPage $ \page -> do
          setContent page "<div>\n      <button title=\"Send to\" aria-description=\"High-Speed Parcel Delivery\">Send</button>\n      <button aria-description=\"High-Speed Parcel Delivery\">Send</button>\n    </div>"
          expectSelector page "button" "internal:attr=[title=\"Send to\"i]"
    , testCase "should ignore empty role for candidate consideration" $
        withRecorderPage $ \page -> do
          setContent page "<button role=\"\" id=\"buttonId\"></button>"
          expectSelector page "button" "#buttonId"
    , testCase "should not accept invalid role for candidate consideration" $
        withRecorderPage $ \page -> do
          setContent page "<button role=\"roleDescription\" id=\"buttonId\"></button>"
          expectSelector page "button" "#buttonId"
    , testCase "should ignore empty data-test-id for candidate consideration" $
        withRecorderPage $ \page -> do
          setContent page "<button data-test-id=\"\" id=\"buttonId\"></button>"
          expectSelector page "button" "#buttonId"
    , testCase "should accept valid data-test-id for candidate consideration" $
        withRecorderPage $ \page -> do
          setContent page "<button data-test-id=\"testId\" id=\"buttonId\"></button>"
          expectSelector page "button" "[data-test-id=\"testId\"]"
    , testCase "should generate label selector" $
        withRecorderPage $ \page -> do
          setContent page "\n      <label for=target1>Target1</label><input id=target1>\n      <label for=target2>Target2</label><button id=target2>??</button>\n      <label for=target3>Target3</label><select id=target3><option>hey</option></select>\n      <label for=target4>Target4</label><progress id=target4 value=70 max=100>70%</progress>\n      <label for=target5>Target5</label><input id=target5 type=hidden>\n      <label for=target6>Target6</label><div id=target6>text</div>\n    "
          expectSelector page "#target1" "internal:role=textbox[name=\"Target1\"i]"
          expectSelector page "#target2" "internal:role=button[name=\"Target2\"i]"
          expectSelector page "#target3" "internal:label=\"Target3\"i"
          expectSelector page "#target4" "internal:role=progressbar[name=\"Target4\"i]"
          expectSelector page "#target5" "#target5"
          expectSelector page "#target6" "internal:text=\"text\"i"
          setContent page "<label for=target>Coun\"try</label><input id=target>"
          expectSelector page "input" "internal:role=textbox[name=\"Coun\\\"try\"i]"
    , testCase "should prefer role other input[type]" $
        withRecorderPage $ \page -> do
          setContent page "<input type=checkbox><div data-testid=wrapper><input type=checkbox></div>"
          expectSelector page "[data-testid=wrapper] > input" "internal:testid=[data-testid=\"wrapper\"s] >> internal:role=checkbox"
    , testCase "should generate exact text when necessary" $
        withRecorderPage $ \page -> do
          setContent page "\n      <span>Text</span>\n      <span>Text and more</span>\n    "
          expectSelector page "span" "internal:text=\"Text\"s"
    , testCase "should generate exact title when necessary" $
        withRecorderPage $ \page -> do
          setContent page "\n      <span title=\"Text\"></span>\n      <span title=\"Text and more\"></span>\n    "
          expectSelector page "span" "internal:attr=[title=\"Text\"s]"
    , testCase "should generate exact placeholder when necessary" $
        withRecorderPage $ \page -> do
          setContent page "\n      <input placeholder=\"Text\"></input>\n      <input placeholder=\"Text and more\"></input>\n    "
          expectSelector page "input" "internal:role=textbox[name=\"Text\"s]"
    , testCase "should generate exact role when necessary" $
        withRecorderPage $ \page -> do
          setContent page "\n      <img alt=\"Text\"></img>\n      <img alt=\"Text and more\"></img>\n    "
          expectSelector page "img" "internal:role=img[name=\"Text\"s]"
    , testCase "should generate exact label when necessary" $
        withRecorderPage $ \page -> do
          setContent page "\n      <label>Text <input></input></label>\n      <label>Text and more <input></input></label>\n    "
          expectSelector page "input" "internal:role=textbox[name=\"Text\"s]"
    , testCase "should generate relative selector" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div>\n        <span>Hello</span>\n        <span>World</span>\n      </div>\n      <section>\n        <span>Hello</span>\n        <span>World</span>\n      </section>\n    "
          relative <- evaluateText page "() => { const target = document.querySelector('section > span'); const root = document.querySelector('section'); return window.__injectedScript.generateSelectorSimple(target, { root }); }"
          absolute <- evaluateText page "() => { const target = document.querySelector('section > span'); return window.__injectedScript.generateSelectorSimple(target); }"
          assertEqual "relative" "internal:text=\"Hello\"i" relative
          assertEqual "absolute" "section >> internal:text=\"Hello\"i" absolute
    , testCase "should generate noText: no text engine" $
        withRecorderPage $ \page -> do
          setContent page "<div>Some text</div>"
          expectNoText page "div" "div"
    , testCase "should generate noText: no name from content" $
        withRecorderPage $ \page -> do
          setContent page "<button><span>Send</span></button>"
          expectNoText page "button" "internal:role=button"
    , testCase "should generate noText: name from aria-label" $
        withRecorderPage $ \page -> do
          setContent page "<button aria-label=\"Send message\"><span>Send</span></button>"
          expectNoText page "button" "internal:role=button[name=\"Send message\"i]"
    , testCase "should generate noText: name from external aria-labelledby" $
        withRecorderPage $ \page -> do
          setContent page "<span id=\"label\">Editor</span><div role=\"textbox\" aria-labelledby=\"label\" contenteditable>Text</div>"
          expectNoText page "div[role=textbox]" "internal:role=textbox[name=\"Editor\"i]"
    , testCase "should generate noText: ignore aria-labelledby pointing inside the element" $
        withRecorderPage $ \page -> do
          setContent page "<div role=\"textbox\" aria-labelledby=\"child\" contenteditable><p id=\"child\">Title</p><p>Text</p></div>"
          expectNoText page "div[role=textbox]" "internal:role=textbox"
    , testCase "should generate noText: contenteditable heading" $
        withRecorderPage $ \page -> do
          setContent page "<h1 contenteditable>Page title</h1>"
          expectNoText page "h1" "internal:role=heading"
    , testCase "should generate noText: description from external aria-describedby" $
        withRecorderPage $ \page -> do
          setContent page "\n      <span id=\"desc1\">First</span><span id=\"desc2\">Second</span>\n      <div role=\"textbox\" aria-label=\"Editor\" aria-describedby=\"desc1\" contenteditable>foo</div>\n      <div role=\"textbox\" aria-label=\"Editor\" aria-describedby=\"desc2\" contenteditable>bar</div>\n    "
          expectNoText page "div[aria-describedby=desc1]" "internal:role=textbox[name=\"Editor\"i][description=\"First\"i]"
    , testCase "should generate noText: ignore aria-describedby pointing inside the element" $
        withRecorderPage $ \page -> do
          setContent page "\n      <div role=\"textbox\" aria-label=\"Editor\" aria-describedby=\"child\" contenteditable><p id=\"child\">First</p></div>\n      <div role=\"textbox\" aria-label=\"Editor\" contenteditable>Second</div>\n    "
          expectNoText page "div[aria-describedby=child]" "internal:role=textbox[name=\"Editor\"i] >> nth=0"
    , testCase "should prefer role with hasText to css with hasText" $
        withRecorderPage $ \page -> do
          setContent page "\n      <ul>\n        <li>\n          <input aria-label=\"Toggle Todo\" type=\"checkbox\">\n          buy flowers\n        </li>\n        <li>\n          <input aria-label=\"Toggle Todo\" type=\"checkbox\">\n          sell milk\n        </li>\n      </ul>\n    "
          expectSelector page "input" "internal:role=listitem >> internal:has-text=\"buy flowers\"i >> internal:label=\"Toggle Todo\"i"
    , testCase "should not use icon fonts aria name" $
        withRecorderPage $ \page -> do
          setContent page "\n      <style>\n        .icon-foo:before {\n          content: \"\\F1D4\";\n        }\n        .icon-bar:before {\n          content: \"\\F0000\";\n        }\n      </style>\n      <div>\n        <button><i class=\"icon-foo\"></i></button>\n        <button><i class=\"icon-bar\"></i></button>\n      </div>\n    "
          expectSelector page "button:first-child" "internal:role=button >> nth=0"
          expectSelector page "button:last-child" "internal:role=button >> nth=1"
    ]
