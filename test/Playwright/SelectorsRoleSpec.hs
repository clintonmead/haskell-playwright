module Playwright.SelectorsRoleSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , unpack
  )
import Playwright
  ( AriaRole (AriaRole)
  , GetByRoleOptions (getByRoleChecked, getByRoleDisabled, getByRoleExpanded, getByRoleIncludeHidden, getByRoleLevel, getByRoleName, getByRolePressed, getByRoleSelected)
  , Locator
  , Page
  , allInnerTexts
  , defaultGetByRoleOptions
  , evalOnSelector
  , evaluateOnLocatorAllTexts
  , getByRole
  , getByTestId
  , isDisabled
  , isEnabled
  , isVisible
  , locator
  , locatorCount
  , querySelector
  , selectorFromText
  , setContent
  , Playwright
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withChromiumPage
  , assertEqual
  )
import Playwright.Locator qualified as Locator
  ( getByRole
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
    "selectors-role"
    [ testCase "should detect roles" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<button>Hello</button><select multiple=\"\" size=\"2\"></select><select></select><h3>Heading</h3><details><summary>Hello</summary></details><div role=\"dialog\">I am a dialog</div>"
          assertRoleHtmls page "role=button" ["<button>Hello</button>"]
          assertRoleHtmls page "role=listbox" ["<select multiple=\"\" size=\"2\"></select>"]
          assertRoleHtmls page "role=combobox" ["<select></select>"]
          assertRoleHtmls page "role=heading" ["<h3>Heading</h3>"]
          assertRoleHtmls page "role=group" ["<details><summary>Hello</summary></details>"]
          assertRoleHtmls page "role=dialog" ["<div role=\"dialog\">I am a dialog</div>"]
          assertRoleHtmls page "role=menuitem" []
          assertGetByHtmls page (AriaRole "menuitem") defaultGetByRoleOptions []
    , testCase "should support selected" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<select><option>Hi</option><option selected>Hello</option></select><div><div role=\"option\" aria-selected=\"true\">Hi</div><div role=\"option\" aria-selected=\"false\">Hello</div></div>"
          let
            selected :: [Text]
            selected =
              [ "<option selected=\"\">Hello</option>"
              , "<div role=\"option\" aria-selected=\"true\">Hi</div>"
              ]
            unselected :: [Text]
            unselected =
              [ "<option>Hi</option>"
              , "<div role=\"option\" aria-selected=\"false\">Hello</div>"
              ]
          assertRoleHtmls page "role=option[selected]" selected
          assertRoleHtmls page "role=option[selected=true]" selected
          assertGetByHtmls
            page
            (AriaRole "option")
            defaultGetByRoleOptions {getByRoleSelected = Just True}
            selected
          assertRoleHtmls page "role=option[selected=false]" unselected
          assertGetByHtmls
            page
            (AriaRole "option")
            defaultGetByRoleOptions {getByRoleSelected = Just False}
            unselected
    , testCase "should support checked" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<input type=checkbox><input type=checkbox checked><input type=checkbox indeterminate><div role=checkbox aria-checked=\"true\">Hi</div><div role=checkbox aria-checked=\"false\">Hello</div><div role=checkbox>Unknown</div>"
          _ <- evalOnSelector page (selectorFromText "[indeterminate]") "input => { input.indeterminate = true; }"
          let
            checked :: [Text]
            checked =
              [ "<input type=\"checkbox\" checked=\"\">"
              , "<div role=\"checkbox\" aria-checked=\"true\">Hi</div>"
              ]
            unchecked :: [Text]
            unchecked =
              [ "<input type=\"checkbox\">"
              , "<div role=\"checkbox\" aria-checked=\"false\">Hello</div>"
              , "<div role=\"checkbox\">Unknown</div>"
              ]
          assertRoleHtmls page "role=checkbox[checked]" checked
          assertRoleHtmls page "role=checkbox[checked=true]" checked
          assertGetByHtmls
            page
            (AriaRole "checkbox")
            defaultGetByRoleOptions {getByRoleChecked = Just True}
            checked
          assertRoleHtmls page "role=checkbox[checked=false]" unchecked
          assertGetByHtmls
            page
            (AriaRole "checkbox")
            defaultGetByRoleOptions {getByRoleChecked = Just False}
            unchecked
          assertRoleHtmls page "role=checkbox[checked=\"mixed\"]" ["<input type=\"checkbox\" indeterminate=\"\">"]
          assertRoleHtmls
            page
            "role=checkbox"
            [ "<input type=\"checkbox\">"
            , "<input type=\"checkbox\" checked=\"\">"
            , "<input type=\"checkbox\" indeterminate=\"\">"
            , "<div role=\"checkbox\" aria-checked=\"true\">Hi</div>"
            , "<div role=\"checkbox\" aria-checked=\"false\">Hello</div>"
            , "<div role=\"checkbox\">Unknown</div>"
            ]
    , testCase "should support pressed" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<button>Hi</button><button aria-pressed=\"true\">Hello</button><button aria-pressed=\"false\">Bye</button><button aria-pressed=\"mixed\">Mixed</button>"
          let
            pressed :: [Text]
            pressed = ["<button aria-pressed=\"true\">Hello</button>"]
            unpressed :: [Text]
            unpressed =
              [ "<button>Hi</button>"
              , "<button aria-pressed=\"false\">Bye</button>"
              ]
          assertRoleHtmls page "role=button[pressed]" pressed
          assertRoleHtmls page "role=button[pressed=true]" pressed
          assertGetByHtmls
            page
            (AriaRole "button")
            defaultGetByRoleOptions {getByRolePressed = Just True}
            pressed
          assertRoleHtmls page "role=button[pressed=false]" unpressed
          assertGetByHtmls
            page
            (AriaRole "button")
            defaultGetByRoleOptions {getByRolePressed = Just False}
            unpressed
          assertRoleHtmls page "role=button[pressed=\"mixed\"]" ["<button aria-pressed=\"mixed\">Mixed</button>"]
          assertRoleHtmls
            page
            "role=button"
            [ "<button>Hi</button>"
            , "<button aria-pressed=\"true\">Hello</button>"
            , "<button aria-pressed=\"false\">Bye</button>"
            , "<button aria-pressed=\"mixed\">Mixed</button>"
            ]
    , testCase "should support expanded" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div role=\"treeitem\">Hi</div><div role=\"treeitem\" aria-expanded=\"true\">Hello</div><div role=\"treeitem\" aria-expanded=\"false\">Bye</div>"
          let
            allItems :: [Text]
            allItems =
              [ "<div role=\"treeitem\">Hi</div>"
              , "<div role=\"treeitem\" aria-expanded=\"true\">Hello</div>"
              , "<div role=\"treeitem\" aria-expanded=\"false\">Bye</div>"
              ]
            expanded :: [Text]
            expanded = ["<div role=\"treeitem\" aria-expanded=\"true\">Hello</div>"]
            collapsed :: [Text]
            collapsed = ["<div role=\"treeitem\" aria-expanded=\"false\">Bye</div>"]
          assertRoleHtmls page "role=treeitem" allItems
          assertGetByHtmls page (AriaRole "treeitem") defaultGetByRoleOptions allItems
          assertRoleHtmls page "role=treeitem[expanded]" expanded
          assertRoleHtmls page "role=treeitem[expanded=true]" expanded
          assertGetByHtmls
            page
            (AriaRole "treeitem")
            defaultGetByRoleOptions {getByRoleExpanded = Just True}
            expanded
          assertRoleHtmls page "role=treeitem[expanded=false]" collapsed
          assertGetByHtmls
            page
            (AriaRole "treeitem")
            defaultGetByRoleOptions {getByRoleExpanded = Just False}
            collapsed
          assertRoleHtmls page "[role=treeitem]:not([aria-expanded])" ["<div role=\"treeitem\">Hi</div>"]
    , testCase "should support disabled" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<button>Hi</button><button disabled>Bye</button><button aria-disabled=\"true\">Hello</button><button aria-disabled=\"false\">Oh</button><fieldset disabled><button>Yay</button></fieldset><select><optgroup disabled><option>one</option></optgroup><optgroup><option>two</option></optgroup><option disabled>three</option></select>"
          let
            disabledButtons :: [Text]
            disabledButtons =
              [ "<button disabled=\"\">Bye</button>"
              , "<button aria-disabled=\"true\">Hello</button>"
              , "<button>Yay</button>"
              ]
            enabledButtons :: [Text]
            enabledButtons =
              [ "<button>Hi</button>"
              , "<button aria-disabled=\"false\">Oh</button>"
              ]
          assertRoleHtmls page "role=button[disabled]" disabledButtons
          assertRoleHtmls page "role=button[disabled=true]" disabledButtons
          assertGetByHtmls
            page
            (AriaRole "button")
            defaultGetByRoleOptions {getByRoleDisabled = Just True}
            disabledButtons
          assertRoleHtmls page "role=button[disabled=false]" enabledButtons
          assertGetByHtmls
            page
            (AriaRole "button")
            defaultGetByRoleOptions {getByRoleDisabled = Just False}
            enabledButtons
          assertGetByHtmls
            page
            (AriaRole "option")
            defaultGetByRoleOptions {getByRoleDisabled = Just True}
            ["<option>one</option>", "<option disabled=\"\">three</option>"]
          assertGetByHtmls
            page
            (AriaRole "option")
            defaultGetByRoleOptions {getByRoleDisabled = Just False}
            ["<option>two</option>"]
          assertGetByHtmls
            page
            (AriaRole "option")
            defaultGetByRoleOptions
            ["<option>one</option>", "<option>two</option>", "<option disabled=\"\">three</option>"]
    , testCase "should inherit disabled from the ancestor" $
        withChromiumPage $ \page -> do
          setContent page "<span aria-disabled=\"true\"><button>Click me!</button></span>"
          assertEqual "button" True =<< isDisabled (locate page "button")
          setContent page "<span aria-disabled=\"true\"><h1>Heading</h1></span>"
          assertEqual "heading" False =<< isDisabled (locate page "h1")
    , testCase "should support disabled fieldset" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<fieldset disabled><input></input><button data-testid=\"inside-fieldset-element\">x</button><legend><button data-testid=\"inside-legend-element\">legend</button></legend></fieldset><fieldset disabled><legend><div><button data-testid=\"nested-inside-legend-element\">x</button></div></legend></fieldset><fieldset disabled><div></div><legend><button data-testid=\"first-legend-element\">x</button></legend><legend><button data-testid=\"second-legend-element\">x</button></legend></fieldset><fieldset disabled><fieldset><button data-testid=\"deep-button\">x</button></fieldset></fieldset>"
          assertEqual "legend" True =<< isEnabled (getByTestId page "inside-legend-element")
          assertEqual "nested legend" True =<< isEnabled (getByTestId page "nested-inside-legend-element")
          assertEqual "first legend" True =<< isEnabled (getByTestId page "first-legend-element")
          assertEqual "second legend" True =<< isDisabled (getByTestId page "second-legend-element")
          assertEqual "deep" True =<< isDisabled (getByTestId page "deep-button")
    , testCase "should support level" $
        withChromiumPage $ \page -> do
          setContent page "<h1>Hello</h1><h3>Hi</h3><div role=\"heading\" aria-level=\"5\">Bye</div>"
          assertRoleHtmls page "role=heading[level=1]" ["<h1>Hello</h1>"]
          assertGetByHtmls
            page
            (AriaRole "heading")
            defaultGetByRoleOptions {getByRoleLevel = Just 1}
            ["<h1>Hello</h1>"]
          assertRoleHtmls page "role=heading[level=3]" ["<h3>Hi</h3>"]
          assertGetByHtmls
            page
            (AriaRole "heading")
            defaultGetByRoleOptions {getByRoleLevel = Just 3}
            ["<h3>Hi</h3>"]
          assertRoleHtmls page "role=heading[level=5]" ["<div role=\"heading\" aria-level=\"5\">Bye</div>"]
    , testCase "should filter hidden, unless explicitly asked for" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<button>Hi</button><button hidden>Hello</button><button aria-hidden=\"true\">Yay</button><button aria-hidden=\"false\">Nay</button><button style=\"visibility:hidden\">Bye</button><div style=\"visibility:hidden\"><button>Oh</button></div><div style=\"visibility:hidden\"><button style=\"visibility:visible\">Still here</button></div><button style=\"display:none\">Never</button><div id=host1></div><div id=host2 style=\"display:none\"></div><input name=\"one\"><details><summary>Open form</summary><label>Label<input name=\"two\"></label></details><select><option style=\"visibility:hidden\">One</option><option style=\"display:none\">Two</option><option>Three</option></select><script>function addButton(host, text) { const root = host.attachShadow({ mode: 'open' }); const button = document.createElement('button'); button.textContent = text; root.appendChild(button); } addButton(document.getElementById('host1'), 'Shadow1'); addButton(document.getElementById('host2'), 'Shadow2');</script>"
          let
            visibleButtons :: [Text]
            visibleButtons =
              [ "<button>Hi</button>"
              , "<button aria-hidden=\"false\">Nay</button>"
              , "<button style=\"visibility:visible\">Still here</button>"
              , "<button>Shadow1</button>"
              ]
            hiddenButtons :: [Text]
            hiddenButtons =
              [ "<button>Hi</button>"
              , "<button hidden=\"\">Hello</button>"
              , "<button aria-hidden=\"true\">Yay</button>"
              , "<button aria-hidden=\"false\">Nay</button>"
              , "<button style=\"visibility:hidden\">Bye</button>"
              , "<button>Oh</button>"
              , "<button style=\"visibility:visible\">Still here</button>"
              , "<button style=\"display:none\">Never</button>"
              , "<button>Shadow1</button>"
              , "<button>Shadow2</button>"
              ]
          assertRoleHtmls page "role=button" visibleButtons
          assertRoleHtmls page "role=button[include-hidden]" hiddenButtons
          assertRoleHtmls page "role=button[include-hidden=true]" hiddenButtons
          assertRoleHtmls page "role=button[include-hidden=false]" visibleButtons
          assertRoleHtmls page "role=textbox" ["<input name=\"one\">"]
          assertRoleHtmls
            page
            "role=option"
            ["<option style=\"visibility:hidden\">One</option>", "<option>Three</option>"]
    , testCase "should support name" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div role=\"button\" aria-label=\" Hello \"></div><div role=\"button\" aria-label=\"Hallo\"></div><div role=\"button\" aria-label=\"Hello\" aria-hidden=\"true\"></div><div role=\"button\" aria-label=\"123\" aria-hidden=\"true\"></div><div role=\"button\" aria-label='foo\"bar' aria-hidden=\"true\"></div>"
          let
            helloVisible :: [Text]
            helloVisible = ["<div role=\"button\" aria-label=\" Hello \"></div>"]
            hallo :: [Text]
            hallo = ["<div role=\"button\" aria-label=\"Hallo\"></div>"]
            helloHidden :: [Text]
            helloHidden =
              [ "<div role=\"button\" aria-label=\" Hello \"></div>"
              , "<div role=\"button\" aria-label=\"Hello\" aria-hidden=\"true\"></div>"
              ]
          assertRoleHtmls page "role=button[name=\"Hello\"]" helloVisible
          assertRoleHtmls page "role=button[name=\" \n Hello \"]" helloVisible
          assertGetByHtmls
            page
            (AriaRole "button")
            defaultGetByRoleOptions {getByRoleName = Just "Hello"}
            helloVisible
          assertRoleHtmls page "role=button[name*=\"all\"]" hallo
          assertRoleHtmls
            page
            "role=button[name=/^H[ae]llo$/]"
            (helloVisible <> hallo)
          assertRoleHtmls
            page
            "role=button[name=/h.*o/i]"
            (helloVisible <> hallo)
          assertRoleHtmls page "role=button[name=\"Hello\"][include-hidden]" helloHidden
          assertGetByHtmls
            page
            (AriaRole "button")
            defaultGetByRoleOptions {getByRoleName = Just "Hello", getByRoleIncludeHidden = Just True}
            helloHidden
          assertGetByHtmls
            page
            (AriaRole "button")
            defaultGetByRoleOptions {getByRoleName = Just "hello", getByRoleIncludeHidden = Just True}
            helloHidden
          assertRoleHtmls page "role=button[name=Hello]" helloVisible
          assertRoleHtmls
            page
            "role=button[name=123][include-hidden]"
            ["<div role=\"button\" aria-label=\"123\" aria-hidden=\"true\"></div>"]
          assertGetByHtmls
            page
            (AriaRole "button")
            defaultGetByRoleOptions {getByRoleName = Just "123", getByRoleIncludeHidden = Just True}
            ["<div role=\"button\" aria-label=\"123\" aria-hidden=\"true\"></div>"]
    , testCase "should support option name with html whitespace" $
        withChromiumPage $ \page -> do
          setContent page "<select><option value=\"html\">&nbsp;HTML</option></select>"
          count <-
            locatorCount
              ( getByRole
                  page
                  (AriaRole "option")
                  defaultGetByRoleOptions {getByRoleName = Just "HTML"}
              )
          assertEqual "count" 1 count
    , testCase "errors" $
        withChromiumPage $ \page -> do
          assertProtocolContains "Role must not be empty" (querySelector page (selectorFromText "role=[bar]"))
          assertProtocolContains
            "Unknown attribute \"sElected\""
            (querySelector page (selectorFromText "role=foo[sElected]"))
          assertProtocolContains
            "Unknown attribute \"bar.qux\""
            (querySelector page (selectorFromText "role=foo[bar . qux=true]"))
          assertProtocolContains
            "\"level\" attribute must be compared to a number"
            (querySelector page (selectorFromText "role=heading[level=\"bar\"]"))
          assertProtocolContains
            "\"checked\" must be one of true, false, \"mixed\""
            (querySelector page (selectorFromText "role=checkbox[checked=\"bar\"]"))
          assertProtocolContains
            "cannot use ~= in attribute with non-string matching value"
            (querySelector page (selectorFromText "role=checkbox[checked~=true]"))
          assertProtocolContains
            "\"level\" attribute is only supported for roles"
            (querySelector page (selectorFromText "role=button[level=3]"))
          assertProtocolContains
            "\"name\" attribute must have a value"
            (querySelector page (selectorFromText "role=button[name]"))
          assertProtocolContains
            "\"expanded\" must be one of true, false"
            (querySelector page (selectorFromText "role=treeitem[expanded=\"none\"]"))
    , testCase "hidden with shadow dom slots" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div make-hidden><button>hidden1</button></div><div make-hidden><span><button>hidden2</button></v></div><div><button>visible1</button></div><div><span><button>visible2</button></span></div><script>for (const div of document.querySelectorAll('div')) { const hidden = div.hasAttribute('make-hidden'); div.attachShadow({ mode: 'open' }).innerHTML = hidden ? 'nothing to see here' : '<slot></slot>'; }</script>"
          assertRoleHtmls
            page
            "role=button"
            ["<button>visible1</button>", "<button>visible2</button>"]
          assertRoleHtmls
            page
            "role=button[include-hidden]"
            [ "<button>hidden1</button>"
            , "<button>hidden2</button>"
            , "<button>visible1</button>"
            , "<button>visible2</button>"
            ]
    , testCase "should support output accessible name" $
        withChromiumPage $ \page -> do
          setContent page "<label>Output1<output>output</output></label>"
          assertEqual "visible" True
            =<< isVisible
              ( getByRole
                  page
                  (AriaRole "status")
                  defaultGetByRoleOptions {getByRoleName = Just "Output1"}
              )
    , testCase "should not match scope by default" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<ul><li aria-label=\"Parent list\">Parent list<ul><li>child 1</li><li>child 2</li></ul></li></ul>"
          let
            children :: Locator
            children =
              Locator.getByRole
                ( getByRole
                    page
                    (AriaRole "listitem")
                    defaultGetByRoleOptions {getByRoleName = Just "Parent list"}
                )
                (AriaRole "listitem")
                defaultGetByRoleOptions
          assertEqual "count" 2 =<< locatorCount children
          assertEqual "text" ["child 1", "child 2"] =<< allInnerTexts children
    ]

assertRoleHtmls :: Page -> Text -> [Text] -> Playwright ()
assertRoleHtmls page sel expected =
  assertEqual (unpack sel) expected
    =<< evaluateOnLocatorAllTexts (locator page (selectorFromText sel)) "els => els.map(e => e.outerHTML)"

assertGetByHtmls :: Page -> AriaRole -> GetByRoleOptions -> [Text] -> Playwright ()
assertGetByHtmls page role opts expected =
  assertEqual "getByRole" expected
    =<< evaluateOnLocatorAllTexts (getByRole page role opts) "els => els.map(e => e.outerHTML)"
