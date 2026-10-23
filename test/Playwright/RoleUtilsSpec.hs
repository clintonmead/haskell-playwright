-- Ports upstream tests/library/role-utils.spec.ts.
--
-- Upstream reads each element's accessible name and role through the internal
-- `window.__injectedScript.utils` helpers, which the Haskell client cannot call.
-- Here the same expectations are checked through the public API: the first
-- line of `ariaSnapshot` gives the role and quoted accessible name, and
-- `getByRole` plus `isVisible` / `locatorCount` cover the getByRole cases.
--
-- Skipped upstream titles (not present in the tree, so they cannot pass or fail):
--   wpt accname #0 .. #3: the wpt/accname/manual fixtures are driven by an
--     ATTAcomm JavaScript shim and checked with the injected-script name and
--     description helpers, which the Haskell client cannot call.
--   wpt accname non-manual: the wpt/accname/name fixtures are driven by an
--     AriaUtils.verifyLabelsBySelector shim in the page, not a Haskell data set.
--   axe-core implicit-role and axe-core accessible-text: the data files are
--     JavaScript modules (module.exports = [...]) that the Haskell harness does
--     not load.
-- Skipped subcases inside ported titles (ariaSnapshot exposes no name or role here):
--   svg title: the role-less g element named "Hello".
--   control embedded in a label: the role-less label with an empty name.
--   svg role=presentation: the svg role "presentation" (only its absence is checked).
module Playwright.RoleUtilsSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Data.Text qualified as T
import Playwright
  ( AriaRole (AriaRole)
  , Exactness (Exact)
  , GetByRoleOptions (getByRoleExact, getByRoleName)
  , Page
  , Playwright
  , ariaSnapshot
  , defaultGetByRoleOptions
  , getByRole
  , isVisible
  , locatorCount
  , setContent
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , locate
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
    "role-utils"
    [ testCase "accessible name with slots" $
        withChromiumPage $ \page -> do
          -- Text "foo" is assigned to the slot, should not be used twice.
          setContent
            page
            (htmlLines
              [ "<button><div>foo</div></button>"
              , "<script>"
              , "(() => {"
              , "const container = document.querySelector('div');"
              , "const shadow = container.attachShadow({ mode: 'open' });"
              , "const slot = document.createElement('slot');"
              , "shadow.appendChild(slot);"
              , "})();"
              , "</script>"
              ])
          checkRoleName page "button" ("button", "foo")

          -- Text "foo" is assigned to the slot, should be used instead of slot content.
          setContent
            page
            (htmlLines
              [ "<div>foo</div>"
              , "<script>"
              , "(() => {"
              , "const container = document.querySelector('div');"
              , "const shadow = container.attachShadow({ mode: 'open' });"
              , "const button = document.createElement('button');"
              , "shadow.appendChild(button);"
              , "const slot = document.createElement('slot');"
              , "button.appendChild(slot);"
              , "const span = document.createElement('span');"
              , "span.textContent = 'pre';"
              , "slot.appendChild(span);"
              , "})();"
              , "</script>"
              ])
          checkRoleName page "button" ("button", "foo")

          -- Nothing is assigned to the slot, should use slot content.
          setContent
            page
            (htmlLines
              [ "<div></div>"
              , "<script>"
              , "(() => {"
              , "const container = document.querySelector('div');"
              , "const shadow = container.attachShadow({ mode: 'open' });"
              , "const button = document.createElement('button');"
              , "shadow.appendChild(button);"
              , "const slot = document.createElement('slot');"
              , "button.appendChild(slot);"
              , "const span = document.createElement('span');"
              , "span.textContent = 'pre';"
              , "slot.appendChild(span);"
              , "})();"
              , "</script>"
              ])
          checkRoleName page "button" ("button", "pre")
    , testCase "accessible name nested treeitem" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<div role=treeitem id=target>"
              , "<span>Top-level</span>"
              , "<div role=group>"
              , "<div role=treeitem><span>Nested 1</span></div>"
              , "<div role=treeitem><span>Nested 2</span></div>"
              , "</div>"
              , "</div>"
              ])
          checkRoleName page "#target" ("treeitem", "Top-level")
    , testCase "svg title" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<div>"
              , "<svg width=\"162\" height=\"30\" viewBox=\"0 0 162 30\" fill=\"none\" xmlns=\"http://www.w3.org/2000/svg\">"
              , "<title>Submit</title>"
              , "<g>"
              , "<title>Hello</title>"
              , "</g>"
              , "<a href=\"example.com\" xlink:title=\"a link\"><circle cx=\"50\" cy=\"40\" r=\"35\" /></a>"
              , "</svg>"
              , "</div>"
              ])
          checkRoleName page "svg" ("img", "Submit")
          -- Upstream also expects the role-less g element to be named "Hello"
          -- (skipped subcase): ariaSnapshot prints no accessible name for it.
          checkRoleName page "a" ("link", "a link")
    , testCase "native controls" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<label for=\"text1\">TEXT1</label><input id=\"text1\" type=text>"
              , "<input id=\"text2\" type=text title=\"TEXT2\">"
              , "<input id=\"text3\" type=text placeholder=\"TEXT3\">"
              , "<input id=\"number1\" type=number placeholder=\"NUMBER1\">"
              , "<label for=\"image1\">IMAGE1</label><input id=\"image1\" type=image>"
              , "<input id=\"image2\" type=image alt=\"IMAGE2\">"
              , "<label for=\"image3\">IMAGE3</label><input id=\"image3\" type=image alt=\"MORE3\">"
              , "<label for=\"button1\">BUTTON1</label><button id=\"button1\" role=\"combobox\">button</button>"
              , "<button id=\"button2\" role=\"combobox\">BUTTON2</button>"
              , "<button id=\"button3\">BUTTON3</button>"
              , "<button id=\"button4\" title=\"BUTTON4\"></button>"
              , "<input id=\"file1\" type=file>"
              , "<label for=\"file2\">FILE2</label><input id=\"file2\" type=file>"
              ])
          checkRoleName page "#text1" ("textbox", "TEXT1")
          checkRoleName page "#text2" ("textbox", "TEXT2")
          checkRoleName page "#text3" ("textbox", "TEXT3")
          checkRoleName page "#number1" ("spinbutton", "NUMBER1")
          checkRoleName page "#image1" ("button", "IMAGE1")
          checkRoleName page "#image2" ("button", "IMAGE2")
          checkRoleName page "#image3" ("button", "IMAGE3")
          checkRoleName page "#button1" ("combobox", "BUTTON1")
          checkRoleName page "#button2" ("combobox", "")
          checkRoleName page "#button3" ("button", "BUTTON3")
          checkRoleName page "#button4" ("button", "BUTTON4")
          checkRoleName page "#file1" ("button", "Choose File")
          checkRoleName page "#file2" ("button", "FILE2")
    , testCase "input type=search maps to searchbox unless list points at a datalist" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<input id=\"search1\" type=search>"
              , "<input id=\"search2\" type=search list=nope>"
              , "<input id=\"search3\" type=search list=dv><div id=dv></div>"
              , "<input id=\"search4\" type=search list=dl><datalist id=dl></datalist>"
              ])
          checkRoleName page "#search1" ("searchbox", "")
          checkRoleName page "#search2" ("searchbox", "")
          checkRoleName page "#search3" ("searchbox", "")
          checkRoleName page "#search4" ("combobox", "")
    , testCase "meter and progress get their name from an associated label" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<label for=\"meter1\">Battery</label><meter id=\"meter1\" value=0.5></meter>"
              , "<label for=\"progress1\">Loading</label><progress id=\"progress1\" value=0.3></progress>"
              , "<label>Charge <meter id=\"meter2\" value=0.5></meter></label>"
              , "<label for=\"meter3\">Ignored</label><meter id=\"meter3\" aria-label=\"Overridden\" value=0.5></meter>"
              ])
          checkRoleName page "#meter1" ("meter", "Battery")
          checkRoleName page "#progress1" ("progressbar", "Loading")
          checkRoleName page "#meter2" ("meter", "Charge")
          checkRoleName page "#meter3" ("meter", "Overridden")
    , testCase "native controls labelled-by" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<label id=\"for-text1\">TEXT1</label><input aria-labelledby=\"for-text1\" id=\"text1\" type=text>"
              , "<label id=\"for-text2\">TEXT2</label><input aria-labelledby=\"for-text2 text2\" id=\"text2\" type=text>"
              , "<label id=\"for-text3\" for=\"text3\">TEXT3</label><input aria-labelledby=\"for-text3 text3\" id=\"text3\" type=text>"
              , "<label id=\"for-submit1\" for=\"submit1\">SUBMIT1</label><input aria-labelledby=\"for-submit1 submit1\" id=\"submit1\" type=submit>"
              , "<label id=\"for-image1\" for=\"image1\">IMAGE1</label><input aria-labelledby=\"for-image1 image1\" id=\"image1\" type=image alt=\"MORE1\">"
              , "<label id=\"for-image2\" for=\"image2\">IMAGE2</label><img aria-labelledby=\"for-image2 image2\" id=\"image2\" alt=\"MORE2\" src=\"data:image/svg,<g></g>\">"
              , "<label id=\"for-button1\">BUTTON1</label><button aria-labelledby=\"for-button1\" id=\"button1\">MORE1</button>"
              , "<label id=\"for-button2\">BUTTON2</label><button aria-labelledby=\"for-button2 button2\" id=\"button2\">MORE2</button>"
              , "<label id=\"for-button3\" for=\"button3\">BUTTON3</label><button aria-labelledby=\"for-button3 button3\" id=\"button3\">MORE3</button>"
              , "<label id=\"for-button4\" for=\"button4\">BUTTON4</label><button aria-labelledby=\"for-button4\" id=\"button4\">MORE4</button>"
              , "<label id=\"for-textarea1\" for=\"textarea1\">TEXTAREA1</label><textarea aria-labelledby=\"for-textarea1 textarea1\" id=\"textarea1\" placeholder=\"MORE1\">MORE2</textarea>"
              ])
          checkRoleName page "#text1" ("textbox", "TEXT1")
          checkRoleName page "#text2" ("textbox", "TEXT2")
          checkRoleName page "#text3" ("textbox", "TEXT3")
          checkRoleName page "#submit1" ("button", "SUBMIT1 Submit")
          checkRoleName page "#image1" ("button", "IMAGE1 MORE1")
          checkRoleName page "#image2" ("img", "IMAGE2 MORE2")
          checkRoleName page "#button1" ("button", "BUTTON1")
          checkRoleName page "#button2" ("button", "BUTTON2 MORE2")
          checkRoleName page "#button3" ("button", "BUTTON3 MORE3")
          checkRoleName page "#button4" ("button", "BUTTON4")
          checkRoleName page "#textarea1" ("textbox", "TEXTAREA1 MORE2")
    , testCase "display:contents should be visible when contents are visible" $
        withChromiumPage $ \page -> do
          setContent page "<button style='display: contents;'>yo</button>"
          count <- locatorCount (getByRole page (AriaRole "button") defaultGetByRoleOptions)
          assertEqual "button count" 1 count
    , testCase "should remove soft hyphens and zero-width spaces" $
        withChromiumPage $ \page -> do
          setContent page "<button>1\x00ad\&2\x200b\&3</button>"
          checkRoleName page "button" ("button", "123")
    , testCase "label/labelled-by aria-hidden with descendants" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<body>"
              , "<div id=\"case1\">"
              , "<button aria-labelledby=\"label1\" type=\"button\"></button>"
              , "<tool-tip id=\"label1\" for=\"button-preview\" popover=\"manual\" aria-hidden=\"true\" role=\"tooltip\">Label1</tool-tip>"
              , "</div>"
              , "<div id=\"case2\">"
              , "<label for=\"button2\" aria-hidden=\"true\"><div id=\"label2\">Label2</div></label>"
              , "<button id=\"button2\" type=\"button\"></button>"
              , "</div>"
              , "</body>"
              , "<script>"
              , "for (const el of document.querySelectorAll('#label1, #label2')) {"
              , "el.attachShadow({ mode: 'open' }).appendChild(document.createElement('slot'));"
              , "}"
              , "</script>"
              ])
          checkRoleName page "#case1 button" ("button", "Label1")
          checkRoleName page "#case2 button" ("button", "Label2")
    , testCase "own aria-label concatenated with aria-labelledby" $
        withChromiumPage $ \page -> do
          -- This is taken from https://w3c.github.io/accname/#example-5-0
          setContent
            page
            (htmlLines
              [ "<h1>Files</h1>"
              , "<ul>"
              , "<li>"
              , "<a id=\"file_row1\" href=\"./files/Documentation.pdf\">Documentation.pdf</a>"
              , "<span role=\"button\" tabindex=\"0\" id=\"del_row1\" aria-label=\"Delete\" aria-labelledby=\"del_row1 file_row1\"></span>"
              , "</li>"
              , "<li>"
              , "<a id=\"file_row2\" href=\"./files/HolidayLetter.pdf\">HolidayLetter.pdf</a>"
              , "<span role=\"button\" tabindex=\"0\" id=\"del_row2\" aria-label=\"Delete\" aria-labelledby=\"del_row2 file_row2\"></span>"
              , "</li>"
              , "</ul>"
              ])
          checkRoleName page "#del_row1" ("button", "Delete Documentation.pdf")
          checkRoleName page "#del_row2" ("button", "Delete HolidayLetter.pdf")
    , testCase "control embedded in a label" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<label for=\"flash\">"
              , "<input type=\"checkbox\" id=\"flash\">"
              , "Flash the screen <span tabindex=\"0\" role=\"textbox\" aria-label=\"number of times\" contenteditable>5</span> times."
              , "</label>"
              ])
          checkRoleName page "input" ("checkbox", "Flash the screen 5 times.")
          checkRoleName page "span" ("textbox", "number of times")
          -- Upstream expects the role-less label to have an empty name (skipped subcase):
          -- ariaSnapshot prints the label's text content instead of an accessible name.
    , testCase "control embedded in a target element" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<h1>"
              , "<input type=\"text\" value=\"Foo bar\">"
              , "</h1>"
              ])
          checkRoleName page "h1" ("heading", "Foo bar")
    , testCase "searchbox embedded control should contribute its value" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<button id=\"b1\" aria-labelledby=\"l1\"></button><div id=\"l1\" hidden><input type=\"text\" value=\"Query\"></div>"
              , "<button id=\"b2\" aria-labelledby=\"l2\"></button><div id=\"l2\" hidden><input type=\"search\" value=\"Query\"></div>"
              , "<label for=\"c1\">Flash the screen <input type=\"search\" value=\"5\"> times.</label>"
              , "<input type=\"checkbox\" id=\"c1\">"
              , "<h1><input type=\"search\" value=\"Foo bar\"></h1>"
              ])
          checkRoleName page "#b1" ("button", "Query")
          checkRoleName page "#b2" ("button", "Query")
          checkRoleName page "#c1" ("checkbox", "Flash the screen 5 times.")
          checkRoleName page "h1" ("heading", "Foo bar")
    , testCase "svg role=presentation" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<img src=\"pptr.png\" alt=\"Code is Poetry.\" />"
              , "<svg viewBox=\"0 0 100 100\" width=\"16\" height=\"16\" xmlns=\"http://www.w3.org/2000/svg\" role=\"presentation\" focusable=\"false\"><circle cx=\"50\" cy=\"50\" r=\"50\"></circle></svg>"
              ])
          checkRoleName page "img" ("img", "Code is Poetry.")
          -- Upstream expects role "presentation" with an empty name (skipped subcase):
          -- ariaSnapshot omits presentation elements, so only their absence is checked.
          svgSnapshot <- ariaSnapshot (locate page "svg")
          assertEqual "svg is omitted from the snapshot" "" svgSnapshot
    , testCase "should work with form and tricky input names" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<form aria-label=\"my form\">"
              , "<input name=\"tagName\" value=\"hello\" title=\"tagName input\">"
              , "<input name=\"localName\" value=\"hello\" title=\"localName input\">"
              , "</form>"
              ])
          checkRoleName page "form" ("form", "my form")
    , testCase "should ignore stylesheet from hidden aria-labelledby subtree" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<div id=mylabel style=\"display:none\">"
              , "<template shadowrootmode=open>"
              , "<style>span { color: red; }</style>"
              , "<span>hello</span>"
              , "</template>"
              , "</div>"
              , "<input aria-labelledby=mylabel type=text>"
              ])
          checkRoleName page "input" ("textbox", "hello")
    , testCase "should not include hidden pseudo into accessible name" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<style>"
              , "span:before {"
              , "content: 'world';"
              , "display: none;"
              , "}"
              , "div:after {"
              , "content: 'bye';"
              , "visibility: hidden;"
              , "}"
              , "</style>"
              , "<a href=\"http://example.com\">"
              , "<span>hello</span>"
              , "<div>hello</div>"
              , "</a>"
              ])
          checkRoleName page "a" ("link", "hello hello")
    , testCase "should resolve pseudo content from attr" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<style>"
              , ".stars:before {"
              , "display: block;"
              , "content: attr(data-hello);"
              , "}"
              , "</style>"
              , "<a href=\"http://example.com\">"
              , "<div class=\"stars\" data-hello=\"hello\">world</div>"
              , "</a>"
              ])
          checkRoleName page "a" ("link", "hello world")
    , testCase "should resolve pseudo content alternative text" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<style>"
              , ".with-content:before {"
              , "content: url(\"data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg'></svg>\") / \"alternative text\";"
              , "}"
              , "</style>"
              , "<div role=\"button\" class=\"with-content\"> inner text</div>"
              ])
          checkRoleName page "div" ("button", "alternative text inner text")
    , testCase "should resolve css content property for an element" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<style>"
              , ".with-content-1 {"
              , "content: url(\"data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg'></svg>\") / \"alternative text\";"
              , "}"
              , ".with-content-2 {"
              , "content: url(\"data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg'></svg>\");"
              , "}"
              , "</style>"
              , "<div id=\"button1\" role=\"button\" class=\"with-content-1\">inner text</div>"
              , "<div id=\"button2\" role=\"button\" class=\"with-content-2\">inner text</div>"
              ])
          checkRoleName page "#button1" ("button", "alternative text")
          checkRoleName page "#button2" ("button", "inner text")
    , testCase "should ignore invalid aria-labelledby" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<label>"
              , "<span>Text here</span>"
              , "<input type=text aria-labelledby=\"does-not-exist\">"
              , "</label>"
              ])
          checkRoleName page "input" ("textbox", "Text here")
    , testCase "should support search element" $
        withChromiumPage $ \page -> do
          setContent
            page
            (htmlLines
              [ "<search id=search1 aria-label=\"example\">"
              , "Hello"
              , "</search>"
              , "<search id=search2>"
              , "World"
              , "</search>"
              ])
          checkRoleName page "#search1" ("search", "example")
          checkRoleName page "#search2" ("search", "")
          visible <-
            isVisible
              ( getByRole
                  page
                  (AriaRole "search")
                  defaultGetByRoleOptions {getByRoleName = Just "example", getByRoleExact = Exact}
              )
          assertBool "search with name example is visible" visible
    ]


-- Asserts the role and quoted accessible name shown by the first ariaSnapshot line.
checkRoleName :: Page -> Text -> (Text, Text) -> Playwright ()
checkRoleName page selector expected = do
  snapshot <- ariaSnapshot (locate page selector)
  assertEqual ("role and name of " <> T.unpack selector) expected (roleAndName snapshot)

-- Parses the first ariaSnapshot line, e.g. `- button "Hello":` into (role, name).
roleAndName :: Text -> (Text, Text)
roleAndName snapshot =
  case T.lines snapshot of
    (firstLine : _) ->
      let body = T.dropWhile (== ' ') (T.drop 2 (T.stripStart firstLine))
          role = T.takeWhile (\c -> c /= ' ' && c /= ':') body
          rest = T.drop (T.length role) body
          name =
            case T.splitOn "\"" rest of
              (_ : quoted : _) -> quoted
              _ -> ""
       in (role, name)
    [] -> ("", "")

htmlLines :: [Text] -> Text
htmlLines = T.unlines
