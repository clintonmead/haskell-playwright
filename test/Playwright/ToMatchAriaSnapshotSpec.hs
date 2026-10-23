module Playwright.ToMatchAriaSnapshotSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import qualified Data.Text as Text
import Playwright
  ( Page
  , Playwright
  , defaultExpectOptions
  , expectIsNot
  , expectLocatorAriaSnapshot
  , expectPageAriaSnapshot
  , expectTimeout
  , setContent
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( assertBool
  , assertProtocolContains
  , locate
  , withChromiumPage
  )
import Playwright.Timeout
  ( milliseconds
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( try
  )

-- Official cases that stay skipped:
--
-- * "expected formatter", "should report error in YAML",
--   "should report error in YAML keys", "should not unshift actual template
--   text", "should not match what is not matched", "top-level deep-equal",
--   "call log should contain actual snapshot": each asserts the exact text of
--   the client-formatted failure message (header, Timeout line, diff, call
--   log, parser error). The Haskell client does not parse errorDetails and
--   does not build that message, so there is nothing to compare against.
--
-- Partially ported cases: where a case mixes a pass or fail check with a
-- message-text check, the pass or fail check is kept (with assertFails) and
-- the message-text assertion is dropped for the same reason.

tests :: TestTree
tests =
  testGroup
    "to-match-aria-snapshot"
    [ testCase "should match page" $
        withChromiumPage $ \page -> do
          setContent page "<h1>title</h1>"
          matchPage page "- heading \"title\""
    , testCase "should match page complex" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<h1>Microsoft</h1>"
                , "<div>Open source projects and samples from Microsoft</div>"
                , "<ul>"
                , "  <li>"
                , "    <a href=\"about:blank\">Playwright</a>"
                , "  </li>"
                , "</ul>"
                ]
            )
          matchPage
            page
            ( lns
                [ "- heading \"Microsoft\""
                , "- text: Open source projects and samples from Microsoft"
                , "- list:"
                , "  - listitem:"
                , "    - link \"Playwright\""
                ]
            )
    , testCase "should match page with not" $
        withChromiumPage $ \page -> do
          setContent page "<h1>title</h1>"
          notMatchPage page "- heading \"wrong\""
    , testCase "should fail page with timeout" $
        withChromiumPage $ \page -> do
          setContent page "<h1>title</h1>"
          failsWithin 2000 page "- heading \"wrong\""
    , testCase "should match" $
        withChromiumPage $ \page -> do
          setContent page "<h1>title</h1>"
          matchPage page "- heading \"title\""
    , testCase "should match in list" $
        withChromiumPage $ \page -> do
          setContent page (lns ["<h1>title</h1>", "<h1>title 2</h1>"])
          matchPage page "- heading \"title\""
    , testCase "should match list with accessible name" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<ul aria-label=\"my list\">"
                , "  <li>one</li>"
                , "  <li>two</li>"
                , "</ul>"
                ]
            )
          matchPage
            page
            ( lns
                [ "- list \"my list\":"
                , "  - listitem: \"one\""
                , "  - listitem: \"two\""
                ]
            )
    , testCase "should match deep item" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<div>"
                , "  <h1>title</h1>"
                , "  <h1>title 2</h1>"
                , "</div>"
                ]
            )
          matchPage page "- heading \"title\""
    , testCase "should match complex" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<ul>"
                , "  <li>"
                , "    <a href='about:blank'>link</a>"
                , "  </li>"
                , "</ul>"
                ]
            )
          matchPage
            page
            ( lns
                [ "- list:"
                , "  - listitem:"
                , "    - link \"link\""
                ]
            )
    , testCase "should match regex" $
        withChromiumPage $ \page -> do
          setContent page "<h1>Issues 12</h1>"
          matchPage page "- heading /Issues \\d+/"
          setContent page "<h1>Issues 1/2</h1>"
          matchPage page "- heading /Issues 1[/]2/"
          setContent page "<h1>Issues 1[</h1>"
          matchPage page "- heading /Issues 1\\[/"
          setContent page "<h1>Issues 1]]2</h1>"
          matchPage page "- heading /Issues 1[\\]]]2/"
    , testCase "should allow text nodes" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<h1>Microsoft</h1>"
                , "<div>Open source projects and samples from Microsoft</div>"
                ]
            )
          matchPage
            page
            ( lns
                [ "- heading \"Microsoft\""
                , "- text: \"Open source projects and samples from Microsoft\""
                ]
            )
    , testCase "details visibility" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<details>"
                , "  <summary>Summary</summary>"
                , "  <div>Details</div>"
                , "</details>"
                ]
            )
          matchPage page "- group: \"Summary\""
    , testCase "checked attribute" $
        withChromiumPage $ \page -> do
          setContent page "<input type='checkbox' checked />"
          matchPage page "- checkbox"
          matchPage page "- checkbox [checked]"
          matchPage page "- checkbox [checked=true]"
          failsWithin 1000 page "- checkbox [checked=false]"
          failsWithin 1000 page "- checkbox [checked=mixed]"
          assertProtocolContains
            " attribute must be a boolean or \"mixed\""
            (expectPageWithin 1000 page "- checkbox [checked=5]")
    , testCase "disabled attribute" $
        withChromiumPage $ \page -> do
          setContent page "<button disabled>Click me</button>"
          matchPage page "- button"
          matchPage page "- button [disabled]"
          matchPage page "- button [disabled=true]"
          failsWithin 1000 page "- button [disabled=false]"
          assertProtocolContains
            " attribute must be a boolean"
            (expectPageWithin 1000 page "- button [disabled=invalid]")
    , testCase "expanded attribute" $
        withChromiumPage $ \page -> do
          setContent page "<button aria-expanded=\"true\">Toggle</button>"
          matchPage page "- button"
          matchPage page "- button [expanded]"
          matchPage page "- button [expanded=true]"
          failsWithin 1000 page "- button [expanded=false]"
          assertProtocolContains
            " attribute must be a boolean"
            (expectPageWithin 1000 page "- button [expanded=invalid]")
    , testCase "level attribute" $
        withChromiumPage $ \page -> do
          setContent page "<h2>Section Title</h2>"
          matchPage page "- heading"
          matchPage page "- heading [level=2]"
          failsWithin 1000 page "- heading [level=3]"
          assertProtocolContains
            " attribute must be a number"
            (expectPageWithin 1000 page "- heading [level=two]")
    , testCase "pressed attribute" $
        withChromiumPage $ \page -> do
          setContent page "<button aria-pressed=\"true\">Like</button>"
          matchPage page "- button"
          matchPage page "- button [pressed]"
          matchPage page "- button [pressed=true]"
          failsWithin 1000 page "- button [pressed=false]"
          setContent page "<button aria-pressed=\"mixed\">Like</button>"
          matchPage page "- button [pressed=mixed]"
          failsWithin 1000 page "- button [pressed=true]"
          assertProtocolContains
            " attribute must be a boolean or \"mixed\""
            (expectPageWithin 1000 page "- button [pressed=5]")
    , testCase "selected attribute" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<table>"
                , "  <tr aria-selected=\"true\">"
                , "    <td>Row</td>"
                , "  </tr>"
                , "  <tr>"
                , "    <td>Row 2</td>"
                , "  </tr>"
                , "</table>"
                ]
            )
          matchPage page (lns ["- row", "- row"])
          matchPage page (lns ["- row [selected]", "- row"])
          matchPage page (lns ["- row [selected=true]", "- row [selected=false]"])
          matchLocator
            page
            "table"
            ( lns
                [ "- table:"
                , "  - rowgroup:"
                , "    - row [selected=true]"
                , "    - row [selected=false]"
                ]
            )
          failsWithin 1000 page (lns ["- row [selected=false]", "- row [selected=false]"])
          assertProtocolContains
            " attribute must be a boolean"
            (expectPageWithin 1000 page "- row [selected=invalid]")
    , testCase "integration test" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<h1>Microsoft</h1>"
                , "<div>Open source projects and samples from Microsoft</div>"
                , "<ul>"
                , "  <li>"
                , "    <details>"
                , "      <summary>"
                , "        Verified"
                , "      </summary>"
                , "      <div>"
                , "        <div>"
                , "          <p>"
                , "            We've verified that the organization <strong>microsoft</strong> controls the domain:"
                , "          </p>"
                , "          <ul>"
                , "            <li class=\"mb-1\">"
                , "              <strong>opensource.microsoft.com</strong>"
                , "            </li>"
                , "          </ul>"
                , "          <div>"
                , "            <a href=\"about: blank\">Learn more about verified organizations</a>"
                , "          </div>"
                , "        </div>"
                , "      </div>"
                , "    </details>"
                , "  </li>"
                , "  <li>"
                , "    <a href=\"about:blank\">"
                , "      <summary title=\"Label: GitHub Sponsor\">Sponsor</summary>"
                , "    </a>"
                , "  </li>"
                , "</ul>"
                ]
            )
          matchPage
            page
            ( lns
                [ "- heading \"Microsoft\""
                , "- text: Open source projects and samples from Microsoft"
                , "- list:"
                , "  - listitem:"
                , "    - group: Verified"
                , "  - listitem:"
                , "    - link \"Sponsor\""
                ]
            )
    , testCase "integration test 2" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<div>"
                , "  <header>"
                , "    <h1>todos</h1>"
                , "    <input placeholder=\"What needs to be done?\">"
                , "  </header>"
                , "</div>"
                ]
            )
          matchPage
            page
            ( lns
                [ "- heading \"todos\""
                , "- textbox \"What needs to be done?\""
                ]
            )
    , testCase "should unpack escaped names" $
        withChromiumPage $ \page -> do
          setContent page "<button>Click: me</button>"
          matchPage page "- 'button \"Click: me\"'"
          matchPage page "- 'button /Click: me/'"
          setContent page "<button>Click / me</button>"
          matchPage page "- button \"Click / me\""
          matchPage page "- button /Click \\/ me/"
          matchPage page "- 'button /Click \\/ me/'"
          setContent page "<button>Click \" me</button>"
          matchPage page "- button \"Click \\\" me\""
          matchPage page "- button /Click \" me/"
          matchPage page "- button /Click \\\" me/"
          setContent page "<button>Click \\ me</button>"
          matchPage page "- button \"Click \\\\ me\""
          matchPage page "- button /Click \\\\ me/"
          matchPage page "- 'button /Click \\\\ me/'"
          setContent page "<button>Click ' me</button>"
          matchPage page "- 'button \"Click '' me\"'"
          setContent page "<h1>heading \"name\" [level=1]</h1>"
          matchPage page "- heading \"heading \\\"name\\\" [level=1]\" [level=1]"
          setContent page "<h1>heading \\\" [level=2]</h1>"
          matchPage
            page
            ( lns
                [ "- |"
                , "    heading    \"heading \\\\\\\" [level=2]\" ["
                , "       level  =   1   ]"
                ]
            )
    , testCase "should match url" $
        withChromiumPage $ \page -> do
          setContent page "<a href='https://example.com'>Link</a>"
          matchPage
            page
            ( lns
                [ "- link:"
                , "  - /url: /.*example.com/"
                ]
            )
    , testCase "should parse attributes" $
        withChromiumPage $ \page -> do
          setContent page "<button aria-pressed=\"mixed\">hello world</button>"
          matchPage page "- button [pressed=mixed ]"
          setContent page "<h2>hello world</h2>"
          notMatchLocator page "body" "- heading [level =  -3 ]"
    , testCase "should allow restoring contain mode inside deep-equal" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<ul>"
                , "  <li>"
                , "    <ul>"
                , "      <li>1.1</li>"
                , "      <li>1.2</li>"
                , "    </ul>"
                , "  </li>"
                , "</ul>"
                ]
            )
          failsWithin
            1000
            page
            ( lns
                [ "- list:"
                , "  - /children: deep-equal"
                , "  - listitem:"
                , "    - list:"
                , "      - listitem: 1.1"
                ]
            )
          matchPage
            page
            ( lns
                [ "- list:"
                , "  - /children: deep-equal"
                , "  - listitem:"
                , "    - list:"
                , "      - /children: contain"
                , "      - listitem: 1.1"
                ]
            )
    , testCase "should detect unexpected children: equal" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<ul>"
                , "  <li>One</li>"
                , "  <li>Two</li>"
                , "  <li>Three</li>"
                , "</ul>"
                ]
            )
          matchPage
            page
            ( lns
                [ "- list:"
                , "  - listitem: \"One\""
                , "  - listitem: \"Three\""
                ]
            )
          failsWithin
            1000
            page
            ( lns
                [ "- list:"
                , "  - /children: equal"
                , "  - listitem: \"One\""
                , "  - listitem: \"Three\""
                ]
            )
    , testCase "should detect unexpected children: deep-equal" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<ul>"
                , "  <li>"
                , "    <ul>"
                , "      <li>1.1</li>"
                , "      <li>1.2</li>"
                , "    </ul>"
                , "  </li>"
                , "</ul>"
                ]
            )
          matchPage
            page
            ( lns
                [ "- list:"
                , "  - listitem:"
                , "    - list:"
                , "      - listitem: 1.1"
                ]
            )
          matchPage
            page
            ( lns
                [ "- list:"
                , "  - /children: equal"
                , "  - listitem:"
                , "    - list:"
                , "      - listitem: 1.1"
                ]
            )
          failsWithin
            1000
            page
            ( lns
                [ "- list:"
                , "  - /children: deep-equal"
                , "  - listitem:"
                , "    - list:"
                , "      - listitem: 1.1"
                ]
            )
    , testCase "generated snapshot includes text children when name is longer than text" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<section>"
                , "  <div role=\"progressbar\" aria-label=\"Alpha Beta\" aria-valuenow=\"7\" aria-valuemin=\"0\" aria-valuemax=\"10\">"
                , "    <span>Alpha</span>"
                , "    <span>7</span>"
                , "  </div>"
                , "</section>"
                ]
            )
          matchLocator
            page
            "section"
            ( lns
                [ "- /children: deep-equal"
                , "- progressbar \"Alpha Beta\": Alpha 7"
                ]
            )
    , testCase "treat bad regex as a string" $
        withChromiumPage $ \page -> do
          setContent page "<a href=\"/foo\">Log in</a>"
          failsWithin
            1
            page
            ( lns
                [ "- link \"Log in\":"
                , "  - /url: /[a/"
                ]
            )
    , testCase "invalid attribute" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( lns
                [ "<input type=\"text\" aria-label=\"Email\" aria-invalid=\"true\" value=\"not-an-email\">"
                , "<input type=\"text\" aria-label=\"Name\" value=\"Alice\">"
                ]
            )
          matchPage
            page
            ( lns
                [ "- textbox \"Email\" [invalid]: not-an-email"
                , "- textbox \"Name\": Alice"
                ]
            )
          matchPage
            page
            ( lns
                [ "- textbox \"Email\" [invalid=true]: not-an-email"
                , "- textbox \"Name\" [invalid=false]: Alice"
                ]
            )
          setContent
            page
            ( lns
                [ "<input type=\"text\" aria-label=\"Bio\" aria-invalid=\"grammar\">"
                , "<input type=\"text\" aria-label=\"Note\" aria-invalid=\"spelling\">"
                ]
            )
          matchPage
            page
            ( lns
                [ "- textbox \"Bio\" [invalid=grammar]"
                , "- textbox \"Note\" [invalid=spelling]"
                ]
            )
          failsWithin 1 page "- textbox \"Bio\" [invalid]"
          setContent page "<input type=\"text\" aria-label=\"Zip\" aria-invalid=\"garbage\">"
          matchPage page "- textbox \"Zip\" [invalid]"
    ]

-- | Match a page against an aria template, with the default timeout.
matchPage :: Page -> Text -> Playwright ()
matchPage page =
  expectPageAriaSnapshot defaultExpectOptions page

-- | The negated page match.
notMatchPage :: Page -> Text -> Playwright ()
notMatchPage page =
  expectPageAriaSnapshot defaultExpectOptions {expectIsNot = True} page

-- | A page match with an explicit timeout in milliseconds.
expectPageWithin :: Double -> Page -> Text -> Playwright ()
expectPageWithin ms page =
  expectPageAriaSnapshot defaultExpectOptions {expectTimeout = milliseconds ms} page

-- | A page match that must fail within the given timeout.
failsWithin :: Double -> Page -> Text -> Playwright ()
failsWithin ms page template =
  assertFails (expectPageWithin ms page template)

-- | A locator match against a CSS selector.
matchLocator :: Page -> Text -> Text -> Playwright ()
matchLocator page selector =
  expectLocatorAriaSnapshot defaultExpectOptions (locate page selector)

-- | The negated locator match against a CSS selector.
notMatchLocator :: Page -> Text -> Text -> Playwright ()
notMatchLocator page selector =
  expectLocatorAriaSnapshot defaultExpectOptions {expectIsNot = True} (locate page selector)

assertFails :: Playwright () -> Playwright ()
assertFails action = do
  outcome <- try action :: Playwright (Either PlaywrightError ())
  assertBool "expectation should fail" (either (const True) (const False) outcome)

-- | Join template lines with newlines.
lns :: [Text] -> Text
lns = Text.intercalate "\n"
