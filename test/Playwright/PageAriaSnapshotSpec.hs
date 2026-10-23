-- Ports upstream tests/page/page-aria-snapshot.spec.ts.
--
-- Each upstream check pairs `ariaSnapshot()` with `toMatchAriaSnapshot()`.
-- Only the `ariaSnapshot()` half is ported here; the matcher half belongs to
-- the toMatchAriaSnapshot slice. Cases whose upstream assertion is only
-- `toContainYaml` are approximated by line containment (`yamlContainsLines`).
module Playwright.PageAriaSnapshotSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Data.Text qualified as T
import Network.HTTP.Types
  ( hContentType
  , status200
  , status404
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Playwright
  ( AriaSnapshotOptions (ariaSnapshotOptionsBoxes, ariaSnapshotOptionsDepth, ariaSnapshotOptionsTimeout)
  , Locator
  , Playwright
  , ariaSnapshot
  , ariaSnapshotWith
  , childLocator
  , defaultAriaSnapshotOptions
  , defaultGotoOptions
  , evaluate
  , first
  , frameLocator
  , frames
  , goto
  , locatorOnFrame
  , pageAriaSnapshot
  , pageAriaSnapshotWith
  , selectorFromText
  , setContent
  , urlFromText
  , waitForTimeout
  )
import Playwright.AriaSnapshotSupport
  ( textLines
  , yamlContainsLines
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , locate
  , requireTimeout
  , withAssets
  , withChromiumPage
  , withLocalApp
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )

tests :: TestTree
tests =
  testGroup
    "page-aria-snapshot"
    [ testCase "should snapshot" $
        withChromiumPage $ \page -> do
          setContent page "<h1>title</h1>"
          checkSnapshot (locate page "body") ["- heading \"title\" [level=1]"]
    , testCase "should snapshot list" $
        withChromiumPage $ \page -> do
          setContent page (textLines ["<h1>title</h1>", "<h1>title 2</h1>"])
          checkSnapshot
            (locate page "body")
            [ "- heading \"title\" [level=1]"
            , "- heading \"title 2\" [level=1]"
            ]
    , testCase "should snapshot list with accessible name" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<ul aria-label=\"my list\">"
                , "  <li>one</li>"
                , "  <li>two</li>"
                , "</ul>"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- list \"my list\":"
            , "  - listitem: one"
            , "  - listitem: two"
            ]
    , testCase "should snapshot complex" $
        withChromiumPage $ \page -> do
          setContent page "<ul><li><a href='about:blank'>link</a></li></ul>"
          checkSnapshot
            (locate page "body")
            [ "- list:"
            , "  - listitem:"
            , "    - link \"link\":"
            , "      - /url: about:blank"
            ]
    , testCase "should allow text nodes" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<h1>Microsoft</h1>"
                , "<div>Open source projects and samples from Microsoft</div>"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- heading \"Microsoft\" [level=1]"
            , "- text: Open source projects and samples from Microsoft"
            ]
    , testCase "should snapshot details visibility" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<details>"
                , "  <summary>Summary</summary>"
                , "  <div>Details</div>"
                , "</details>"
                ]
            )
          checkSnapshot (locate page "body") ["- group: Summary"]
    , testCase "should snapshot integration" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
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
          checkSnapshot
            (locate page "body")
            [ "- heading \"Microsoft\" [level=1]"
            , "- text: Open source projects and samples from Microsoft"
            , "- list:"
            , "  - listitem:"
            , "    - group: Verified"
            , "  - listitem:"
            , "    - link \"Sponsor\":"
            , "      - /url: about:blank"
            ]
    , testCase "should support multiline text" $
        withChromiumPage $ \page -> do
          setContent
            page
            (textLines ["<p>", "  Line 1", "  Line 2", "  Line 3", "</p>"])
          checkSnapshot (locate page "body") ["- paragraph: Line 1 Line 2 Line 3"]
    , testCase "should concatenate span text" $
        withChromiumPage $ \page -> do
          setContent page "<span>One</span> <span>Two</span> <span>Three</span>"
          checkSnapshot (locate page "body") ["- text: One Two Three"]
    , testCase "should concatenate span text 2" $
        withChromiumPage $ \page -> do
          setContent page "<span>One </span><span>Two </span><span>Three</span>"
          checkSnapshot (locate page "body") ["- text: One Two Three"]
    , testCase "should concatenate div text with spaces" $
        withChromiumPage $ \page -> do
          setContent page "<div>One</div><div>Two</div><div>Three</div>"
          checkSnapshot (locate page "body") ["- text: One Two Three"]
    , testCase "should include pseudo in text" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<style>"
                , "  span:before {"
                , "    content: 'world';"
                , "  }"
                , "  div:after {"
                , "    content: 'bye';"
                , "  }"
                , "</style>"
                , "<a href=\"about:blank\">"
                , "  <span>hello</span>"
                , "  <div>hello</div>"
                , "</a>"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- link \"worldhello hellobye\":"
            , "  - /url: about:blank"
            ]
    , testCase "should not include hidden pseudo in text" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<style>"
                , "  span:before {"
                , "    content: 'world';"
                , "    display: none;"
                , "  }"
                , "  div:after {"
                , "    content: 'bye';"
                , "    visibility: hidden;"
                , "  }"
                , "</style>"
                , "<a href=\"about:blank\">"
                , "  <span>hello</span>"
                , "  <div>hello</div>"
                , "</a>"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- link \"hello hello\":"
            , "  - /url: about:blank"
            ]
    , testCase "should include new line for block pseudo" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<style>"
                , "  span:before {"
                , "    content: 'world';"
                , "    display: block;"
                , "  }"
                , "  div:after {"
                , "    content: 'bye';"
                , "    display: block;"
                , "  }"
                , "</style>"
                , "<a href=\"about:blank\">"
                , "  <span>hello</span>"
                , "  <div>hello</div>"
                , "</a>"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- link \"world hello hello bye\":"
            , "  - /url: about:blank"
            ]
    , testCase "should work with slots" $
        withChromiumPage $ \page -> do
          -- Text "foo" is assigned to the slot, should not be used twice.
          setContent
            page
            ( textLines
                [ "<button><div>foo</div></button>"
                , "<script>"
                , "  (() => {"
                , "    const container = document.querySelector('div');"
                , "    const shadow = container.attachShadow({ mode: 'open' });"
                , "    const slot = document.createElement('slot');"
                , "    shadow.appendChild(slot);"
                , "  })();"
                , "</script>"
                ]
            )
          checkSnapshot (locate page "body") ["- button \"foo\""]
          -- Text "foo" is assigned to the slot, should be used instead of slot content.
          setContent
            page
            ( textLines
                [ "<div>foo</div>"
                , "<script>"
                , "  (() => {"
                , "    const container = document.querySelector('div');"
                , "    const shadow = container.attachShadow({ mode: 'open' });"
                , "    const button = document.createElement('button');"
                , "    shadow.appendChild(button);"
                , "    const slot = document.createElement('slot');"
                , "    button.appendChild(slot);"
                , "    const span = document.createElement('span');"
                , "    span.textContent = 'pre';"
                , "    slot.appendChild(span);"
                , "  })();"
                , "</script>"
                ]
            )
          checkSnapshot (locate page "body") ["- button \"foo\""]
          -- Nothing is assigned to the slot, should use slot content.
          setContent
            page
            ( textLines
                [ "<div></div>"
                , "<script>"
                , "  (() => {"
                , "    const container = document.querySelector('div');"
                , "    const shadow = container.attachShadow({ mode: 'open' });"
                , "    const button = document.createElement('button');"
                , "    shadow.appendChild(button);"
                , "    const slot = document.createElement('slot');"
                , "    button.appendChild(slot);"
                , "    const span = document.createElement('span');"
                , "    span.textContent = 'pre';"
                , "    slot.appendChild(span);"
                , "  })();"
                , "</script>"
                ]
            )
          checkSnapshot (locate page "body") ["- button \"pre\""]
    , testCase "should snapshot inner text" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<div role=\"listitem\">"
                , "  <div>"
                , "    <div>"
                , "      <span title=\"a.test.ts\">a.test.ts</span>"
                , "    </div>"
                , "    <div>"
                , "      <button title=\"Run\"></button>"
                , "      <button title=\"Show source\"></button>"
                , "      <button title=\"Watch\"></button>"
                , "    </div>"
                , "  </div>"
                , "</div>"
                , "<div role=\"listitem\">"
                , "  <div>"
                , "    <div>"
                , "      <span title=\"snapshot\">snapshot</span>"
                , "    </div>"
                , "    <div class=\"ui-mode-list-item-time\">30ms</div>"
                , "    <div>"
                , "      <button title=\"Run\"></button>"
                , "      <button title=\"Show source\"></button>"
                , "      <button title=\"Watch\"></button>"
                , "    </div>"
                , "  </div>"
                , "</div>"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- listitem:"
            , "  - text: a.test.ts"
            , "  - button \"Run\""
            , "  - button \"Show source\""
            , "  - button \"Watch\""
            , "- listitem:"
            , "  - text: snapshot 30ms"
            , "  - button \"Run\""
            , "  - button \"Show source\""
            , "  - button \"Watch\""
            ]
    , testCase "should include pseudo codepoints" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent
            page
            ( textLines
                [ "<link href=\"codicon.css\" rel=\"stylesheet\" />"
                , "<p class='codicon codicon-check'>hello</p>"
                ]
            )
          checkSnapshot (locate page "body") ["- paragraph: \xeab2hello"]
    , testCase "check aria-hidden text" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<p>"
                , "  <span>hello</span>"
                , "  <span aria-hidden=\"true\">world</span>"
                , "</p>"
                ]
            )
          checkSnapshot (locate page "body") ["- paragraph: hello"]
    , testCase "should ignore presentation and none roles" $
        withChromiumPage $ \page -> do
          setContent
            page
            (textLines ["<ul>", "  <li role='presentation'>hello</li>", "  <li role='none'>world</li>", "</ul>"])
          checkSnapshot (locate page "body") ["- list: hello world"]
    , testCase "should treat input value as text in templates, but not for checkbox/radio/file" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<input value='hello world'>"
                , "<input type=file>"
                , "<input type=checkbox checked>"
                , "<input type=radio checked>"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- textbox: hello world"
            , "- button \"Choose File\""
            , "- checkbox [checked]"
            , "- radio [checked]"
            ]
    , testCase "should not use on as checkbox value" $
        withChromiumPage $ \page -> do
          setContent page (textLines ["<input type='checkbox'>", "<input type='radio'>"])
          checkSnapshot (locate page "body") ["- checkbox", "- radio"]
    , testCase "should respect aria-owns" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<a href='about:blank' aria-owns='input p'>"
                , "  <div role='region'>Link 1</div>"
                , "</a>"
                , "<a href='about:blank' aria-owns='input p'>"
                , "  <div role='region'>Link 2</div>"
                , "</a>"
                , "<input id='input' value='Value'>"
                , "<p id='p'>Paragraph</p>"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- link \"Link 1 Value Paragraph\":"
            , "  - /url: about:blank"
            , "  - region: Link 1"
            , "  - textbox: Value"
            , "  - paragraph: Paragraph"
            , "- link \"Link 2 Value Paragraph\":"
            , "  - /url: about:blank"
            , "  - region: Link 2"
            ]
    , testCase "should be ok with circular ownership" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<a href='about:blank' id='parent'>"
                , "  <div role='region' aria-owns='parent'>Hello</div>"
                , "</a>"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- link \"Hello\":"
            , "  - /url: about:blank"
            , "  - region: Hello"
            ]
    , testCase "should escape yaml text in text nodes" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<details>"
                , "  <summary>one: <a href=\"#\">link1</a> \"two <a href=\"#\">link2</a> 'three <a href=\"#\">link3</a> `four</summary>"
                , "</details>"
                , "<ul>"
                , "  <a href=\"#\">one</a>,<a href=\"#\">two</a>"
                , "  (<a href=\"#\">three</a>)"
                , "  {<a href=\"#\">four</a>}"
                , "  [<a href=\"#\">five</a>]"
                , "</ul>"
                , "<div>[Select all]</div>"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- group:"
            , "  - text: \"one:\""
            , "  - link \"link1\":"
            , "    - /url: \"#\""
            , "  - text: \"\\\"two\""
            , "  - link \"link2\":"
            , "    - /url: \"#\""
            , "  - text: \"'three\""
            , "  - link \"link3\":"
            , "    - /url: \"#\""
            , "  - text: \"`four\""
            , "- list:"
            , "  - link \"one\":"
            , "    - /url: \"#\""
            , "  - text: \",\""
            , "  - link \"two\":"
            , "    - /url: \"#\""
            , "  - text: ("
            , "  - link \"three\":"
            , "    - /url: \"#\""
            , "  - text: \") {\""
            , "  - link \"four\":"
            , "    - /url: \"#\""
            , "  - text: \"} [\""
            , "  - link \"five\":"
            , "    - /url: \"#\""
            , "  - text: \"]\""
            , "- text: \"[Select all]\""
            ]
    , testCase "should normalize whitespace" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<details>"
                , "  <summary> one  \n two <a href=\"#\"> link &nbsp;\n  1 </a> </summary>"
                , "</details>"
                , "<input value='  hello   &nbsp; world '>"
                , "<button>hello\x00ad\x200bworld</button>"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- group:"
            , "  - text: one two"
            , "  - link \"link 1\":"
            , "    - /url: \"#\""
            , "- textbox: hello world"
            , "- button \"helloworld\""
            ]
    , testCase "should handle long strings" $
        withChromiumPage $ \page -> do
          let s = T.replicate 10000 "a"
          setContent
            page
            ( textLines
                [ "<a href='about:blank'>"
                , "  <div role='region'>" <> s <> "</div>"
                , "</a>"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- link:"
            , "  - /url: about:blank"
            , "  - region: " <> s
            ]
    , testCase "should escape special yaml characters" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<a href=\"#\">@hello</a>@hello"
                , "<a href=\"#\">]hello</a>]hello"
                , "<a href=\"#\">hello"
                , "</a>"
                , "hello"
                , "<a href=\"#\">"
                , " hello</a>"
                , " hello"
                , "<a href=\"#\">#hello</a>#hello"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- link \"@hello\":"
            , "  - /url: \"#\""
            , "- text: \"@hello\""
            , "- link \"]hello\":"
            , "  - /url: \"#\""
            , "- text: \"]hello\""
            , "- link \"hello\":"
            , "  - /url: \"#\""
            , "- text: hello"
            , "- link \"hello\":"
            , "  - /url: \"#\""
            , "- text: hello"
            , "- link \"#hello\":"
            , "  - /url: \"#\""
            , "- text: \"#hello\""
            ]
    , testCase "should escape special yaml values" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<a href=\"#\">true</a>False"
                , "<a href=\"#\">NO</a>yes"
                , "<a href=\"#\">y</a>N"
                , "<a href=\"#\">on</a>Off"
                , "<a href=\"#\">null</a>NULL"
                , "<a href=\"#\">123</a>123"
                , "<a href=\"#\">-1.2</a>-1.2"
                , "<a href=\"#\">-</a>-"
                , "<input type=text value=\"555\">"
                ]
            )
          checkSnapshot
            (locate page "body")
            [ "- link \"true\":"
            , "  - /url: \"#\""
            , "- text: \"False\""
            , "- link \"NO\":"
            , "  - /url: \"#\""
            , "- text: \"yes\""
            , "- link \"y\":"
            , "  - /url: \"#\""
            , "- text: \"N\""
            , "- link \"on\":"
            , "  - /url: \"#\""
            , "- text: \"Off\""
            , "- link \"null\":"
            , "  - /url: \"#\""
            , "- text: \"NULL\""
            , "- link \"123\":"
            , "  - /url: \"#\""
            , "- text: \"123\""
            , "- link \"-1.2\":"
            , "  - /url: \"#\""
            , "- text: \"-1.2\""
            , "- link \"-\":"
            , "  - /url: \"#\""
            , "- text: \"-\""
            , "- textbox: \"555\""
            ]
    , testCase "should not report textarea textContent" $
        withChromiumPage $ \page -> do
          setContent page "<textarea>Before</textarea>"
          checkSnapshot (locate page "body") ["- textbox: Before"]
          _ <- evaluate page "() => { document.querySelector('textarea').value = 'After'; }"
          checkSnapshot (locate page "body") ["- textbox: After"]
    , testCase "should not show visible children of hidden elements" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<div style=\"visibility: hidden;\">"
                , "  <div style=\"visibility: visible;\">"
                , "    <button>Button</button>"
                , "  </div>"
                , "</div>"
                ]
            )
          assertEqual "aria snapshot" "" =<< ariaSnapshot (locate page "body")
    , testCase "should not show unhidden children of aria-hidden elements" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<div aria-hidden=\"true\">"
                , "  <div aria-hidden=\"false\">"
                , "    <button>Button</button>"
                , "  </div>"
                , "</div>"
                ]
            )
          assertEqual "aria snapshot" "" =<< ariaSnapshot (locate page "body")
    , testCase "should snapshot placeholder when different from the name" $
        withChromiumPage $ \page -> do
          setContent page "<input placeholder=\"Placeholder\">"
          snapshot <- ariaSnapshot (locate page "body")
          assertBool "contains textbox" (yamlContainsLines snapshot ["- textbox \"Placeholder\""])
          setContent page "<input placeholder=\"Placeholder\" aria-label=\"Label\">"
          labelled <- ariaSnapshot (locate page "body")
          assertBool
            "contains labelled textbox"
            (yamlContainsLines labelled ["- textbox \"Label\":", "  - /placeholder: Placeholder"])
    , testCase "match values both against regex and string" $
        withChromiumPage $ \page -> do
          setContent page "<a href=\"/auth?r=/\">Log in</a>"
          checkSnapshot
            (locate page "body")
            [ "- link \"Log in\":"
            , "  - /url: /auth?r=/"
            ]
    , testCase "auto-waits the locator and does not include iframes" $
        withChromiumPage $ \page -> do
          setContent page "<div>Hello</div>"
          withAsync (ariaSnapshot (locate page "#target")) $ \snapshotAsync -> do
            waitForTimeout page =<< requireTimeout 2000
            setContent
              page
              ( textLines
                  [ "<div id=target>"
                  , "  Hello"
                  , "  <iframe srcdoc=\"<ul><li>Item 1</li><li>Item 2</li></ul>\"></iframe>"
                  , "</div>"
                  ]
              )
            snapshot <- wait snapshotAsync
            assertBool "contains text" (yamlContainsLines snapshot ["- text: Hello"])
    , testCase "should limit depth" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<ul id=target>"
                , "  <li>item2</li>"
                , "  <li>"
                , "    <ul>"
                , "      <li>item3</li>"
                , "    </ul>"
                , "  </li>"
                , "</ul>"
                ]
            )
          snapshot <-
            ariaSnapshotWith
              (locate page "#target")
              defaultAriaSnapshotOptions {ariaSnapshotOptionsDepth = Just 1}
          assertBool
            "contains depth-limited list"
            (yamlContainsLines snapshot ["- list:", "- listitem: item2", "- listitem"])
    , testCase "should snapshot a locator inside an iframe" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<h1>Main Page</h1>"
                , "<iframe srcdoc=\"<ul><li>Item 1</li><li>Item 2</li></ul>\"></iframe>"
                ]
            )
          fs <- frames page
          case fs of
            (_ : frame : _) -> do
              snapshot <- ariaSnapshot (locatorOnFrame page frame (selectorFromText "ul"))
              assertBool
                "contains frame list"
                (yamlContainsLines snapshot ["- list:", "- listitem: Item 1", "- listitem: Item 2"])
            _ -> assertFailure "expected an iframe"
    , testCase "should include frames on frameset pages" $
        withChromiumPage $ \page ->
          withLocalApp framesetApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/frameset.html")) defaultGotoOptions
            timeout <- requireTimeout 3000
            snapshot <-
              pageAriaSnapshotWith
                page
                defaultAriaSnapshotOptions {ariaSnapshotOptionsTimeout = Just timeout}
            assertEqual "aria snapshot" "- iframe\n- iframe" snapshot
    , testCase "should snapshot a locator inside a frameset frame" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/frameset.html")) defaultGotoOptions
          checkSnapshot
            (childLocator (first (frameLocator page (selectorFromText "frame"))) (selectorFromText "body"))
            ["- text: Hi, I'm frame"]
    , testCase "should snapshot with box from page" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<button style=\"position:absolute;left:100px;top:50px;width:80px;height:40px;margin:0;padding:0;border:0;\">click</button>"
          snapshot <-
            pageAriaSnapshotWith
              page
              defaultAriaSnapshotOptions {ariaSnapshotOptionsBoxes = Just True}
          assertEqual "aria snapshot" "- button \"click\" [box=100,50,80,40]" snapshot
    , testCase "should snapshot with box from locator" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( textLines
                [ "<div style=\"position:absolute;left:10px;top:20px;width:200px;height:100px;\">"
                , "  <button style=\"position:absolute;left:5px;top:5px;width:60px;height:30px;margin:0;padding:0;border:0;\">ok</button>"
                , "</div>"
                ]
            )
          snapshot <-
            ariaSnapshotWith
              (locate page "div")
              defaultAriaSnapshotOptions {ariaSnapshotOptionsBoxes = Just True}
          assertEqual "aria snapshot" "- button \"ok\" [box=15,25,60,30]" snapshot
    , testCase "should not include box when option is omitted" $
        withChromiumPage $ \page -> do
          setContent page "<button>click</button>"
          snapshot <- pageAriaSnapshot page
          assertBool "no box" (not (T.isInfixOf "[box=" snapshot))
    ]

-- | Checks the `ariaSnapshot()` half of an upstream `checkAndMatchSnapshot`.
checkSnapshot :: Locator -> [Text] -> Playwright ()
checkSnapshot loc expected =
  assertEqual "aria snapshot" (textLines expected) =<< ariaSnapshot loc

-- | Serves /frameset.html for the frameset cases and 404s otherwise.
framesetApp :: Application
framesetApp request respond =
  case pathInfo request of
    ["frameset.html"] ->
      respond $
        responseLBS
          status200
          [(hContentType, "text/html")]
          "<frameset cols=\"50%,50%\"><frame src=\"about:blank\"><frame src=\"about:blank\"></frameset>"
    _ -> respond (responseLBS status404 [(hContentType, "text/plain")] "not found")
