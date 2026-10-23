module Playwright.SelectorsGetBySpec
  ( tests
  ) where

import Data.Text
  ( Text
  , isInfixOf
  )
import Playwright
  ( AriaRole (AriaRole)
  , Exactness (Exact, Inexact)
  , GetByRoleOptions (getByRoleExact, getByRoleName, getByRoleNameRegex)
  , Locator
  , Playwright
  , defaultGetByRoleOptions
  , evaluateOnLocatorAllTexts
  , evaluateOnLocatorText
  , getByAltText
  , getByAltTextRegex
  , getByLabel
  , getByLabelRegex
  , getByPlaceholder
  , getByPlaceholderRegex
  , getByRole
  , getByTestId
  , getByTestIdAttr
  , getByTestIdRegex
  , getByText
  , getByTextRegex
  , getByTitle
  , getByTitleRegex
  , innerText
  , locator
  , locatorCount
  , selectorFromText
  , setContent
  )
import Playwright.Harness
  ( withChromiumPage
  , assertEqual
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

idOf :: Locator -> Playwright Text
idOf loc =
  evaluateOnLocatorText loc "e => e.id"

outerHtmlsOf :: Locator -> Playwright [Text]
outerHtmlsOf loc =
  evaluateOnLocatorAllTexts loc "els => els.map(e => e.outerHTML)"

roleSetup :: Text
roleSetup =
  "<a href=\"https://playwright.dev\">issues 123</a><a href=\"https://playwright.dev\">he llo 56</a><button>Click me</button>"

issuesLink :: Text
issuesLink =
  "<a href=\"https://playwright.dev\">issues 123</a>"

heLink :: Text
heLink =
  "<a href=\"https://playwright.dev\">he llo 56</a>"

buttonHtml :: Text
buttonHtml =
  "<button>Click me</button>"

tests :: TestTree
tests =
  testGroup
    "selectors-get-by"
    [ testCase "getByTestId should work" $
        withChromiumPage $ \page -> do
          setContent page "<div><div data-testid=\"Hello\">Hello world</div></div>"
          assertEqual "text" "Hello world" =<< innerText (getByTestId page "Hello")
    , testCase "getByText exact should work" $
        withChromiumPage $ \page -> do
          setContent page "<div>Hello world</div><div>Hello</div>"
          html <- evaluateOnLocatorText (getByText page "Hello" Exact) "e => e.outerHTML"
          assertEqual "exact hello" True (">Hello</div>" `isInfixOf` html)
    , testCase "getByLabel should work" $
        withChromiumPage $ \page -> do
          setContent page "<div><label for=target>Name</label><input id=target type=text></div>"
          assertEqual "text is label" "LABEL"
            =<< evaluateOnLocatorText (getByText page "Name" Inexact) "e => e.nodeName"
          assertEqual "label is input" "INPUT"
            =<< evaluateOnLocatorText (getByLabel page "Name" Inexact) "e => e.nodeName"
    , testCase "getByLabel should work with nested elements" $
        withChromiumPage $ \page -> do
          setContent page "<label for=target>Last <span>Name</span></label><input id=target type=text>"
          mapM_
            (\loc -> assertEqual "target id" "target" =<< idOf loc)
            [ getByLabel page "last name" Inexact
            , getByLabel page "st na" Inexact
            , getByLabel page "Name" Inexact
            , getByLabel page "Last Name" Exact
            , getByLabelRegex page "/Last\\s+name/i"
            ]
          mapM_
            (\loc -> assertEqual "no match" 0 =<< locatorCount loc)
            [ getByLabel page "Last" Exact
            , getByLabel page "last name" Exact
            , getByLabel page "Name" Exact
            , getByLabel page "what?" Inexact
            , getByLabelRegex page "/last name/"
            ]
    , testCase "getByLabel should work with multiply-labelled elements" $
        withChromiumPage $ \page -> do
          setContent page "<label for=target>Name</label><input id=target type=text><label for=target>First or Last</label>"
          assertEqual "name" "target" =<< idOf (getByLabel page "Name" Inexact)
          assertEqual "first or last" "target" =<< idOf (getByLabel page "First or Last" Inexact)
    , testCase "getByLabel should work with ancestor label and multiple controls" $
        withChromiumPage $ \page -> do
          setContent page "<label>Name<button id=target>Click me</button><input type=text></label>"
          assertEqual "name" "target" =<< idOf (getByLabel page "Name" Inexact)
    , testCase "getByLabel should work with ancestor label and for" $
        withChromiumPage $ \page -> do
          setContent page "<label for=target>Name<input type=text id=nontarget></label><input type=text id=target>"
          assertEqual "name" "target" =<< idOf (getByLabel page "Name" Inexact)
    , testCase "getByLabel should work with aria-labelledby" $
        withChromiumPage $ \page -> do
          setContent page "<label id=name-label>Name</label><button aria-labelledby=name-label>Click me</button>"
          assertEqual "text" "Click me"
            =<< evaluateOnLocatorText (getByLabel page "Name" Inexact) "e => e.textContent"
    , testCase "getByLabel should prefer aria-labelledby over a native label" $
        withChromiumPage $ \page -> do
          setContent page "<label id=name-label>Name</label><label>Wrong<button aria-labelledby=name-label>Click me</button></label>"
          assertEqual "text" "Click me"
            =<< evaluateOnLocatorText (getByLabel page "Name" Inexact) "e => e.textContent"
    , testCase "getByLabel should work with aria-label" $
        withChromiumPage $ \page -> do
          setContent page "<input id=target aria-label=\"Name\">"
          assertEqual "name" "target" =<< idOf (getByLabel page "Name" Inexact)
    , testCase "getByLabel should work with empty aria-label" $
        withChromiumPage $ \page -> do
          setContent page "<label for=target>Last Name</label><input id=target type=text aria-label>"
          assertEqual "last name" "target" =<< idOf (getByLabel page "Last Name" Inexact)
    , testCase "getByLabel should prefer aria-labelledby over aria-label" $
        withChromiumPage $ \page -> do
          setContent page "<label id=other-label>Other</label><input id=target aria-label=\"Name\" aria-labelledby=other-label>"
          assertEqual "other" "target" =<< idOf (getByLabel page "Other" Inexact)
    , testCase "getByRole should work" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<a href=\"https://playwright.dev\">issues 123</a><a href=\"https://playwright.dev\">he llo 56</a><button>Click me</button>"
          buttons <-
            evaluateOnLocatorAllTexts
              (getByRole page (AriaRole "button") defaultGetByRoleOptions)
              "els => els.map(e => e.outerHTML)"
          assertEqual "button" ["<button>Click me</button>"] buttons
          links <-
            evaluateOnLocatorAllTexts
              (getByRole page (AriaRole "link") defaultGetByRoleOptions)
              "els => els.map(e => e.outerHTML)"
          assertEqual
            "links"
            [ "<a href=\"https://playwright.dev\">issues 123</a>"
            , "<a href=\"https://playwright.dev\">he llo 56</a>"
            ]
            links
    , testCase "getByRole should work with name" $
        withChromiumPage $ \page -> do
          setContent page roleSetup
          assertEqual "name issues 123" [issuesLink]
            =<< outerHtmlsOf (getByRole page (AriaRole "link") (defaultGetByRoleOptions { getByRoleName = Just "issues 123" }))
          assertEqual "name sues" [issuesLink]
            =<< outerHtmlsOf (getByRole page (AriaRole "link") (defaultGetByRoleOptions { getByRoleName = Just "sues" }))
          assertEqual "name with whitespace" [heLink]
            =<< outerHtmlsOf (getByRole page (AriaRole "link") (defaultGetByRoleOptions { getByRoleName = Just "  he    \n  llo " }))
          assertEqual "wrong role" []
            =<< outerHtmlsOf (getByRole page (AriaRole "button") (defaultGetByRoleOptions { getByRoleName = Just "issues" }))
          assertEqual "exact partial" []
            =<< outerHtmlsOf (getByRole page (AriaRole "link") (defaultGetByRoleOptions { getByRoleName = Just "sues", getByRoleExact = Exact }))
          assertEqual "exact whitespace" [heLink]
            =<< outerHtmlsOf (getByRole page (AriaRole "link") (defaultGetByRoleOptions { getByRoleName = Just "   he \n llo 56 ", getByRoleExact = Exact }))
          assertEqual "exact button" [buttonHtml]
            =<< outerHtmlsOf (getByRole page (AriaRole "button") (defaultGetByRoleOptions { getByRoleName = Just "Click me", getByRoleExact = Exact }))
    , testCase "getByRole should escape quotes and backslashes in name" $
        withChromiumPage $ \page -> do
          setContent page roleSetup
          assertEqual "unescaped" [buttonHtml]
            =<< outerHtmlsOf (getByRole page (AriaRole "button") (defaultGetByRoleOptions { getByRoleName = Just "Click me", getByRoleExact = Exact }))
          assertEqual "one backslash" []
            =<< outerHtmlsOf (getByRole page (AriaRole "button") (defaultGetByRoleOptions { getByRoleName = Just "Click \\me", getByRoleExact = Exact }))
          assertEqual "two backslashes" []
            =<< outerHtmlsOf (getByRole page (AriaRole "button") (defaultGetByRoleOptions { getByRoleName = Just "Click \\\\me", getByRoleExact = Exact }))
    , testCase "getByRole should work with v-flag name regex" $
        withChromiumPage $ \page -> do
          setContent page "<button>Click me</button><button>Submit</button>"
          assertEqual "v" 1
            =<< locatorCount (getByRole page (AriaRole "button") (defaultGetByRoleOptions { getByRoleNameRegex = Just "/Click me/v" }))
          assertEqual "iv" 1
            =<< locatorCount (getByRole page (AriaRole "button") (defaultGetByRoleOptions { getByRoleNameRegex = Just "/click me/iv" }))
          assertEqual "missing" 0
            =<< locatorCount (getByRole page (AriaRole "button") (defaultGetByRoleOptions { getByRoleNameRegex = Just "/Missing/v" }))
    , testCase "getByTestId with custom testId should work" $
        withChromiumPage $ \page -> do
          setContent page "<div><div data-my-custom-testid=\"Hello\">Hello world</div></div>"
          assertEqual "text" "Hello world" =<< innerText (getByTestIdAttr page "data-my-custom-testid" "Hello")
    , testCase "getByTestId with comma-separated testIdAttributes should match any" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<section><div data-pw=\"Hello\">first</div><div data-ti=\"Hello\">second</div><div data-testid=\"Hello\">third</div></section>"
          assertEqual "count" 2 =<< locatorCount (getByTestIdAttr page "data-pw,data-ti" "Hello")
          texts <- evaluateOnLocatorAllTexts (getByTestIdAttr page "data-pw,data-ti" "Hello") "els => els.map(e => e.textContent)"
          assertEqual "texts" ["first", "second"] texts
    , testCase "getByTestId should work for regex" $
        withChromiumPage $ \page -> do
          setContent page "<div><div data-testid=\"Hello\">Hello world</div></div>"
          assertEqual "text" "Hello world" =<< innerText (getByTestIdRegex page "/He[l]*o/")
    , testCase "getByTestId should escape id" $
        withChromiumPage $ \page -> do
          setContent page "<div><div data-testid='He\"llo'>Hello world</div></div>"
          assertEqual "text" "Hello world" =<< innerText (getByTestId page "He\"llo")
    , testCase "getByText should work" $
        withChromiumPage $ \page -> do
          setContent page "<div>yo</div><div>ya</div><div>\nye  </div>"
          html <- evaluateOnLocatorText (getByText page "ye" Inexact) "e => e.outerHTML"
          assertEqual "ye" True (">\nye  </div>" `isInfixOf` html)
    , testCase "getByText should escape special characters" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<label id=label for=control>Hello my\nwo\"rld</label><input id=control placeholder='hello my&#10;wo&quot;rld' title='hello my&#10;wo&quot;rld' alt='hello my&#10;wo&quot;rld'>"
          assertEqual "text" "label" =<< idOf (getByText page "hello my\nwo\"rld" Inexact)
          assertEqual "text with spaces" "label" =<< idOf (getByText page "hello       my     wo\"rld" Inexact)
          assertEqual "label" "control" =<< idOf (getByLabel page "hello my\nwo\"rld" Inexact)
          assertEqual "placeholder" "control" =<< idOf (getByPlaceholder page "hello my\nwo\"rld" Inexact)
          assertEqual "alt" "control" =<< idOf (getByAltText page "hello my\nwo\"rld" Inexact)
          assertEqual "title" "control" =<< idOf (getByTitle page "hello my\nwo\"rld" Inexact)
    , testCase "getByText should normalize whitespace in text" $
        withChromiumPage $ \page -> do
          setContent page "<label id=label for=control>hello my\nworld</label><input id=control>"
          assertEqual "text" "label" =<< idOf (getByText page "hello        my    world" Inexact)
    , testCase "getByTitle should escape backslashes" $
        withChromiumPage $ \page -> do
          setContent page "<div id=target title=\"my title\">Text here</div>"
          assertEqual "exact" 1 =<< locatorCount (getByTitle page "my title" Exact)
          assertEqual "one backslash" 0 =<< locatorCount (getByTitle page "my t\\itle" Exact)
          assertEqual "two backslashes" 0 =<< locatorCount (getByTitle page "my t\\\\itle" Exact)
    , testCase "getByText should work with >> in the text" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<label for=target>foo &gt;&gt; bar</label><input id=target placeholder='foo &gt;&gt; bar' title='foo &gt;&gt; bar' alt='foo &gt;&gt; bar'>"
          assertEqual "text" "foo >> bar" =<< innerText (getByText page "foo >> bar" Inexact)
          assertEqual "text regex" "foo >> bar" =<< innerText (getByTextRegex page "/foo >> bar/")
          assertEqual "locator" "foo >> bar" =<< innerText (locator page (selectorFromText "label"))
          assertEqual "label" "target" =<< idOf (getByLabel page "foo >> bar" Inexact)
          assertEqual "label regex" "target" =<< idOf (getByLabelRegex page "/foo >> bar/")
          assertEqual "placeholder" "target" =<< idOf (getByPlaceholder page "foo >> bar" Inexact)
          assertEqual "placeholder regex" "target" =<< idOf (getByPlaceholderRegex page "/foo >> bar/")
          assertEqual "alt" "target" =<< idOf (getByAltText page "foo >> bar" Inexact)
          assertEqual "alt regex" "target" =<< idOf (getByAltTextRegex page "/foo >> bar/")
          assertEqual "title" "target" =<< idOf (getByTitle page "foo >> bar" Inexact)
          assertEqual "title regex" "target" =<< idOf (getByTitleRegex page "/foo >> bar/")
    , testCase "getByPlaceholder should work" $
        withChromiumPage $ \page -> do
          setContent page "<div><input placeholder='Hello'><input placeholder='Hello World'></div>"
          assertEqual "inexact" 2 =<< locatorCount (getByPlaceholder page "hello" Inexact)
          assertEqual "exact" 1 =<< locatorCount (getByPlaceholder page "Hello" Exact)
          assertEqual "regex" 1 =<< locatorCount (getByPlaceholderRegex page "/wor/i")
    , testCase "getByAltText should work" $
        withChromiumPage $ \page -> do
          setContent page "<div><input alt='Hello'><input alt='Hello World'></div>"
          assertEqual "inexact" 2 =<< locatorCount (getByAltText page "hello" Inexact)
          assertEqual "exact" 1 =<< locatorCount (getByAltText page "Hello" Exact)
          assertEqual "regex" 1 =<< locatorCount (getByAltTextRegex page "/wor/i")
    , testCase "getByTitle should work" $
        withChromiumPage $ \page -> do
          setContent page "<div><input title='Hello'><input title='Hello World'></div>"
          assertEqual "inexact" 2 =<< locatorCount (getByTitle page "hello" Inexact)
          assertEqual "exact" 1 =<< locatorCount (getByTitle page "Hello" Exact)
          assertEqual "regex" 1 =<< locatorCount (getByTitleRegex page "/wor/i")
    ]
