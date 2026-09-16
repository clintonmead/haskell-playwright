module Playwright.LocatorQuerySpec
  ( tests
  ) where

import Playwright
  ( AriaRole (AriaRole)
  , allInnerTexts
  , andLocator
  , chainLocator
  , childLocator
  , click
  , defaultGetByRoleOptions
  , evaluateOnLocator
  , evaluateOnLocatorText
  , filterHas
  , filterHasNot
  , filterHasNotText
  , filterHasText
  , filterHasTextRegex
  , first
  , frameLocator
  , getAttribute
  , getByRole
  , getByTestId
  , innerText
  , isVisible
  , lastLocator
  , locatorCount
  , locatorOnFrame
  , mainFrame
  , nth
  , orLocator
  , selectorFromText
  , setContent
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withChromiumPage
  , assertEqual
  )
import Playwright.Locator qualified as Locator
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
    "locator-query"
    [ testCase "should respect first and last" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<section><div><p>A</p></div><div><p>A</p><p>A</p></div><div><p>A</p><p>A</p><p>A</p></div></section>"
          let
            pInDiv = childLocator (locate page "div") (selectorFromText "p")
          assertEqual "div >> p" 6 =<< locatorCount (locate page "div >> p")
          assertEqual "nested" 6 =<< locatorCount pInDiv
          assertEqual "first" 1 =<< locatorCount (childLocator (first (locate page "div")) (selectorFromText "p"))
          assertEqual "last" 3 =<< locatorCount (childLocator (lastLocator (locate page "div")) (selectorFromText "p"))
    , testCase "should respect nth" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<section><div><p>A</p></div><div><p>A</p><p>A</p></div><div><p>A</p><p>A</p><p>A</p></div></section>"
          assertEqual "nth 0" 1 =<< locatorCount (nth (locate page "div >> p") 0)
          assertEqual "nth 1" 2
            =<< locatorCount (childLocator (nth (locate page "div") 1) (selectorFromText "p"))
          assertEqual "nth 2" 3
            =<< locatorCount (childLocator (nth (locate page "div") 2) (selectorFromText "p"))
    , testCase "should throw on capture w/ nth()" $
        withChromiumPage $ \page -> do
          setContent page "<section><div><p>A</p></div></section>"
          assertProtocolContains "Can't query n-th element" (click (nth (locate page "*css=div >> p") 1))
    , testCase "should throw on due to strictness" $
        withChromiumPage $ \page -> do
          setContent page "<div>A</div><div>B</div>"
          assertProtocolContains "strict mode violation" (isVisible (locate page "div"))
    , testCase "should throw on due to strictness 2" $
        withChromiumPage $ \page -> do
          setContent page "<select><option>One</option><option>Two</option></select>"
          assertProtocolContains "strict mode violation" (evaluateOnLocator (locate page "option") "e => {}")
    , testCase "should filter by text" $
        withChromiumPage $ \page -> do
          setContent page "<div>Foobar</div><div>Bar</div>"
          assertEqual "Foo" "Foobar" =<< innerText (filterHasText (locate page "div") "Foo")
    , testCase "should filter by text 2" $
        withChromiumPage $ \page -> do
          setContent page "<div>foo <span>hello world</span> bar</div>"
          assertEqual "hello world" "foo hello world bar" =<< innerText (filterHasText (locate page "div") "hello world")
    , testCase "should filter by regex" $
        withChromiumPage $ \page -> do
          setContent page "<div>Foobar</div><div>Bar</div>"
          assertEqual "Foo.*" "Foobar" =<< innerText (filterHasTextRegex (locate page "div") "/Foo.*/")
    , testCase "should filter by text with quotes" $
        withChromiumPage $ \page -> do
          setContent page "<div>Hello \"world\"</div><div>Hello world</div>"
          assertEqual "quoted" "Hello \"world\"" =<< innerText (filterHasText (locate page "div") "Hello \"world\"")
    , testCase "should filter by regex and regexp flags" $
        withChromiumPage $ \page -> do
          setContent page "<div>Hello \"world\"</div><div>Hello world</div>"
          assertEqual "flags" "Hello \"world\""
            =<< innerText (filterHasTextRegex (locate page "div") "/hElLo \"world\"/i")
    , testCase "should filter by case-insensitive regex in a child" $
        withChromiumPage $ \page -> do
          setContent page "<div class=\"test\"><h5>Title Text</h5></div>"
          assertEqual "title" "Title Text"
            =<< innerText (filterHasTextRegex (locate page "div") "/^title text$/i")
    , testCase "should filter by case-insensitive regex in multiple children" $
        withChromiumPage $ \page -> do
          setContent page "<div class=\"test\"><h5>Title</h5> <h2><i>Text</i></h2></div>"
          assertEqual "class" (Just "test")
            =<< getAttribute (filterHasTextRegex (locate page "div") "/^title text$/i") "class"
    , testCase "should support has:locator" $
        withChromiumPage $ \page -> do
          setContent page "<div><span>hello</span></div><div><span>world</span></div>"
          assertEqual "world count" 1 =<< locatorCount (filterHas (locate page "div") (locate page "text=world"))
          assertEqual "world html" "<div><span>world</span></div>"
            =<< evaluateOnLocatorText (filterHas (locate page "div") (locate page "text=world")) "e => e.outerHTML"
          assertEqual "hello count" 1 =<< locatorCount (filterHas (locate page "div") (locate page "text=\"hello\""))
          assertEqual "hello html" "<div><span>hello</span></div>"
            =<< evaluateOnLocatorText (filterHas (locate page "div") (locate page "text=\"hello\"")) "e => e.outerHTML"
          assertEqual "xpath" 2 =<< locatorCount (filterHas (locate page "div") (locate page "xpath=./span"))
          assertEqual "span" 2 =<< locatorCount (filterHas (locate page "div") (locate page "span"))
          assertEqual "span wor count" 1
            =<< locatorCount (filterHas (locate page "div") (filterHasText (locate page "span") "wor"))
          assertEqual "span wor html" "<div><span>world</span></div>"
            =<< evaluateOnLocatorText (filterHas (locate page "div") (filterHasText (locate page "span") "wor")) "e => e.outerHTML"
          assertEqual "has+text" 1
            =<< locatorCount (filterHas (filterHasText (locate page "div") "wor") (locate page "span"))
    , testCase "should support locator.filter" $
        withChromiumPage $ \page -> do
          setContent page "<section><div><span>hello</span></div><div><span>world</span></div></section>"
          assertEqual "hello" 1 =<< locatorCount (filterHasText (locate page "div") "hello")
          assertEqual "hello hello" 1
            =<< locatorCount (filterHasText (filterHasText (locate page "div") "hello") "hello")
          assertEqual "hello world" 0
            =<< locatorCount (filterHasText (filterHasText (locate page "div") "hello") "world")
          assertEqual "section" 1
            =<< locatorCount (filterHasText (filterHasText (locate page "section") "hello") "world")
          assertEqual "span" 1
            =<< locatorCount (childLocator (filterHasText (locate page "div") "hello") (selectorFromText "span"))
          assertEqual "has world" 1
            =<< locatorCount (filterHas (locate page "div") (filterHasText (locate page "span") "world"))
          assertEqual "has span" 2
            =<< locatorCount (filterHas (locate page "div") (locate page "span"))
          assertEqual "has+text world" 1
            =<< locatorCount (filterHas (filterHasText (locate page "div") "world") (locate page "span"))
          assertEqual "hasNot world" 1
            =<< locatorCount (filterHasNot (locate page "div") (filterHasText (locate page "span") "world"))
          assertEqual "hasNot section" 2
            =<< locatorCount (filterHasNot (locate page "div") (locate page "section"))
          assertEqual "hasNot span" 0
            =<< locatorCount (filterHasNot (locate page "div") (locate page "span"))
          assertEqual "hasNotText hello" 1
            =<< locatorCount (filterHasNotText (locate page "div") "hello")
          assertEqual "hasNotText foo" 2
            =<< locatorCount (filterHasNotText (locate page "div") "foo")
    , testCase "should support locator.and" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div data-testid=foo>hello</div><div data-testid=bar>world</div><span data-testid=foo>hello2</span><span data-testid=bar>world2</span>"
          assertEqual "div and div" 2 =<< locatorCount (andLocator (locate page "div") (locate page "div"))
          assertEqual "div foo" ["hello"] =<< allInnerTexts (andLocator (locate page "div") (getByTestId page "foo"))
          assertEqual "div bar" ["world"] =<< allInnerTexts (andLocator (locate page "div") (getByTestId page "bar"))
          assertEqual "foo div" ["hello"] =<< allInnerTexts (andLocator (getByTestId page "foo") (locate page "div"))
          assertEqual "bar span" ["world2"] =<< allInnerTexts (andLocator (getByTestId page "bar") (locate page "span"))
    , testCase "should support locator.or" $
        withChromiumPage $ \page -> do
          setContent page "<div>hello</div><span>world</span>"
          assertEqual "count" 2 =<< locatorCount (orLocator (locate page "div") (locate page "span"))
          assertEqual "both" ["hello", "world"] =<< allInnerTexts (orLocator (locate page "div") (locate page "span"))
          assertEqual "nested or" ["hello", "world"]
            =<< allInnerTexts (orLocator (orLocator (locate page "span") (locate page "article")) (locate page "div"))
          assertEqual "empty" 0 =<< locatorCount (orLocator (locate page "article") (locate page "something"))
          assertEqual "article or div" "hello" =<< innerText (orLocator (locate page "article") (locate page "div"))
          assertEqual "article or span" "world" =<< innerText (orLocator (locate page "article") (locate page "span"))
          assertEqual "div or article" "hello" =<< innerText (orLocator (locate page "div") (locate page "article"))
          assertEqual "span or article" "world" =<< innerText (orLocator (locate page "span") (locate page "article"))
    , testCase "should support locator.locator with and/or" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div>one <span>two</span> <button>three</button> </div><span>four</span><button>five</button>"
          assertEqual "div button" ["three"]
            =<< allInnerTexts (chainLocator (locate page "div") (locate page "button"))
          assertEqual "div button or span" ["two", "three"]
            =<< allInnerTexts (chainLocator (locate page "div") (orLocator (locate page "button") (locate page "span")))
          assertEqual "button or span" ["two", "three", "four", "five"]
            =<< allInnerTexts (orLocator (locate page "button") (locate page "span"))
          assertEqual "div button and role" ["three"]
            =<< allInnerTexts
              (chainLocator (locate page "div") (andLocator (locate page "button") (getByRole page (AriaRole "button") defaultGetByRoleOptions)))
          assertEqual "button and role" ["three", "five"]
            =<< allInnerTexts (andLocator (locate page "button") (getByRole page (AriaRole "button") defaultGetByRoleOptions))
    , testCase "should allow some, but not all nested frameLocators" $
        withChromiumPage $ \page -> do
          setContent page "<iframe srcdoc=\"<span id=target>world</span>\"></iframe><span>hello</span>"
          let
            iframe = frameLocator page (selectorFromText "iframe")
            other = frameLocator page (selectorFromText "#iframe")
            iframeSpan = childLocator iframe (selectorFromText "span")
            iframeArticle = childLocator iframe (selectorFromText "article")
            iframeTarget = childLocator iframe (selectorFromText "#target")
          assertEqual "or article" "world" =<< innerText (orLocator iframeSpan iframeArticle)
          assertEqual "or span" "world" =<< innerText (orLocator iframeArticle iframeSpan)
          assertEqual "and target" "world" =<< innerText (andLocator iframeSpan iframeTarget)
          assertProtocolContains
            "Frame locators are not allowed inside composite locators"
            (innerText (orLocator (childLocator iframe (selectorFromText "div")) (childLocator other (selectorFromText "span"))))
          assertProtocolContains
            "Frame locators are not allowed inside composite locators"
            (innerText (andLocator (childLocator iframe (selectorFromText "div")) (childLocator other (selectorFromText "span"))))
    , testCase "alias methods coverage" $
        withChromiumPage $ \page -> do
          setContent page "<div><button>Submit</button></div>"
          assertEqual "button" 1 =<< locatorCount (locate page "button")
          assertEqual "div button" 1
            =<< locatorCount (childLocator (locate page "div") (selectorFromText "button"))
          assertEqual "getByRole" 1
            =<< locatorCount (Locator.getByRole (locate page "div") (AriaRole "button") defaultGetByRoleOptions)
          assertEqual "mainFrame" 1
            =<< locatorCount (locatorOnFrame page (mainFrame page) (selectorFromText "button"))
    ]
