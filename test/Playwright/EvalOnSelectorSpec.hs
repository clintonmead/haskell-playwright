module Playwright.EvalOnSelectorSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Data.Maybe
  ( isJust
  , isNothing
  )
import Playwright
  ( WaitForState (WaitForVisible)
  , elementArgument
  , evalOnSelectorText
  , evalOnSelectorTextWith
  , querySelector
  , selectorFromText
  , setContent
  , textArgument
  , waitForSelector
  )
import Playwright.Harness
  ( assertProtocolContains
  , requireHandle
  , withAssetPage
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "eval-on-selector"
    [ testCase "should work with css selector" $
        withChromiumPage $ \page -> do
          setContent page "<section id=\"testAttribute\">43543</section>"
          assertEqual "id" "testAttribute"
            =<< evalOnSelectorText page (selectorFromText "css=section") "e => e.id"
    , testCase "should work with id selector" $
        withChromiumPage $ \page -> do
          setContent page "<section id=\"testAttribute\">43543</section>"
          assertEqual "id" "testAttribute"
            =<< evalOnSelectorText page (selectorFromText "id=testAttribute") "e => e.id"
    , testCase "should work with data-test selector" $
        withChromiumPage $ \page -> do
          setContent page "<section data-test=foo id=\"testAttribute\">43543</section>"
          assertEqual "id" "testAttribute"
            =<< evalOnSelectorText page (selectorFromText "data-test=foo") "e => e.id"
    , testCase "should work with data-testid selector" $
        withChromiumPage $ \page -> do
          setContent page "<section data-testid=foo id=\"testAttribute\">43543</section>"
          assertEqual "id" "testAttribute"
            =<< evalOnSelectorText page (selectorFromText "data-testid=foo") "e => e.id"
    , testCase "should work with data-test-id selector" $
        withChromiumPage $ \page -> do
          setContent page "<section data-test-id=foo id=\"testAttribute\">43543</section>"
          assertEqual "id" "testAttribute"
            =<< evalOnSelectorText page (selectorFromText "data-test-id=foo") "e => e.id"
    , testCase "should work with text selector in quotes" $
        withChromiumPage $ \page -> do
          setContent page "<section id=\"testAttribute\">43543</section>"
          assertEqual "id" "testAttribute"
            =<< evalOnSelectorText page (selectorFromText "text=\"43543\"") "e => e.id"
    , testCase "should work with xpath selector" $
        withChromiumPage $ \page -> do
          setContent page "<section id=\"testAttribute\">43543</section>"
          assertEqual "id" "testAttribute"
            =<< evalOnSelectorText page (selectorFromText "xpath=/html/body/section") "e => e.id"
    , testCase "should work with text selector" $
        withChromiumPage $ \page -> do
          setContent page "<section id=\"testAttribute\">43543</section>"
          assertEqual "id" "testAttribute"
            =<< evalOnSelectorText page (selectorFromText "text=43543") "e => e.id"
    , testCase "should auto-detect css selector" $
        withChromiumPage $ \page -> do
          setContent page "<section id=\"testAttribute\">43543</section>"
          assertEqual "id" "testAttribute"
            =<< evalOnSelectorText page (selectorFromText "section") "e => e.id"
    , testCase "should auto-detect css selector with attributes" $
        withChromiumPage $ \page -> do
          setContent page "<section id=\"testAttribute\">43543</section>"
          assertEqual "id" "testAttribute"
            =<< evalOnSelectorText page (selectorFromText "section[id=\"testAttribute\"]") "e => e.id"
    , testCase "should auto-detect nested selectors" $
        withChromiumPage $ \page -> do
          setContent page "<div foo=bar><section>43543<span>Hello<div id=target></div></span></section></div>"
          assertEqual "id" "target"
            =<< evalOnSelectorText page (selectorFromText "div[foo=bar] > section >> \"Hello\" >> div") "e => e.id"
    , testCase "should accept arguments" $
        withChromiumPage $ \page -> do
          setContent page "<section>hello</section>"
          assertEqual "text" "hello world!"
            =<< evalOnSelectorTextWith page (selectorFromText "section") "(e, suffix) => e.textContent + suffix" (textArgument " world!")
    , testCase "should accept ElementHandles as arguments" $
        withChromiumPage $ \page -> do
          setContent page "<section>hello</section><div> world</div>"
          divHandle <- requireHandle =<< querySelector page (selectorFromText "div")
          assertEqual "text" "hello world"
            =<< evalOnSelectorTextWith
              page
              (selectorFromText "section")
              "(e, div) => e.textContent + div.textContent"
              (elementArgument divHandle)
    , testCase "should throw error if no element is found" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "Failed to find element matching selector \"section\""
            (evalOnSelectorText page (selectorFromText "section") "e => e.id")
    , testCase "should support >> syntax" $
        withChromiumPage $ \page -> do
          setContent page "<section><div>hello</div></section>"
          assertEqual "text" "hello world!"
            =<< evalOnSelectorTextWith
              page
              (selectorFromText "css=section >> css=div")
              "(e, suffix) => e.textContent + suffix"
              (textArgument " world!")
    , testCase "should support >> syntax with different engines" $
        withChromiumPage $ \page -> do
          setContent page "<section><div><span>hello</span></div></section>"
          assertEqual "text" "hello world!"
            =<< evalOnSelectorTextWith
              page
              (selectorFromText "xpath=/html/body/section >> css=div >> text=\"hello\"")
              "(e, suffix) => e.textContent + suffix"
              (textArgument " world!")
    , testCase "should support spaces with >> syntax" $
        withAssetPage "deep-shadow.html" $ \page ->
          assertEqual "text" "Hello from root2"
            =<< evalOnSelectorText page (selectorFromText " css = div >>css=div>>css   = span  ") "e => e.textContent"
    , testCase "should not stop at first failure with >> syntax" $
        withChromiumPage $ \page -> do
          setContent page "<div><span>Next</span><button>Previous</button><button>Next</button></div>"
          assertEqual "html" "<button>Next</button>"
            =<< evalOnSelectorText page (selectorFromText "button >> \"Next\"") "e => e.outerHTML"
    , testCase "should support * capture" $
        withChromiumPage $ \page -> do
          setContent page "<section><div><span>a</span></div></section><section><div><span>b</span></div></section>"
          assertEqual "div" "<div><span>b</span></div>"
            =<< evalOnSelectorText page (selectorFromText "*css=div >> \"b\"") "e => e.outerHTML"
          assertEqual "section div" "<div><span>b</span></div>"
            =<< evalOnSelectorText page (selectorFromText "section >> *css=div >> \"b\"") "e => e.outerHTML"
          assertEqual "span" "<span>b</span>"
            =<< evalOnSelectorText page (selectorFromText "css=div >> *text=\"b\"") "e => e.outerHTML"
          found <- querySelector page (selectorFromText "*")
          assertEqual "star" True (isJust found)
    , testCase "should throw on multiple * captures" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "Only one of the selectors can capture using * modifier"
            (evalOnSelectorText page (selectorFromText "*css=div >> *css=span") "e => e.outerHTML")
    , testCase "should throw on malformed * capture" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "Unknown engine \"\" while parsing selector *=div"
            (evalOnSelectorText page (selectorFromText "*=div") "e => e.outerHTML")
    , testCase "should work with spaces in css attributes" $
        withChromiumPage $ \page -> do
          setContent page "<div><input placeholder=\"Select date\"></div>"
          waitForSelector page (selectorFromText "[placeholder=\"Select date\"]") WaitForVisible
          waitForSelector page (selectorFromText "[placeholder='Select date']") WaitForVisible
          waitForSelector page (selectorFromText "input[placeholder=\"Select date\"]") WaitForVisible
          waitForSelector page (selectorFromText "input[placeholder='Select date']") WaitForVisible
          assertEqual "found dq" True . isJust =<< querySelector page (selectorFromText "[placeholder=\"Select date\"]")
          assertEqual "found sq" True . isJust =<< querySelector page (selectorFromText "[placeholder='Select date']")
          assertEqual "found input dq" True . isJust =<< querySelector page (selectorFromText "input[placeholder=\"Select date\"]")
          assertEqual "found input sq" True . isJust =<< querySelector page (selectorFromText "input[placeholder='Select date']")
          assertEqual "html dq" "<input placeholder=\"Select date\">"
            =<< evalOnSelectorText page (selectorFromText "[placeholder=\"Select date\"]") "e => e.outerHTML"
          assertEqual "html sq" "<input placeholder=\"Select date\">"
            =<< evalOnSelectorText page (selectorFromText "[placeholder='Select date']") "e => e.outerHTML"
          assertEqual "html input dq" "<input placeholder=\"Select date\">"
            =<< evalOnSelectorText page (selectorFromText "input[placeholder=\"Select date\"]") "e => e.outerHTML"
          assertEqual "html input sq" "<input placeholder=\"Select date\">"
            =<< evalOnSelectorText page (selectorFromText "input[placeholder='Select date']") "e => e.outerHTML"
          assertEqual "html css dq" "<input placeholder=\"Select date\">"
            =<< evalOnSelectorText page (selectorFromText "css=[placeholder=\"Select date\"]") "e => e.outerHTML"
          assertEqual "html css sq" "<input placeholder=\"Select date\">"
            =<< evalOnSelectorText page (selectorFromText "css=[placeholder='Select date']") "e => e.outerHTML"
          assertEqual "html css input dq" "<input placeholder=\"Select date\">"
            =<< evalOnSelectorText page (selectorFromText "css=input[placeholder=\"Select date\"]") "e => e.outerHTML"
          assertEqual "html css input sq" "<input placeholder=\"Select date\">"
            =<< evalOnSelectorText page (selectorFromText "css=input[placeholder='Select date']") "e => e.outerHTML"
          assertEqual "html div dq" "<input placeholder=\"Select date\">"
            =<< evalOnSelectorText page (selectorFromText "div >> [placeholder=\"Select date\"]") "e => e.outerHTML"
          assertEqual "html div sq" "<input placeholder=\"Select date\">"
            =<< evalOnSelectorText page (selectorFromText "div >> [placeholder='Select date']") "e => e.outerHTML"
    , testCase "should work with quotes in css attributes" $
        withChromiumPage $ \page -> do
          setContent page "<div><input placeholder=\"Select&quot;date\"></div>"
          assertEqual "escaped dq" True . isJust =<< querySelector page (selectorFromText "[placeholder=\"Select\\\"date\"]")
          assertEqual "sq with dq" True . isJust =<< querySelector page (selectorFromText "[placeholder='Select\"date']")
          setContent page "<div><input placeholder=\"Select &quot; date\"></div>"
          assertEqual "spaced escaped dq" True . isJust =<< querySelector page (selectorFromText "[placeholder=\"Select \\\" date\"]")
          assertEqual "spaced sq with dq" True . isJust =<< querySelector page (selectorFromText "[placeholder='Select \" date']")
          setContent page "<div><input placeholder=\"Select&apos;date\"></div>"
          assertEqual "dq with sq" True . isJust =<< querySelector page (selectorFromText "[placeholder=\"Select'date\"]")
          assertEqual "escaped sq" True . isJust =<< querySelector page (selectorFromText "[placeholder='Select\\'date']")
          setContent page "<div><input placeholder=\"Select &apos; date\"></div>"
          assertEqual "spaced dq with sq" True . isJust =<< querySelector page (selectorFromText "[placeholder=\"Select ' date\"]")
          assertEqual "spaced escaped sq" True . isJust =<< querySelector page (selectorFromText "[placeholder='Select \\' date']")
    , testCase "should work with spaces in css attributes when missing" $
        withChromiumPage $ \page ->
          withAsync (waitForSelector page (selectorFromText "[placeholder=\"Select date\"]") WaitForVisible) $ \pending -> do
            missing <- querySelector page (selectorFromText "[placeholder=\"Select date\"]")
            assertEqual "missing" True (isNothing missing)
            setContent page "<div><input placeholder=\"Select date\"></div>"
            wait pending
    , testCase "should work with quotes in css attributes when missing" $
        withChromiumPage $ \page ->
          withAsync (waitForSelector page (selectorFromText "[placeholder=\"Select\\\"date\"]") WaitForVisible) $ \pending -> do
            missing <- querySelector page (selectorFromText "[placeholder=\"Select\\\"date\"]")
            assertEqual "missing" True (isNothing missing)
            setContent page "<div><input placeholder=\"Select&quot;date\"></div>"
            wait pending
    , testCase "should return complex values" $
        withChromiumPage $ \page -> do
          setContent page "<section id=\"testAttribute\">43543</section>"
          assertEqual "json" "[{\"id\":\"testAttribute\"}]"
            =<< evalOnSelectorText page (selectorFromText "css=section") "e => JSON.stringify([{ id: e.id }])"
    ]
