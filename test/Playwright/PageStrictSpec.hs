module Playwright.PageStrictSpec
  ( tests
  ) where

import Playwright
  ( DomEventType (DomEventType)
  , WaitForState (WaitForAttached)
  , dispatchEvent
  , fill
  , getAttribute
  , hover
  , querySelectorStrict
  , selectorFromText
  , setContent
  , textContent
  , waitForSelectorStrict
  )
import Playwright.Harness
  ( assertProtocolContains
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
    "page-strict"
    [ testCase "should fail page.textContent in strict mode" $
        withChromiumPage $ \page -> do
          setContent page "<span>span1</span><div><span>target</span></div>"
          assertProtocolContains "strict mode violation" (textContent (locate page "span"))
    , testCase "should fail page.getAttribute in strict mode" $
        withChromiumPage $ \page -> do
          setContent page "<span>span1</span><div><span>target</span></div>"
          assertProtocolContains "strict mode violation" (getAttribute (locate page "span") "id")
    , testCase "should fail page.fill in strict mode" $
        withChromiumPage $ \page -> do
          setContent page "<input></input><div><input></input></div>"
          assertProtocolContains "strict mode violation" (fill (locate page "input") "text")
    , testCase "should fail page.$ in strict mode" $
        withChromiumPage $ \page -> do
          setContent page "<span>span1</span><div><span>target</span></div>"
          assertProtocolContains "strict mode violation" (querySelectorStrict page (selectorFromText "span"))
    , testCase "should fail page.waitForSelector in strict mode" $
        withChromiumPage $ \page -> do
          setContent page "<span>span1</span><div><span>target</span></div>"
          assertProtocolContains
            "strict mode violation"
            (waitForSelectorStrict page (selectorFromText "span") WaitForAttached)
    , testCase "should fail page.dispatchEvent in strict mode" $
        withChromiumPage $ \page -> do
          setContent page "<span></span><div><span></span></div>"
          assertProtocolContains "strict mode violation" (dispatchEvent (locate page "span") (DomEventType "click"))
    , testCase "should fail locator hover in strict mode" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div><div></div><div><div></div><div></div></div></div><div><div class='foo'></div><div class='foo'></div></div>"
          assertProtocolContains "strict mode violation" (hover (locate page ".foo"))
    ]
