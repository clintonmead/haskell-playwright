module Playwright.EvalOnSelectorAllSpec
  ( tests
  ) where

import Playwright
  ( evalOnSelectorAllNumber
  , evalOnSelectorAllTexts
  , evaluate
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

tests :: TestTree
tests =
  testGroup
    "eval-on-selector-all"
    [ testCase "should work with css selector" $
        withChromiumPage $ \page -> do
          setContent page "<div>hello</div><div>beautiful</div><div>world!</div>"
          assertEqual "count" (3 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "css=div") "divs => divs.length"
    , testCase "should work with text selector" $
        withChromiumPage $ \page -> do
          setContent page "<div>hello</div><div>beautiful</div><div>beautiful</div><div>world!</div>"
          assertEqual "count" (2 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "text=\"beautiful\"") "divs => divs.length"
    , testCase "should work with xpath selector" $
        withChromiumPage $ \page -> do
          setContent page "<div>hello</div><div>beautiful</div><div>world!</div>"
          assertEqual "count" (3 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "xpath=/html/body/div") "divs => divs.length"
    , testCase "should auto-detect css selector" $
        withChromiumPage $ \page -> do
          setContent page "<div>hello</div><div>beautiful</div><div>world!</div>"
          assertEqual "count" (3 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "div") "divs => divs.length"
    , testCase "should support >> syntax" $
        withChromiumPage $ \page -> do
          setContent page "<div><span>hello</span></div><div>beautiful</div><div><span>wo</span><span>rld!</span></div><span>Not this one</span>"
          assertEqual "count" (3 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "css=div >> css=span") "spans => spans.length"
    , testCase "should support * capture" $
        withChromiumPage $ \page -> do
          setContent page "<section><div><span>a</span></div></section><section><div><span>b</span></div></section>"
          assertEqual "div b" (1 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "*css=div >> \"b\"") "els => els.length"
          assertEqual "section div b" (1 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "section >> *css=div >> \"b\"") "els => els.length"
          assertEqual "section star" (4 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "section >> *") "els => els.length"
          setContent page "<section><div><span>a</span><span>a</span></div></section>"
          assertEqual "div a" (1 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "*css=div >> \"a\"") "els => els.length"
          assertEqual "section div a" (1 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "section >> *css=div >> \"a\"") "els => els.length"
          setContent page "<div><span>a</span></div><div><span>a</span></div><section><div><span>a</span></div></section>"
          assertEqual "three div a" (3 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "*css=div >> \"a\"") "els => els.length"
          assertEqual "one section div a" (1 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "section >> *css=div >> \"a\"") "els => els.length"
    , testCase "should support * capture when multiple paths match" $
        withChromiumPage $ \page -> do
          setContent page "<div><div><span></span></div></div><div></div>"
          assertEqual "nested" (2 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "*css=div >> span") "els => els.length"
          setContent page "<div><div><span></span></div><span></span><span></span></div><div></div>"
          assertEqual "siblings" (2 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "*css=div >> span") "els => els.length"
    , testCase "should return complex values" $
        withChromiumPage $ \page -> do
          setContent page "<div>hello</div><div>beautiful</div><div>world!</div>"
          assertEqual "texts" ["hello", "beautiful", "world!"]
            =<< evalOnSelectorAllTexts page (selectorFromText "css=div") "divs => divs.map(div => div.textContent)"
    , testCase "should work with bogus Array.from" $
        withChromiumPage $ \page -> do
          setContent page "<div>hello</div><div>beautiful</div><div>world!</div>"
          _ <- evaluate page "() => { Array.from = () => []; }"
          assertEqual "count" (3 :: Double)
            =<< evalOnSelectorAllNumber page (selectorFromText "css=div") "divs => divs.length"
    ]
