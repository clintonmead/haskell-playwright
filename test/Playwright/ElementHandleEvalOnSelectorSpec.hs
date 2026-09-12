module Playwright.ElementHandleEvalOnSelectorSpec
  ( tests
  ) where

import Playwright
  ( evalOnSelectorAllHandleNumber
  , evalOnSelectorAllHandleTexts
  , evalOnSelectorHandleText
  , querySelector
  , selectorFromText
  , setContent
  )
import Playwright.Harness
  ( assertProtocolContains
  , requireHandle
  , withChromiumPage
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
    "elementhandle-eval-on-selector"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<html><body><div class=\"tweet\"><div class=\"like\">100</div><div class=\"retweets\">10</div></div></body></html>"
          tweet <- requireHandle =<< querySelector page (selectorFromText ".tweet")
          content <- evalOnSelectorHandleText tweet (selectorFromText ".like") "node => node.innerText"
          assertEqual "like" "100" content
    , testCase "should retrieve content from subtree" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div class=\"a\">not-a-child-div</div><div id=\"myId\"><div class=\"a\">a-child-div</div></div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "#myId")
          content <- evalOnSelectorHandleText handle (selectorFromText ".a") "node => node.innerText"
          assertEqual "child" "a-child-div" content
    , testCase "should throw in case of missing selector" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div class=\"a\">not-a-child-div</div><div id=\"myId\"></div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "#myId")
          assertProtocolContains
            "Failed to find element matching selector \".a\""
            (evalOnSelectorHandleText handle (selectorFromText ".a") "node => node.innerText")
    , testCase "should work for all" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<html><body><div class=\"tweet\"><div class=\"like\">100</div><div class=\"like\">10</div></div></body></html>"
          tweet <- requireHandle =<< querySelector page (selectorFromText ".tweet")
          content <-
            evalOnSelectorAllHandleTexts
              tweet
              (selectorFromText ".like")
              "nodes => nodes.map(n => n.innerText)"
          assertEqual "likes" ["100", "10"] content
    , testCase "should retrieve content from subtree for all" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div class=\"a\">not-a-child-div</div><div id=\"myId\"><div class=\"a\">a1-child-div</div><div class=\"a\">a2-child-div</div></div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "#myId")
          content <-
            evalOnSelectorAllHandleTexts
              handle
              (selectorFromText ".a")
              "nodes => nodes.map(n => n.innerText)"
          assertEqual "children" ["a1-child-div", "a2-child-div"] content
    , testCase "should not throw in case of missing selector for all" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div class=\"a\">not-a-child-div</div><div id=\"myId\"></div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "#myId")
          count <-
            evalOnSelectorAllHandleNumber
              handle
              (selectorFromText ".a")
              "nodes => nodes.length"
          assertEqual "none" (0 :: Double) count
    ]
