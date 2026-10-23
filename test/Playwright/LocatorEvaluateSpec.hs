module Playwright.LocatorEvaluateSpec
  ( tests
  ) where

import Playwright
  ( evaluateOnLocatorAllTexts
  , evaluateOnLocatorText
  , setContent
  )
import Playwright.Harness
  ( locate
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
    "locator-evaluate"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<html><body><div class=\"tweet\"><div class=\"like\">100</div><div class=\"retweets\">10</div></div></body></html>"
          content <- evaluateOnLocatorText (locate page ".tweet .like") "node => node.innerText"
          assertEqual "like" "100" content
    , testCase "should retrieve content from subtree" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div class=\"a\">not-a-child-div</div><div id=\"myId\"><div class=\"a\">a-child-div</div></div>"
          content <- evaluateOnLocatorText (locate page "#myId .a") "node => node.innerText"
          assertEqual "child" "a-child-div" content
    , testCase "should work for all" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<html><body><div class=\"tweet\"><div class=\"like\">100</div><div class=\"like\">10</div></div></body></html>"
          content <-
            evaluateOnLocatorAllTexts
              (locate page ".tweet .like")
              "nodes => nodes.map(n => n.innerText)"
          assertEqual "likes" ["100", "10"] content
    , testCase "should not throw in case of missing selector for all" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div class=\"a\">not-a-child-div</div><div id=\"myId\"></div>"
          content <-
            evaluateOnLocatorAllTexts
              (locate page "#myId .a")
              "nodes => nodes.map(n => n.innerText)"
          assertEqual "none" [] content
    ]
