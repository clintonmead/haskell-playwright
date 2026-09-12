module Playwright.PageSetContentSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( content
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
    "page-set-content"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          setContent page "<div>hello</div>"
          result <- content page
          assertEqual "content" expectedOutput result
    , testCase "should work with doctype" $
        withChromiumPage $ \page -> do
          let
            doctype :: Text
            doctype = "<!DOCTYPE html>"
          setContent page (doctype <> "<div>hello</div>")
          result <- content page
          assertEqual "content" (doctype <> expectedOutput) result
    , testCase "should work with HTML 4 doctype" $
        withChromiumPage $ \page -> do
          let
            doctype :: Text
            doctype =
              "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
          setContent page (doctype <> "<div>hello</div>")
          result <- content page
          assertEqual "content" (doctype <> expectedOutput) result
    ]

expectedOutput :: Text
expectedOutput = "<html><head></head><body><div>hello</div></body></html>"
