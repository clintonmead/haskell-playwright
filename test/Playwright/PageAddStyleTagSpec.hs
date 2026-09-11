module Playwright.PageAddStyleTagSpec
  ( tests
  ) where

import Playwright
  ( StyleTag (styleTagContent, styleTagUrl)
  , addStyleTag
  , defaultGotoOptions
  , emptyStyleTag
  , evaluateText
  , goto
  , urlFromText
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssetPage
  , withAssets
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
    "page-add-style-tag"
    [ testCase "should work with a url" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            addStyleTag
              page
              emptyStyleTag {styleTagUrl = Just (urlFromText (prefix <> "/injectedstyle.css"))}
          color <-
            evaluateText
              page
              "() => window.getComputedStyle(document.querySelector('body')).getPropertyValue('background-color')"
          assertEqual "background" "rgb(255, 0, 0)" color
    , testCase "should work with content" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            addStyleTag
              page
              emptyStyleTag {styleTagContent = Just "body { background-color: green; }"}
          color <-
            evaluateText
              page
              "() => window.getComputedStyle(document.querySelector('body')).getPropertyValue('background-color')"
          assertEqual "background" "rgb(0, 128, 0)" color
    , testCase "should throw an error if loading from url fail" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          -- Official Playwright rejects with the stylesheet error Event, whose
          -- serialized message is just "Event", not the missing URL.
          assertProtocolContains
            "Event"
            ( addStyleTag
                page
                emptyStyleTag {styleTagUrl = Just (urlFromText (prefix <> "/nonexistfile.js"))}
            )
    ]
