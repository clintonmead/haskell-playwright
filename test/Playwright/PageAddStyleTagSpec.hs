module Playwright.PageAddStyleTagSpec
  ( tests
  ) where

import Data.Text
  ( replace
  )
import UnliftIO.Exception
  ( try
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright
  ( ElementHandle
  , Playwright
  , StyleTag (styleTagContent, styleTagPath, styleTagUrl)
  , emptyStyleTag
  , url
  , urlFromText
  , urlToText
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
  , testAssetsDir
  , assertFailure
  , withAssets
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
    , testCase "should throw when added with content to the CSP page" $
        withAssetPage "csp.html" $ \page -> do
          result <-
            try (addStyleTag page emptyStyleTag {styleTagContent = Just "body { background-color: green; }"})
              :: Playwright (Either PlaywrightError ElementHandle)
          case result of
            Left _ ->
              pure ()
            Right _ ->
              assertFailure "expected the inline style to be refused"
    , testCase "should throw when added with URL to the CSP page" $
        withAssetPage "csp.html" $ \page -> do
          here <- urlToText <$> url page
          let
            cross = replace "127.0.0.1" "localhost" here <> "/injectedstyle.css"
          result <-
            try (addStyleTag page emptyStyleTag {styleTagUrl = Just (urlFromText cross)})
              :: Playwright (Either PlaywrightError ElementHandle)
          case result of
            Left _ ->
              pure ()
            Right _ ->
              assertFailure "expected the linked style to be refused"
    , testCase "should throw an error if no options are provided" $
        withAssetPage "empty.html" $ \page -> do
          result <-
            try (addStyleTag page emptyStyleTag) :: Playwright (Either PlaywrightError ElementHandle)
          case result of
            Left _ ->
              pure ()
            Right _ ->
              assertFailure "expected an error without options"
    , testCase "should work with a path" $ do
        assets <- testAssetsDir
        withAssetPage "empty.html" $ \page -> do
          _ <- addStyleTag page emptyStyleTag {styleTagPath = Just (assets <> "/injectedstyle.css")}
          assertEqual "background" "rgb(255, 0, 0)"
            =<< evaluateText page "() => window.getComputedStyle(document.querySelector('body')).getPropertyValue('background-color')"
    ]
