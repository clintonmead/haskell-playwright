module Playwright.LocatorIsVisibleSpec
  ( tests
  ) where

import Playwright
  ( WaitForState (WaitForHidden, WaitForVisible)
  , Playwright
  , goto
  , defaultGotoOptions
  , urlFromText
  , isHidden
  , isVisible
  , setContent
  , waitFor
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssets
  , locate
  , withChromiumPage
  , assertEqual
  )
import Playwright.Error
  ( PlaywrightError
  )
import UnliftIO.Exception
  ( try
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
    "locator-is-visible"
    [ testCase "isVisible should be true for opacity:0" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"opacity:0\">Hi</div>"
          assertEqual "visible" True =<< isVisible (locate page "div")
    , testCase "isVisible should be true for element outside view" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"position: absolute; left: -1000px\">Hi</div>"
          assertEqual "visible" True =<< isVisible (locate page "div")
    , testCase "isVisible and isHidden should work with details" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<details><summary>click to open</summary><ul><li>hidden item 1</li><li>hidden item 2</li><li>hidden item 3</li></ul></details>"
          assertEqual "hidden" True =<< isHidden (locate page "ul")
    , testCase "isVisible inside a button" $
        withChromiumPage $ \page -> do
          setContent page "<button><span></span>a button</button>"
          let
            spanLoc = locate page "span"
          assertEqual "span visible" False =<< isVisible spanLoc
          assertEqual "span hidden" True =<< isHidden spanLoc
          waitFor spanLoc WaitForHidden
          waitFor (locate page "button") WaitForVisible
    , testCase "isVisible inside a role=button" $
        withChromiumPage $ \page -> do
          setContent page "<div role=button><span></span>a button</div>"
          let
            spanLoc = locate page "span"
          assertEqual "span visible" False =<< isVisible spanLoc
          assertEqual "span hidden" True =<< isHidden spanLoc
          waitFor spanLoc WaitForHidden
          waitFor (locate page "[role=button]") WaitForVisible
    , testCase "isVisible with invalid selector should throw" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "Unknown engine \"hey\" while parsing selector hey=what"
            (isVisible (locate page "hey=what"))
    , testCase "isVisible during navigation should not throw" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          mapM_
            ( \_ -> do
                _ <-
                  try
                    ( setContent
                        page
                        ( "<script>setTimeout(() => { window.location.href = '" <> prefix <> "/empty.html'; }, Math.random() * 50);</script>"
                        )
                    )
                    :: Playwright (Either PlaywrightError ())
                visible <- isVisible (locate page "div")
                assertEqual "visible" False visible
            )
            [1 .. 20 :: Int]
    ]
