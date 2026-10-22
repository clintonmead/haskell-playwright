module Playwright.ElementHandleClickSpec
  ( tests
  ) where

import Playwright
  ( ClickOptions (clickOptionsForce)
  , asElement
  , clickHandle
  , evaluateHandle
  , clickHandleWith
  , defaultClickOptions
  , defaultGotoOptions
  , dblclick
  , elementArgument
  , evaluate
  , evaluateBool
  , evaluateText
  , evaluateWith
  , goto
  , querySelector
  , selectorFromText
  , setContent
  , urlFromText
  )
import Playwright.Harness
  ( assertEqual
  , assertProtocolContains
  , locate
  , requireHandle
  , withAssetPage
  , withAssets
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
    "elementhandle-click"
    [ testCase "should work" $
        withAssetPage "input/button.html" $ \page -> do
          handle <- requireHandle =<< querySelector page (selectorFromText "button")
          clickHandle handle
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should work with Node removed" $
        withAssetPage "input/button.html" $ \page -> do
          _ <- evaluate page "() => { delete window.Node; }"
          handle <- requireHandle =<< querySelector page (selectorFromText "button")
          clickHandle handle
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should throw for detached nodes" $
        withAssetPage "input/button.html" $ \page -> do
          button <- requireHandle =<< querySelector page (selectorFromText "button")
          _ <- evaluateWith page "button => button.remove()" (elementArgument button)
          assertProtocolContains "Element is not attached to the DOM" (clickHandle button)
    , testCase "should throw for hidden nodes with force" $
        withAssetPage "input/button.html" $ \page -> do
          button <- requireHandle =<< querySelector page (selectorFromText "button")
          _ <- evaluateWith page "button => button.style.display = 'none'" (elementArgument button)
          assertProtocolContains
            "Element is not visible"
            (clickHandleWith button defaultClickOptions {clickOptionsForce = Just True})
    , testCase "should throw for recursively hidden nodes with force" $
        withAssetPage "input/button.html" $ \page -> do
          button <- requireHandle =<< querySelector page (selectorFromText "button")
          _ <- evaluateWith page "button => button.parentElement.style.display = 'none'" (elementArgument button)
          assertProtocolContains
            "Element is not visible"
            (clickHandleWith button defaultClickOptions {clickOptionsForce = Just True})
    , testCase "should throw for <br> elements with force" $
        withChromiumPage $ \page -> do
          setContent page "hello<br>goodbye"
          br <- requireHandle =<< querySelector page (selectorFromText "br")
          assertProtocolContains
            "Element is outside of the viewport"
            (clickHandleWith br defaultClickOptions {clickOptionsForce = Just True})
    , testCase "should double click the button" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/input/button.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { window['double'] = false; document.querySelector('button').addEventListener('dblclick', () => { window['double'] = true; }); }"
          dblclick (locate page "button")
          assertEqual "double" True =<< evaluateBool page "() => window['double']"
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should work for Shadow DOM v1" $
        withAssetPage "shadow.html" $ \page -> do
          handle <- requireHandle =<< asElement =<< evaluateHandle page "() => window['button']"
          clickHandle handle
          assertEqual "clicked" True =<< evaluateBool page "() => clicked"
    ]
