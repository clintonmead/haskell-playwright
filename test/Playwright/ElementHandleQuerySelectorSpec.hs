module Playwright.ElementHandleQuerySelectorSpec
  ( tests
  ) where

import Data.Maybe
  ( isNothing
  )
import Playwright
  ( ElementHandle
  , LoadState (LoadStateDomContentLoaded)
  , Page
  , Playwright
  , armContextPage
  , asElement
  , defaultGotoOptions
  , defaultNewContextOptions
  , elementArgument
  , evaluate
  , evaluateHandle
  , evaluateTextWith
  , evaluateWith
  , goto
  , querySelector
  , querySelectorAllHandle
  , querySelectorHandle
  , selectorFromText
  , setContent
  , textArgument
  , urlFromText
  , waitForLoadState
  )
import Playwright.Harness
  ( requireHandle
  , withAssetsContextWith
  , withChromiumPage
  , assertEqual
  , assertFailure
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
    "elementhandle-query-selector"
    [ testCase "should query existing element" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><div class=\"second\"><div class=\"inner\">A</div></div></body></html>"
          html <- requireHandle =<< querySelector page (selectorFromText "html")
          second <- requireHandle =<< querySelectorHandle html (selectorFromText ".second")
          inner <- requireHandle =<< querySelectorHandle second (selectorFromText ".inner")
          assertEqual "text" "A" =<< evaluateTextWith page "e => e.textContent" (elementArgument inner)
    , testCase "should return null for non-existing element" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><div class=\"second\"><div class=\"inner\">B</div></div></body></html>"
          html <- requireHandle =<< querySelector page (selectorFromText "html")
          second <- querySelectorHandle html (selectorFromText ".third")
          assertEqual "missing" True (isNothing second)
    , testCase "should query existing elements" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><div>A</div><br/><div>B</div></body></html>"
          html <- requireHandle =<< querySelector page (selectorFromText "html")
          elements <- querySelectorAllHandle html (selectorFromText "div")
          assertEqual "count" 2 (length elements)
          texts <- traverse (\el -> evaluateTextWith page "e => e.textContent" (elementArgument el)) elements
          assertEqual "texts" ["A", "B"] texts
    , testCase "should return empty array for non-existing elements" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><span>A</span><br/><span>B</span></body></html>"
          html <- requireHandle =<< querySelector page (selectorFromText "html")
          elements <- querySelectorAllHandle html (selectorFromText "div")
          assertEqual "empty" 0 (length elements)
    , testCase "xpath should query existing element" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><div class=\"second\"><div class=\"inner\">A</div></div></body></html>"
          html <- requireHandle =<< querySelector page (selectorFromText "html")
          seconds <- querySelectorAllHandle html (selectorFromText "xpath=./body/div[contains(@class, 'second')]")
          case seconds of
            [second] -> do
              inners <- querySelectorAllHandle second (selectorFromText "xpath=./div[contains(@class, 'inner')]")
              case inners of
                [inner] ->
                  assertEqual "text" "A" =<< evaluateTextWith page "e => e.textContent" (elementArgument inner)
                _ ->
                  assertFailure "expected one inner"
            _ ->
              assertFailure "expected one second"
    , testCase "xpath should return null for non-existing element" $
        withChromiumPage $ \page -> do
          setContent page "<html><body><div class=\"second\"><div class=\"inner\">B</div></div></body></html>"
          html <- requireHandle =<< querySelector page (selectorFromText "html")
          second <- querySelectorAllHandle html (selectorFromText "xpath=/div[contains(@class, 'third')]")
          assertEqual "empty" 0 (length second)
    , testCase "should work for adopted elements" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          takePopup <- armContextPage ctx
          _ <- evaluateWith page "url => window['__popup'] = window.open(url)" (textArgument (prefix <> "/empty.html"))
          popup <- takePopup
          divHandle <-
            requireHandle =<< asElement =<< evaluateHandle page "() => { const div = document.createElement('div'); document.body.appendChild(div); const span = document.createElement('span'); span.textContent = 'hello'; div.appendChild(span); return div; }"
          assertSpanText page divHandle
          waitForLoadState popup LoadStateDomContentLoaded
          _ <- evaluate page "() => { const div = document.querySelector('div'); window['__popup'].document.body.appendChild(div); }"
          assertSpanText page divHandle
    ]

-- | The handle's child span is found and reads "hello".
assertSpanText :: Page -> ElementHandle -> Playwright ()
assertSpanText page divHandle = do
  child <- requireHandle =<< querySelectorHandle divHandle (selectorFromText "span")
  assertEqual "text" "hello" =<< evaluateTextWith page "e => e.textContent" (elementArgument child)
