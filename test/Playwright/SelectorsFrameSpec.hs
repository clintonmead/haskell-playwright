module Playwright.SelectorsFrameSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Control.Exception
  ( try
  )
import Control.Monad
  ( forM_
  )
import Data.Maybe
  ( isNothing
  )
import Data.Text
  ( Text
  , isInfixOf
  , unpack
  )
import Playwright
  ( ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , Locator
  , Page
  , Timeout
  , UrlGlob (UrlGlob)
  , WaitForState (WaitForVisible)
  , click
  , clickHandle
  , defaultFulfillOptions
  , defaultGotoOptions
  , evalOnSelectorAllHandleTexts
  , evalOnSelectorAllTexts
  , evalOnSelectorHandleText
  , evalOnSelectorText
  , evaluate
  , evaluateOnLocator
  , fulfillRoute
  , goto
  , innerHtml
  , innerText
  , innerTextHandle
  , locator
  , milliseconds
  , querySelector
  , querySelectorAll
  , querySelectorAllHandle
  , querySelectorHandle
  , route
  , selectorFromText
  , setContent
  , textContentHandle
  , urlFromText
  , waitFor
  , waitForSelector
  , waitForSelectorOnHandle
  , waitForTimeout
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , requireHandle
  , withAssets
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , localOption
  , mkTimeout
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "selectors-frame"
    [ testCase "should work for iframe" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          let
            button :: Locator
            button = locator page (selectorFromText iframeButton)
          waitFor button WaitForVisible
          assertEqual "text" "Hello iframe" =<< innerText button
          click button
    , testCase "should work for iframe (handle)" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          body <- requireHandle =<< querySelector page (selectorFromText "body")
          button <- requireHandle =<< waitForSelectorOnHandle body (selectorFromText iframeButton) WaitForVisible
          assertEqual "inner" "Hello iframe" =<< innerTextHandle button
          assertEqual "text" (Just "Hello iframe") =<< textContentHandle button
          clickHandle button
    , testCase "should work for nested iframe" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          let
            button :: Locator
            button = locator page (selectorFromText nestedButton)
          waitFor button WaitForVisible
          assertEqual "text" "Hello nested iframe" =<< innerText button
          click button
    , testCase "should work for nested iframe (handle)" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          body <- requireHandle =<< querySelector page (selectorFromText "body")
          button <- requireHandle =<< waitForSelectorOnHandle body (selectorFromText nestedButton) WaitForVisible
          assertEqual "inner" "Hello nested iframe" =<< innerTextHandle button
          assertEqual "text" (Just "Hello nested iframe") =<< textContentHandle button
          clickHandle button
    , testCase "should work for $ and $$" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          button <- requireHandle =<< querySelector page (selectorFromText iframeButton)
          assertEqual "text" (Just "Hello iframe") =<< textContentHandle button
          spans <- querySelectorAll page (selectorFromText iframeSpans)
          assertEqual "spans" 2 (length spans)
    , testCase "$ should not wait for frame" $
        withAssets $ \prefix page -> do
          _ <- gotoEmpty prefix page
          assertEqual "page" True . isNothing =<< querySelector page (selectorFromText iframeCanvas)
          body <- requireHandle =<< querySelector page (selectorFromText "body")
          assertEqual "handle" True . isNothing
            =<< querySelectorHandle body (selectorFromText iframeCanvas)
    , testCase "$$ should not wait for frame" $
        withAssets $ \prefix page -> do
          _ <- gotoEmpty prefix page
          assertEqual "page" 0 . length =<< querySelectorAll page (selectorFromText iframeCanvas)
          body <- requireHandle =<< querySelector page (selectorFromText "body")
          assertEqual "handle" 0 . length
            =<< querySelectorAllHandle body (selectorFromText iframeCanvas)
    , testCase "$eval should throw for missing frame" $
        withAssets $ \prefix page -> do
          _ <- gotoEmpty prefix page
          assertProtocolContains
            "Failed to find element matching selector"
            (evalOnSelectorText page (selectorFromText iframeCanvas) "e => 1")
          body <- requireHandle =<< querySelector page (selectorFromText "body")
          assertProtocolContains
            "Failed to find element matching selector"
            (evalOnSelectorHandleText body (selectorFromText iframeCanvas) "e => 1")
    , testCase "$$eval should throw for missing frame" $
        withAssets $ \prefix page -> do
          _ <- gotoEmpty prefix page
          assertProtocolContains
            "Failed to find frame for selector"
            (evalOnSelectorAllTexts page (selectorFromText iframeCanvas) "els => 1")
          body <- requireHandle =<< querySelector page (selectorFromText "body")
          assertProtocolContains
            "Failed to find frame for selector"
            (evalOnSelectorAllHandleTexts body (selectorFromText iframeCanvas) "els => 1")
    , testCase "should work for $ and $$ (handle)" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          body <- requireHandle =<< querySelector page (selectorFromText "body")
          button <- requireHandle =<< querySelectorHandle body (selectorFromText iframeButton)
          assertEqual "text" (Just "Hello iframe") =<< textContentHandle button
          spans <- querySelectorAllHandle body (selectorFromText iframeSpans)
          assertEqual "spans" 2 (length spans)
    , testCase "should work for $eval" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          assertEqual "name" "BUTTON"
            =<< evalOnSelectorText page (selectorFromText iframeButton) "b => b.nodeName"
    , testCase "should work for $eval (handle)" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          body <- requireHandle =<< querySelector page (selectorFromText "body")
          assertEqual "name" "BUTTON"
            =<< evalOnSelectorHandleText body (selectorFromText iframeButton) "b => b.nodeName"
    , testCase "should work for $$eval" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          assertEqual "spans" ["1", "2"]
            =<< evalOnSelectorAllTexts page (selectorFromText iframeSpans) "ss => ss.map(s => s.textContent)"
    , testCase "should work for $$eval (handle)" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          body <- requireHandle =<< querySelector page (selectorFromText "body")
          assertEqual "spans" ["1", "2"]
            =<< evalOnSelectorAllHandleTexts body (selectorFromText iframeSpans) "ss => ss.map(s => s.textContent)"
    , testCase "should not allow dangling enter-frame" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          let
            button :: Locator
            button = locator page (selectorFromText "iframe >> internal:control=enter-frame")
          assertProtocolContainsAll
            ["Selector cannot end with", "iframe >> internal:control=enter-frame"]
            (click button)
    , testCase "should not allow leading enter-frame" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          assertProtocolContains
            "Selector cannot start with"
            (waitForSelector page (selectorFromText "internal:control=enter-frame >> button") WaitForVisible)
    , testCase "should not allow capturing before enter-frame" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          let
            button :: Locator
            button = locator page (selectorFromText "*css=iframe >> internal:control=enter-frame >> div")
          assertProtocolContains "Can not capture the selector before diving into the frame" (click button)
    , testCase "should capture after the enter-frame" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- gotoEmpty prefix page
          let
            divLoc :: Locator
            divLoc = locator page (selectorFromText "iframe >> internal:control=enter-frame >> *css=div >> button")
          html <- innerHtml divLoc
          assertEqual "button" True ("<button>" `isInfixOf` html)
    , localOption (mkTimeout 30000000) $
        testCase "should click in lazy iframe" $
          withAssets $ \prefix page -> do
            route page (UrlGlob "**/iframe.html") $ \handled ->
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillContentType = Just (ContentType "text/html")
                  , fulfillBody = Just (FulfillText "<html><button>Hello iframe</button></html>")
                  }
            _ <- gotoEmpty prefix page
            let
              button :: Locator
              button = locator page (selectorFromText iframeButton)
            withAsync (addLazyIframe page) $ \_ -> do
              click button
              assertEqual "text" "Hello iframe" =<< innerText button
    , localOption (mkTimeout 30000000) $
        testCase "waitFor should survive frame reattach" $
          withAssets $ \prefix page -> do
            routeIframe page
            _ <- gotoEmpty prefix page
            let
              button :: Locator
              button = locator page (selectorFromText nestedHello)
            withAsync (waitFor button WaitForVisible) $ \pending -> do
              reattachNested page
              wait pending
    , localOption (mkTimeout 30000000) $
        testCase "waitForSelector should survive frame reattach (handle)" $
          withAssets $ \prefix page -> do
            routeIframe page
            _ <- gotoEmpty prefix page
            body <- requireHandle =<< querySelector page (selectorFromText "body")
            withAsync (waitForSelectorOnHandle body (selectorFromText nestedHello) WaitForVisible) $ \pending -> do
              reattachNested page
              _ <- wait pending
              pure ()
    , localOption (mkTimeout 30000000) $
        testCase "waitForSelector should survive iframe navigation (handle)" $
          withAssets $ \prefix page -> do
            routeIframe page
            _ <- gotoEmpty prefix page
            body <- requireHandle =<< querySelector page (selectorFromText "body")
            withAsync (waitForSelectorOnHandle body (selectorFromText nestedHello) WaitForVisible) $ \pending -> do
              _ <- evaluateOnLocator (locator page (selectorFromText "iframe")) "e => { e.src = 'iframe-2.html'; }"
              _ <- wait pending
              pure ()
    , localOption (mkTimeout 30000000) $
        testCase "click should survive frame reattach" $
          withAssets $ \prefix page -> do
            routeIframe page
            _ <- gotoEmpty prefix page
            let
              button :: Locator
              button = locator page (selectorFromText nestedHello)
            withAsync (click button) $ \pending -> do
              reattachNested page
              wait pending
    , localOption (mkTimeout 30000000) $
        testCase "click should survive iframe navigation" $
          withAssets $ \prefix page -> do
            routeIframe page
            _ <- gotoEmpty prefix page
            let
              button :: Locator
              button = locator page (selectorFromText nestedHello)
            withAsync (click button) $ \pending -> do
              _ <- evaluateOnLocator (locator page (selectorFromText "iframe")) "e => { e.src = 'iframe-2.html'; }"
              wait pending
    , localOption (mkTimeout 30000000) $
        testCase "click should survive navigation" $
          withAssets $ \prefix page -> do
            routeIframe page
            _ <- goto page (urlFromText (prefix <> "/iframe.html")) defaultGotoOptions
            timeout <- requireTimeout 100
            withAsync (click (locate page "button:has-text(\"Hello nested iframe\")")) $ \pending -> do
              waitForTimeout page timeout
              _ <- goto page (urlFromText (prefix <> "/iframe-2.html")) defaultGotoOptions
              wait pending
    , testCase "should non work for non-frame" $
        withChromiumPage $ \page -> do
          routeIframe page
          setContent page "<div></div>"
          let
            button :: Locator
            button = locator page (selectorFromText "div >> internal:control=enter-frame >> button")
          assertProtocolContainsAll ["<div></div>", "<iframe> was expected"] (waitFor button WaitForVisible)
    ]

iframeButton :: Text
iframeButton =
  "iframe >> internal:control=enter-frame >> button"

iframeSpans :: Text
iframeSpans =
  "iframe >> internal:control=enter-frame >> span"

iframeCanvas :: Text
iframeCanvas =
  "iframe >> internal:control=enter-frame >> canvas"

nestedButton :: Text
nestedButton =
  "iframe >> internal:control=enter-frame >> iframe >> internal:control=enter-frame >> button"

nestedHello :: Text
nestedHello =
  "iframe >> internal:control=enter-frame >> button:has-text(\"Hello nested iframe\")"

gotoEmpty :: Text -> Page -> IO ()
gotoEmpty prefix page = do
  _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
  pure ()

routeIframe :: Page -> IO ()
routeIframe page = do
  route page (UrlGlob "**/empty.html") $ \handled ->
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody = Just (FulfillText "<iframe src=\"iframe.html\"></iframe>")
        }
  route page (UrlGlob "**/iframe.html") $ \handled ->
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody =
            Just
              ( FulfillText
                  "<html><div><button>Hello iframe</button><iframe src=\"iframe-2.html\"></iframe></div><span>1</span><span>2</span></html>"
              )
        }
  route page (UrlGlob "**/iframe-2.html") $ \handled ->
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody = Just (FulfillText "<html><button>Hello nested iframe</button></html>")
        }

reattachNested :: Page -> IO ()
reattachNested page = do
  _ <- evaluateOnLocator (locator page (selectorFromText "iframe")) "e => e.remove()"
  _ <-
    evaluate
      page
      "() => { const iframe = document.createElement('iframe'); iframe.src = 'iframe-2.html'; document.body.appendChild(iframe); }"
  pure ()

addLazyIframe :: Page -> IO ()
addLazyIframe page = do
  threadDelay 500000
  _ <- evaluate page "() => { const iframe = document.createElement('iframe'); document.body.appendChild(iframe); }"
  threadDelay 500000
  _ <- evaluate page "() => document.querySelector('iframe').src = 'iframe.html'"
  pure ()

requireTimeout :: Double -> IO Timeout
requireTimeout n =
  case milliseconds n of
    Nothing ->
      assertFailure ("milliseconds " <> show n)
    Just timeout ->
      pure timeout

assertProtocolContainsAll :: [Text] -> IO a -> IO ()
assertProtocolContainsAll needles action = do
  result <- try action
  case result of
    Left (ProtocolRpc failure) ->
      forM_ needles $ \needle ->
        if needle `isInfixOf` protocolFailureMessage failure
          then pure ()
          else
            assertFailure
              ( "expected protocol error to contain "
                  <> unpack needle
                  <> ", got: "
                  <> unpack (protocolFailureMessage failure)
              )
    Left err ->
      assertFailure ("expected ProtocolRpc, got: " <> show err)
    Right _ ->
      assertFailure "expected protocol error"
