module Playwright.LocatorFrameSpec
  ( tests
  ) where

import Playwright
  ( ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , Page
  , UrlMatch (UrlMatchGlob)
  , WaitForState (WaitForVisible)
  , childFrameLocator
  , childLocator
  , click
  , ClickOptions (clickOptionsTimeout)
  , clickWith
  , defaultClickOptions
  , milliseconds
  , setContent
  , evaluate
  , defaultFulfillOptions
  , defaultGotoOptions
  , frameLocator
  , fulfillRoute
  , goto
  , isHidden
  , isVisible
  , innerText
  , locatorCount
  , route
  , selectorFromText
  , urlFromText
  , waitFor
  , Playwright
  )
import Control.Concurrent
  ( threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import UnliftIO.Async
  ( withAsync
  )
import Playwright.Harness
  ( withAssets
  , assertEqual
  , assertProtocolContains
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
    "locator-frame"
    [ testCase "should work for iframe" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            button =
              childLocator
                (frameLocator page (selectorFromText "iframe"))
                (selectorFromText "button")
          waitFor button WaitForVisible
          assertEqual "text" "Hello iframe" =<< innerText button
          click button
    , testCase "should work for nested iframe" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            button =
              childLocator
                ( childFrameLocator
                    (frameLocator page (selectorFromText "iframe"))
                    (selectorFromText "iframe")
                )
                (selectorFromText "button")
          waitFor button WaitForVisible
          assertEqual "nested" "Hello nested iframe" =<< innerText button
    , testCase "should not wait for frame" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          -- No frame exists, so the locator resolves to nothing and reports hidden at once.
          let spanLocator = childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "span")
          assertEqual "hidden" True =<< isHidden spanLocator
    , testCase "should not wait for frame 2" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let spanLocator = childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "span")
          assertEqual "not visible" False =<< isVisible spanLocator
    , testCase "should work for $ and $$" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            frame = frameLocator page (selectorFromText "iframe")
            button = childLocator frame (selectorFromText "button")
            spans = childLocator frame (selectorFromText "span")
          waitFor button WaitForVisible
          assertEqual "button" "Hello iframe" =<< innerText button
          assertEqual "spans" 2 =<< locatorCount spans
    , testCase "should wait for frame" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            spanLocator = childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "span")
          assertProtocolContains
            "Timeout 1000ms exceeded"
            (clickWith spanLocator defaultClickOptions {clickOptionsTimeout = milliseconds 1000})
    , testCase "should wait for frame 2" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            button = childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "button")
          withAsync
            (liftIO (threadDelay 300000) >> goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
            $ \_ -> click button
    , testCase "should non work for non-frame" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- setContent page "<div></div>"
          let
            button = childLocator (frameLocator page (selectorFromText "div")) (selectorFromText "button")
          assertProtocolContains "<iframe> was expected" (waitFor button WaitForVisible)
    , testCase "should click in lazy iframe" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/iframe.html") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "text/html")
                , fulfillBody = Just (FulfillText "<html><button>Hello iframe</button></html>")
                }
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            button = childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "button")
          withAsync
            ( do
                liftIO (threadDelay 500000)
                _ <- evaluate page "() => { const iframe = document.createElement('iframe'); document.body.appendChild(iframe); }"
                liftIO (threadDelay 500000)
                _ <- evaluate page "() => { document.querySelector('iframe').src = 'iframe.html'; }"
                pure ()
            )
            $ \_ -> do
              click button
              assertEqual "text" "Hello iframe" =<< innerText button
    ]

routeIframe :: Page -> Playwright ()
routeIframe page = do
  route page (UrlMatchGlob "**/empty.html") $ \handled ->
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody = Just (FulfillText "<iframe src=\"iframe.html\" name=\"frame1\"></iframe>")
        }
  route page (UrlMatchGlob "**/iframe.html") $ \handled ->
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody =
            Just
              ( FulfillText
                  "<html><div><button data-testid=\"buttonId\">Hello iframe</button><iframe src=\"iframe-2.html\"></iframe></div><span>1</span><span>2</span></html>"
              )
        }
  route page (UrlMatchGlob "**/iframe-2.html") $ \handled ->
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody = Just (FulfillText "<html><button>Hello nested iframe</button></html>")
        }
