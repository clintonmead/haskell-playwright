module Playwright.LocatorFrameSpec
  ( tests
  ) where

import Playwright
  ( ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , Page
  , UrlMatch (UrlMatchGlob)
  , WaitForState (WaitForAttached, WaitForDetached, WaitForHidden, WaitForVisible)
  , waitForWith
  , childFrameLocator
  , childLocator
  , click
  , contentFrame
  , frameLocatorOwner
  , getAttribute
  , locator
  , first
  , lastLocator
  , nth
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
  ( wait
  , withAsync
  )
import Playwright.Harness
  ( withAssets
  , assertEqual
  , assertProtocolContains
  , requireTimeout
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
    , testCase "locator.contentFrame should work" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            button = childLocator (contentFrame (locator page (selectorFromText "iframe"))) (selectorFromText "button")
          assertEqual "text" "Hello iframe" =<< innerText button
          click button
    , testCase "frameLocator.owner should work" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let owner = frameLocatorOwner (frameLocator page (selectorFromText "iframe"))
          waitFor owner WaitForVisible
          assertEqual "name" (Just "frame1") =<< getAttribute owner "name"
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
    , testCase "wait for hidden should succeed when frame is not in dom" $
        withAssets $ \_ page -> do
          _ <- goto page (urlFromText "about:blank") defaultGotoOptions
          let
            button = childLocator (frameLocator page (selectorFromText "iframe1")) (selectorFromText "button")
          assertEqual "hidden" True =<< isHidden button
          waitFor button WaitForHidden
          waitFor button WaitForDetached
          timeout <- requireTimeout 1000
          assertProtocolContains "Timeout 1000ms exceeded" (waitForWith button timeout WaitForAttached)
    , testCase "locator.frameLocator should throw on ambiguity" $
        withAssets $ \prefix page -> do
          routeAmbiguous page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            button = childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "button")
          assertProtocolContains "strict mode violation" (waitFor button WaitForVisible)
    , testCase "should wait for frame to go" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let button = childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "button")
          withAsync
            ( do
                liftIO (threadDelay 300000)
                _ <- evaluate page "() => document.querySelector('iframe').remove()"
                pure ()
            )
            $ \_ -> waitFor button WaitForHidden
    , testCase "waitFor should survive frame reattach" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let button = childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "button:has-text(\"Hello nested iframe\")")
          withAsync (waitFor button WaitForVisible) $ \promise -> do
            _ <- evaluate page "() => document.querySelector('iframe').remove()"
            _ <- evaluate page "() => { const iframe = document.createElement('iframe'); iframe.src = 'iframe-2.html'; document.body.appendChild(iframe); }"
            wait promise
    , testCase "click should survive frame reattach" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let button = childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "button:has-text(\"Hello nested iframe\")")
          withAsync (click button) $ \promise -> do
            _ <- evaluate page "() => document.querySelector('iframe').remove()"
            _ <- evaluate page "() => { const iframe = document.createElement('iframe'); iframe.src = 'iframe-2.html'; document.body.appendChild(iframe); }"
            wait promise
    , testCase "click should survive iframe navigation" $
        withAssets $ \prefix page -> do
          routeIframe page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let button = childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "button:has-text(\"Hello nested iframe\")")
          withAsync (click button) $ \promise -> do
            _ <- evaluate page "() => { document.querySelector('iframe').src = 'iframe-2.html'; }"
            wait promise
    , testCase "locator.frameLocator should not throw on first/last/nth" $
        withAssets $ \prefix page -> do
          routeAmbiguous page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            frames = childLocator (locator page (selectorFromText "body")) (selectorFromText "iframe")
            button1 = childLocator (contentFrame (first frames)) (selectorFromText "button")
            button2 = childLocator (contentFrame (nth frames 1)) (selectorFromText "button")
            button3 = childLocator (contentFrame (lastLocator frames)) (selectorFromText "button")
          waitFor button1 WaitForVisible
          assertEqual "first" "Hello from iframe-1.html" =<< innerText button1
          waitFor button2 WaitForVisible
          assertEqual "second" "Hello from iframe-2.html" =<< innerText button2
          waitFor button3 WaitForVisible
          assertEqual "last" "Hello from iframe-3.html" =<< innerText button3
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

-- | Serves three iframes on the page, each with its own button.
routeAmbiguous :: Page -> Playwright ()
routeAmbiguous page = do
  route page (UrlMatchGlob "**/empty.html") $ \handled ->
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody =
            Just
              ( FulfillText
                  "<iframe src=\"iframe-1.html\"></iframe><iframe src=\"iframe-2.html\"></iframe><iframe src=\"iframe-3.html\"></iframe>"
              )
        }
  route page (UrlMatchGlob "**/iframe-1.html") $ \handled ->
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody = Just (FulfillText "<html><button>Hello from iframe-1.html</button></html>")
        }
  route page (UrlMatchGlob "**/iframe-2.html") $ \handled ->
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody = Just (FulfillText "<html><button>Hello from iframe-2.html</button></html>")
        }
  route page (UrlMatchGlob "**/iframe-3.html") $ \handled ->
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody = Just (FulfillText "<html><button>Hello from iframe-3.html</button></html>")
        }
