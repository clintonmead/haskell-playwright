module Playwright.LocatorFrameSpec
  ( tests
  ) where

import Playwright
  ( ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , Page
  , UrlGlob (UrlGlob)
  , WaitForState (WaitForVisible)
  , childFrameLocator
  , childLocator
  , click
  , defaultFulfillOptions
  , defaultGotoOptions
  , frameLocator
  , fulfillRoute
  , goto
  , isHidden
  , isVisible
  , innerText
  , route
  , selectorFromText
  , urlFromText
  , waitFor
  , Playwright
  )
import Playwright.Harness
  ( withAssets
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
    ]

routeIframe :: Page -> Playwright ()
routeIframe page = do
  route page (UrlGlob "**/empty.html") $ \handled ->
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody = Just (FulfillText "<iframe src=\"iframe.html\" name=\"frame1\"></iframe>")
        }
  route page (UrlGlob "**/iframe.html") $ \handled ->
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
  route page (UrlGlob "**/iframe-2.html") $ \handled ->
    fulfillRoute
      handled
      defaultFulfillOptions
        { fulfillContentType = Just (ContentType "text/html")
        , fulfillBody = Just (FulfillText "<html><button>Hello nested iframe</button></html>")
        }
