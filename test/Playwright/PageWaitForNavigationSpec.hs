module Playwright.PageWaitForNavigationSpec
  ( tests
  ) where

import Data.Maybe
  ( isNothing
  )
import Data.Text
  ( isInfixOf
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Playwright
  ( LoadState (LoadStateCommit, LoadStateDomContentLoaded)
  , UrlMatch (UrlMatchGlob)
  , WaitForNavigationOptions (waitForNavigationOptionsWaitUntil)
  , armNavigationOptions
  , click
  , defaultGotoOptions
  , defaultWaitForNavigationOptions
  , evaluateTextWith
  , goto
  , milliseconds
  , responseOk
  , responseUrl
  , setContent
  , textArgument
  , title
  , url
  , urlFromText
  , urlToText
  , waitForNavigation
  , waitForNavigationWith
  )
import Playwright.Harness
  ( assertBool
  , locate
  , assertEqual
  , assertFailure
  , assertStillPending
  , assertWaitTimeout
  , stallingStylesheetApp
  , testAssetsDir
  , withAssetPage
  , withAssets
  , withChromiumPage
  , withLocalApp
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  )

tests :: TestTree
tests =
  testGroup
    "page-wait-for-navigation"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForNavigation page) $ \nav -> do
            _ <-
              evaluateTextWith
                page
                "url => { window.location.href = url; return url; }"
                (textArgument (prefix <> "/grid.html"))
            response <- wait nav
            case response of
              Nothing ->
                assertFailure "expected navigation response"
              Just value -> do
                assertEqual "ok" True =<< responseOk value
                url' <- responseUrl value
                assertBool "grid.html" ("grid.html" `isInfixOf` urlToText url')
    , testCase "should respect timeout" $
        withAssetPage "empty.html" $ \page ->
          case milliseconds 1000 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertWaitTimeout (waitForNavigationWith page timeout (Just (UrlMatchGlob "**/frame.html")))
    , testCase "should work with both domcontentloaded and load" $ do
        assets <- testAssetsDir
        arrived <- newEmptyMVar
        release <- newEmptyMVar
        withChromiumPage $ \page ->
          withLocalApp (stallingStylesheetApp assets arrived release) $ \prefix -> do
            takeDomContentLoaded <-
              armNavigationOptions
                page
                defaultWaitForNavigationOptions
                  {waitForNavigationOptionsWaitUntil = Just LoadStateDomContentLoaded}
            takeLoad <- armNavigationOptions page defaultWaitForNavigationOptions
            withAsync (goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions) $ \navigation -> do
              takeMVar arrived
              _ <- takeDomContentLoaded
              withAsync takeLoad $ \loadPending -> do
                assertStillPending loadPending
                putMVar release ()
                _ <- wait loadPending
                _ <- wait navigation
                pure ()
    , testCase "should work with commit" $ do
        release <- newEmptyMVar
        withChromiumPage $ \page ->
          withLocalApp (stallingScriptApp release) $ \prefix -> do
            takeCommit <-
              armNavigationOptions
                page
                defaultWaitForNavigationOptions
                  {waitForNavigationOptionsWaitUntil = Just LoadStateCommit}
            withAsync (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions) $ \navigation -> do
              _ <- takeCommit
              assertEqual "title" "Hello" =<< title page
              putMVar release ()
              _ <- wait navigation
              pure ()
    , testCase "should work with clicking on anchor links" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page "<a href='#foobar'>foobar</a>"
          takeNavigation <- armNavigationOptions page defaultWaitForNavigationOptions
          click (locate page "a")
          response <- takeNavigation
          assertEqual "no response" True (isNothing response)
          current <- url page
          assertEqual "url" (prefix <> "/empty.html#foobar") (urlToText current)
    , testCase "should work with history.pushState()" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent
            page
            "\n    <a onclick='javascript:pushState()'>SPA</a>\n    <script>\n      function pushState() { history.pushState({}, '', 'wow.html') }\n    </script>\n  "
          takeNavigation <- armNavigationOptions page defaultWaitForNavigationOptions
          click (locate page "a")
          response <- takeNavigation
          assertEqual "no response" True (isNothing response)
          current <- url page
          assertEqual "url" (prefix <> "/wow.html") (urlToText current)
    , testCase "should work with history.replaceState()" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent
            page
            "\n    <a onclick='javascript:replaceState()'>SPA</a>\n    <script>\n      function replaceState() { history.replaceState({}, '', '/replaced.html') }\n    </script>\n  "
          takeNavigation <- armNavigationOptions page defaultWaitForNavigationOptions
          click (locate page "a")
          response <- takeNavigation
          assertEqual "no response" True (isNothing response)
          current <- url page
          assertEqual "url" (prefix <> "/replaced.html") (urlToText current)
    , testCase "should work with DOM history.back()/history.forward()" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent
            page
            "\n    <a id=back onclick='javascript:goBack()'>back</a>\n    <a id=forward onclick='javascript:goForward()'>forward</a>\n    <script>\n      function goBack() { history.back(); }\n      function goForward() { history.forward(); }\n      history.pushState({}, '', '/first.html');\n      history.pushState({}, '', '/second.html');\n    </script>\n  "
          current <- url page
          assertEqual "start" (prefix <> "/second.html") (urlToText current)
          takeBack <- armNavigationOptions page defaultWaitForNavigationOptions
          click (locate page "a#back")
          backResponse <- takeBack
          assertEqual "back response" True (isNothing backResponse)
          afterBack <- url page
          assertEqual "after back" (prefix <> "/first.html") (urlToText afterBack)
          takeForward <- armNavigationOptions page defaultWaitForNavigationOptions
          click (locate page "a#forward")
          forwardResponse <- takeForward
          assertEqual "forward response" True (isNothing forwardResponse)
          afterForward <- url page
          assertEqual "after forward" (prefix <> "/second.html") (urlToText afterForward)
    ]

-- | Serves a page whose script never finishes until the test releases it.
stallingScriptApp :: MVar () -> Application
stallingScriptApp release request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html")]
            "<title>Hello</title><script src=\"script.js\"></script>"
        )
    ["script.js"] -> do
      takeMVar release
      respond (responseLBS status200 [(hContentType, "application/javascript")] "")
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] "")
