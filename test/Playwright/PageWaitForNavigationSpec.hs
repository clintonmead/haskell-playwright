module Playwright.PageWaitForNavigationSpec
  ( tests
  ) where

import Data.Maybe
  ( isNothing
  )
import Data.Text
  ( isInfixOf
  , replace
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
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
  ( Frame
  , LoadState (LoadStateCommit, LoadStateDomContentLoaded)
  , Page
  , Playwright
  , Response
  , UrlMatch (UrlMatchGlob, UrlMatchRegex)
  , WaitForNavigationOptions (waitForNavigationOptionsUrl, waitForNavigationOptionsWaitUntil)
  , armNavigationFrame
  , childFrames
  , evaluateOnFrame
  , evaluateOnFrameText
  , evaluateText
  , framesEqual
  , mainFrame
  , responseFrame
  , waitForNavigationFrame
  , waitForNavigationOptions
  , armNavigationOptions
  , click
  , defaultGotoOptions
  , defaultWaitForNavigationOptions
  , evaluate
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
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( assertBool
  , attachFrame
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
import UnliftIO.Exception
  ( try
  )
import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  )

-- Skipped: "should work with clicking on links which do not commit navigation".
-- The upstream case links to an https fixture and expects waitForNavigation to
-- reject with the SSL certificate error. This harness has no TLS fixture. A
-- refused-port link (https://127.0.0.1:1) was tried as a stand-in, but there the
-- driver resolves waitForNavigation with no response and no error, so the
-- upstream rejection cannot be reproduced.
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
    , testCase "should work with url match for same document navigations" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let options = defaultWaitForNavigationOptions {waitForNavigationOptionsUrl = Just (UrlMatchRegex "third\\.html" "")}
          withAsync (waitForNavigationOptions page options) $ \pending -> do
            assertStillPending pending
            _ <- evaluate page "() => { history.pushState({}, '', '/first.html'); }"
            assertStillPending pending
            _ <- evaluate page "() => { history.pushState({}, '', '/second.html'); }"
            assertStillPending pending
            _ <- evaluate page "() => { history.pushState({}, '', '/third.html'); }"
            _ <- wait pending
            pure ()
    , testCase "should work on frame" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          child <- firstChild page
          withAsync (waitForNavigationFrame child) $ \nav -> do
            _ <-
              evaluateOnFrameText
                child
                ("() => { window.location.href = '" <> prefix <> "/grid.html'; return ''; }")
            response <- wait nav
            case response of
              Nothing ->
                assertFailure "expected navigation response"
              Just value -> do
                assertEqual "ok" True =<< responseOk value
                url' <- responseUrl value
                assertBool "grid.html" ("grid.html" `isInfixOf` urlToText url')
                frame <- responseFrame value
                assertEqual "response frame" True (maybe False (framesEqual child) frame)
            current <- url page
            assertBool "page url" ("/frames/one-frame.html" `isInfixOf` urlToText current)
    , testCase "should fail when frame detaches" $ do
        assets <- testAssetsDir
        arrived <- newEmptyMVar
        release <- newEmptyMVar
        withChromiumPage $ \page ->
          withLocalApp (stallingStylesheetApp assets arrived release) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            frame <- attachFrame page "frame1" (prefix <> "/empty.html")
            takeNavigation <- armNavigationFrame frame defaultWaitForNavigationOptions
            withAsync takeNavigation $ \pending -> do
              _ <-
                evaluateTextWith
                  page
                  "url => { document.querySelector('iframe').src = url; return url; }"
                  (textArgument (prefix <> "/one-style.html"))
              takeMVar arrived
              _ <- evaluate page "() => setTimeout(() => document.querySelector('iframe').remove(), 0)"
              result <-
                try (wait pending)
                  :: Playwright (Either PlaywrightError (Maybe Response))
              putMVar release ()
              case result of
                Left (ProtocolRpc failure) -> do
                  let
                    message = protocolFailureMessage failure
                  assertBool "waiting" ("waiting for navigation until \"load\"" `isInfixOf` message)
                  assertBool "detached" ("frame was detached" `isInfixOf` message)
                Left err ->
                  assertFailure ("expected ProtocolRpc, got: " <> show err)
                Right _ ->
                  assertFailure "expected the frame detach to fail the wait"
    , testCase "should work when subframe issues window.stop()" $ do
        assets <- testAssetsDir
        arrived <- newEmptyMVar
        release <- newEmptyMVar
        withChromiumPage $ \page ->
          withLocalApp (stallingFrameStyleApp assets arrived release) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            withAsync (goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions) $ \navigation -> do
              takeMVar arrived
              frame <- firstChild page
              _ <- evaluateOnFrame frame "() => window.stop()"
              _ <- wait navigation
              pure ()
            putMVar release ()
    , testCase "should work for cross-process navigations" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let target = replace "127.0.0.1" "localhost" prefix <> "/empty.html"
          takeNavigation <-
            armNavigationOptions
              page
              defaultWaitForNavigationOptions
                {waitForNavigationOptionsWaitUntil = Just LoadStateDomContentLoaded}
          withAsync (goto page (urlFromText target) defaultGotoOptions) $ \navigation -> do
            response <- takeNavigation
            case response of
              Nothing ->
                assertFailure "expected navigation response"
              Just value -> do
                responseTarget <- urlToText <$> responseUrl value
                assertEqual "response url" target responseTarget
            pageTarget <- urlToText <$> url page
            assertEqual "page url" target pageTarget
            assertEqual "location href" target =<< evaluateText page "() => document.location.href"
            _ <- wait navigation
            pure ()
    ]

-- | Serves the test assets, but holds /frames/style.css until the test releases it.
-- The app puts to arrived when the frame's stylesheet request comes in.
stallingFrameStyleApp :: FilePath -> MVar () -> MVar () -> Application
stallingFrameStyleApp assets arrived release =
  let
    static = staticApp (defaultFileServerSettings assets)
  in
    \request respond ->
      case pathInfo request of
        ["frames", "style.css"] -> do
          putMVar arrived ()
          takeMVar release
          respond (responseLBS status200 [(hContentType, "text/css")] "")
        _ ->
          static request respond

firstChild :: Page -> Playwright Frame
firstChild page = do
  children <- childFrames (mainFrame page)
  case children of
    child : _ -> pure child
    [] -> assertFailure "expected a child frame"

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
