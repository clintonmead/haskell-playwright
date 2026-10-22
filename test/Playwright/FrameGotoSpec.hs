module Playwright.FrameGotoSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  )
import UnliftIO.Async
  ( async
  , wait
  , withAsync
  )
import Control.Monad
  ( forM_
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import UnliftIO.Exception
  ( try
  )
import Data.Text qualified as Text
import Data.Text
  ( Text
  , isInfixOf
  , pack
  )
import Playwright
  ( GotoOptions (gotoOptionsTimeout, gotoOptionsWaitUntil)
  , LoadState (LoadStateNetworkIdle)
  , Playwright
  , Response
  , UrlMatch (UrlMatchGlob)
  , currentUrl
  , defaultGotoOptions
  , evaluateText
  , frames
  , framesEqual
  , goto
  , gotoFrame
  , milliseconds
  , responseFrame
  , responseOk
  , responseText
  , route
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , attachFrame
  , testAssetsDir
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

tests :: TestTree
tests =
  testGroup
    "frame-goto"
    [ testCase "should navigate subframes" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          allFrames <- frames page
          case allFrames of
            [parentFrame, childFrame] -> do
              parentUrl <- urlToText <$> currentUrl parentFrame
              childUrl <- urlToText <$> currentUrl childFrame
              assertBool "parent" ("/frames/one-frame.html" `isInfixOf` parentUrl)
              assertBool "child" ("/frames/frame.html" `isInfixOf` childUrl)
              response <-
                gotoFrame childFrame (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
              case response of
                Just value -> do
                  assertEqual "ok" True =<< responseOk value
                  frame <- responseFrame value
                  assertEqual "response frame" True (maybe False (framesEqual childFrame) frame)
                Nothing ->
                  assertFailure "expected navigation response"
            _ ->
              assertFailure ("expected 2 frames, got " <> show (length allFrames))
    , testCase "should continue after client redirect" $ do
        assets <- testAssetsDir
        withChromiumPage $ \page ->
          withLocalApp (stallingScriptApp assets) $ \prefix -> do
            let
              url = prefix <> "/frames/child-redirect.html"
            case milliseconds 5000 of
              Nothing ->
                assertFailure "milliseconds 5000"
              Just timeout -> do
                result <-
                  try (goto page (urlFromText url) defaultGotoOptions {gotoOptionsTimeout = Just timeout, gotoOptionsWaitUntil = Just LoadStateNetworkIdle})
                    :: Playwright (Either PlaywrightError (Maybe Response))
                case result of
                  Left (ProtocolRpc failure) -> do
                    assertBool "timeout" ("Timeout 5000ms exceeded" `isInfixOf` protocolFailureMessage failure)
                    assertBool "waiting until networkidle" ("waiting until \"networkidle\"" `isInfixOf` protocolFailureMessage failure)
                  Left err ->
                    assertFailure ("expected ProtocolRpc, got: " <> show err)
                  Right _ ->
                    assertFailure "expected the navigation to time out"
    , testCase "should reject when frame detaches" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          held <- newEmptyMVar
          route page (UrlMatchGlob (prefix <> "/one-style.css")) $ \handled -> putMVar held handled
          allFrames <- frames page
          case drop 1 allFrames of
            [] ->
              assertFailure "expected a child frame"
            child : _ ->
              withAsync
                (try (gotoFrame child (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions) :: Playwright (Either PlaywrightError (Maybe Response)))
                $ \navigation -> do
                  _ <- takeMVar held
                  _ <- evaluateText page "() => { document.querySelector('iframe').remove(); return 'removed'; }"
                  result <- wait navigation
                  case result of
                    Left (ProtocolRpc failure) ->
                      assertBool
                        "detached"
                        ( "net::ERR_ABORTED" `isInfixOf` protocolFailureMessage failure
                            || "frame was detached" `isInfixOf` Text.toLower (protocolFailureMessage failure)
                        )
                    Left err ->
                      assertFailure ("expected ProtocolRpc, got: " <> show err)
                    Right _ ->
                      assertFailure "expected the navigation to fail"
    , testCase "should return matching responses" $ do
        arrivals <- newEmptyMVar
        releases <- mapM (const newEmptyMVar) [0 :: Int, 1, 2]
        withApplication (pure (heldPagesApp arrivals releases)) $ \port ->
          withChromiumPage $ \page -> do
            let
              base = "http://127.0.0.1:" <> pack (show port)
            _ <- goto page (urlFromText (base <> "/empty.html")) defaultGotoOptions
            children <- mapM (\i -> attachFrame page ("frame" <> pack (show i)) (base <> "/empty.html")) [0 :: Int, 1, 2]
            navigations <-
              mapM
                ( \(i, child) -> do
                    pending <-
                      async (gotoFrame child (urlFromText (base <> "/" <> pack (show i) <> ".html")) defaultGotoOptions)
                    arrived <- takeMVar arrivals
                    assertEqual "arrival" i arrived
                    pure (i, child, pending)
                )
                (zip [0 :: Int, 1, 2] children)
            forM_ [1, 2, 0] $ \i ->
              case [(child, pending) | (j, child, pending) <- navigations, j == i] of
                [(child, pending)] -> do
                  putMVar (releases !! i) ()
                  response <- wait pending
                  case response of
                    Nothing -> assertFailure "missing response"
                    Just value -> do
                      frame <- responseFrame value
                      assertEqual "frame" True (maybe False (`framesEqual` child) frame)
                      assertEqual "text" (["AAA", "BBB", "CCC"] !! i) =<< responseText value
                _ -> assertFailure "missing navigation"
    ]

-- | Serves the assets, except that frames/script.js answers only after a long
-- delay, so a navigation waiting for the network to go idle times out.
stallingScriptApp :: FilePath -> Application
stallingScriptApp assets request respond =
  case pathInfo request of
    ["frames", "script.js"] -> do
      threadDelay 10000000
      respond (responseLBS status200 [] LBS8.empty)
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

-- | Records each request to /0.html, /1.html and /2.html as it arrives, then
-- holds the response until the test releases it with the matching MVar.
heldPagesApp :: MVar Int -> [MVar ()] -> Application
heldPagesApp arrivals releases request respond =
  case pathInfo request of
    [name] | Just index <- heldIndex name -> do
      putMVar arrivals index
      takeMVar (releases !! index)
      respond (responseLBS status200 [(hContentType, "text/plain")] (LBS8.pack (["AAA", "BBB", "CCC"] !! index)))
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] LBS8.empty)
  where
    heldIndex :: Text -> Maybe Int
    heldIndex name = lookup name [("0.html", 0), ("1.html", 1), ("2.html", 2)]
