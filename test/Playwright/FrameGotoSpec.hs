module Playwright.FrameGotoSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Network.HTTP.Types
  ( status200
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
import Data.Text
  ( isInfixOf
  )
import Playwright
  ( GotoOptions (gotoOptionsTimeout, gotoOptionsWaitUntil)
  , LoadState (LoadStateNetworkIdle)
  , Playwright
  , Response
  , currentUrl
  , milliseconds
  , defaultGotoOptions
  , frames
  , framesEqual
  , goto
  , gotoFrame
  , responseFrame
  , responseOk
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( testAssetsDir
  , withChromiumPage
  , withLocalApp
  , withAssets
  , assertBool
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
