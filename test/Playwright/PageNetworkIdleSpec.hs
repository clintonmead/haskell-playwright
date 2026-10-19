module Playwright.PageNetworkIdleSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Network.HTTP.Types
  ( status404
  )
import Network.Wai
  ( Application
  , ResponseReceived
  , pathInfo
  , responseLBS
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Playwright
  ( GotoOptions (gotoOptionsWaitUntil)
  , HttpStatus (HttpStatus)
  , LoadState (LoadStateNetworkIdle)
  , Playwright
  , defaultGotoOptions
  , evaluate
  , goto
  , responseStatus
  , urlFromText
  )
import Playwright.Harness
  ( assertStillPending
  , testAssetsDir
  , withAssets
  , withChromiumPage
  , withLocalApp
  , assertEqual
  , assertFailure
  )
import UnliftIO.Async
  ( race
  , wait
  , withAsync
  )
import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  , tryPutMVar
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
    "page-network-idle"
    [ testCase "should navigate to empty page with networkidle" $
        withAssets $ \prefix page -> do
          response <-
            goto
              page
              (urlFromText (prefix <> "/empty.html"))
              defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateNetworkIdle}
          case response of
            Just value ->
              assertEqual "status" (HttpStatus 200) =<< responseStatus value
            Nothing ->
              assertFailure "expected navigation response"
    , testCase "should wait for networkidle to succeed navigation" $
        withChromiumPage $ \page -> do
          assets <- testAssetsDir
          requestedA <- newEmptyMVar
          releaseA <- newEmptyMVar
          requestedB <- newEmptyMVar
          releaseB <- newEmptyMVar
          withLocalApp (networkIdleApp assets (requestedA, releaseA) (requestedB, releaseB)) $ \prefix ->
            withAsync
              (goto page (urlFromText (prefix <> "/networkidle.html")) defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateNetworkIdle})
              $ \pending -> do
                awaitSignal requestedA
                assertStillPending pending
                _ <- evaluate page "() => window.fetchSecond()"
                awaitSignal requestedB
                assertStillPending pending
                -- Answering the held requests lets the page go idle.
                putMVar releaseA ()
                putMVar releaseB ()
                response <- wait pending
                case response of
                  Nothing ->
                    assertFailure "expected navigation response"
                  Just value ->
                    assertEqual "status" (HttpStatus 200) =<< responseStatus value
    ]

-- | Waits for a held request to arrive. A request that never arrives fails the
-- test instead of hanging the run.
awaitSignal :: MVar () -> Playwright ()
awaitSignal signal = do
  outcome <- race (liftIO (threadDelay 30000000)) (takeMVar signal)
  case outcome of
    Left () ->
      assertFailure "expected request never arrived"
    Right () ->
      pure ()

-- | Serves the networkidle page and its scripts. The two fetch requests are
-- held until the test releases them, and each one signals that it arrived.
networkIdleApp :: FilePath -> (MVar (), MVar ()) -> (MVar (), MVar ()) -> Application
networkIdleApp assets (requestedA, releaseA) (requestedB, releaseB) request respond =
  case pathInfo request of
    ["fetch-request-a.js"] ->
      held requestedA releaseA
    ["fetch-request-b.js"] ->
      held requestedB releaseB
    _ ->
      staticApp (defaultFileServerSettings assets) request respond
  where
    held :: MVar () -> MVar () -> IO ResponseReceived
    held requested release = do
      _ <- tryPutMVar requested ()
      _ <- takeMVar release
      respond (responseLBS status404 [] (LBS8.pack "File not found"))
