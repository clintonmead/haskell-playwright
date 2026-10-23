-- Skipped upstream cases (tests/library/unroute-behavior.spec.ts):
--
-- * context.unroute should not wait for pending handlers to complete:
--   Haskell unroute removes by UrlMatch glob only, with no behavior option.
-- * context.unrouteAll should wait for pending handlers to complete:
--   unrouteAll takes no behavior option ('wait'), so the wait mode is not exposed.
-- * context.unrouteAll should not wait for pending handlers to complete if behavior is ignoreErrors:
--   unrouteAll takes no behavior option ('ignoreErrors'), so the mode is not exposed.
-- * page.unroute should not wait for pending handlers to complete:
--   same as context.unroute: glob-only removal, no behavior option.
-- * page.unrouteAll should wait for pending handlers to complete:
--   same as context.unrouteAll: no behavior option.
-- * page.unrouteAll should not wait for pending handlers to complete if behavior is ignoreErrors:
--   same as context.unrouteAll: no behavior option.
-- * should not continue requests in flight (page):
--   relies on unroute and in-flight route continuation semantics the port does not expose.
-- * should not continue requests in flight (context):
--   same as the page variant, at context scope.
module Playwright.UnrouteBehaviorSpec
  ( tests
  ) where

import Control.Concurrent.STM
  ( atomically
  , retry
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( Text
  )
import Playwright
  ( Page
  , Playwright
  , Response
  , Route
  , UrlMatch (UrlMatchGlob)
  , abortRoute
  , closeContext
  , closePage
  , contextRoute
  , continueRoute
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , fallbackRoute
  , fulfillRoute
  , goto
  , responseOk
  , route
  , unrouteAll
  , unrouteContextAll
  , urlFromText
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( withAssets
  , withAssetsContextWith
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
import UnliftIO.Async
  ( async
  )
import UnliftIO.Concurrent
  ( threadDelay
  )
import UnliftIO.Exception
  ( try
  )
import UnliftIO.IORef
  ( newIORef
  , readIORef
  , writeIORef
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )

tests :: TestTree
tests =
  testGroup
    "unroute-behavior"
    [ testCase "context.unrouteAll removes all handlers" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          contextRoute ctx (UrlMatchGlob "**/*") abortRoute
          contextRoute ctx (UrlMatchGlob "**/empty.html") abortRoute
          unrouteContextAll ctx
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
    , testCase "page.unrouteAll removes all routes" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**/*") abortRoute
          route page (UrlMatchGlob "**/empty.html") abortRoute
          unrouteAll page
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
    , testCase "page.close should not wait for active route handlers on the owning context" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          arrived <- newEmptyMVar
          contextRoute ctx (UrlMatchGlob "**/*") $ \_handled -> do
            putMVar arrived ()
            hangForever
          route page (UrlMatchGlob "**/*") fallbackRoute
          ignoreGoto page (prefix <> "/empty.html")
          takeMVar arrived
          closePage page
    , testCase "context.close should not wait for active route handlers on the owned pages" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          arrived <- newEmptyMVar
          route page (UrlMatchGlob "**/*") $ \_handled -> do
            putMVar arrived ()
            hangForever
          route page (UrlMatchGlob "**/*") fallbackRoute
          ignoreGoto page (prefix <> "/empty.html")
          takeMVar arrived
          closeContext ctx
    , testCase "page.close does not wait for active route handlers" $
        withAssets $ \prefix page -> do
          secondCalled <- newIORef False
          route page (UrlMatchGlob "**/*") $ \_handled ->
            writeIORef secondCalled True
          arrived <- newEmptyMVar
          route page (UrlMatchGlob "**/*") $ \_handled -> do
            putMVar arrived ()
            hangForever
          ignoreGoto page (prefix <> "/empty.html")
          takeMVar arrived
          closePage page
          liftIO (threadDelay 500000)
          assertEqual "second handler" False =<< readIORef secondCalled
    , testCase "route.continue should not throw if page has been closed" $
        withHeldRoute $ \page handled -> do
          closePage page
          continueRoute handled
    , testCase "route.fallback should not throw if page has been closed" $
        withHeldRoute $ \page handled -> do
          closePage page
          fallbackRoute handled
    , testCase "route.fulfill should not throw if page has been closed" $
        withHeldRoute $ \page handled -> do
          closePage page
          fulfillRoute handled defaultFulfillOptions
    ]

withHeldRoute :: (Page -> Route -> Playwright ()) -> IO ()
withHeldRoute action =
  withAssets $ \prefix page -> do
    arrived <- newEmptyMVar
    route page (UrlMatchGlob "**/*") $ \handled ->
      putMVar arrived handled
    ignoreGoto page (prefix <> "/empty.html")
    handled <- takeMVar arrived
    action page handled

ignoreGoto :: Page -> Text -> Playwright ()
ignoreGoto page target = do
  _ <-
    async $ do
      _ <- try (goto page (urlFromText target) defaultGotoOptions) :: Playwright (Either PlaywrightError (Maybe Response))
      pure ()
  pure ()

hangForever :: Playwright ()
hangForever =
  liftIO (atomically retry)
