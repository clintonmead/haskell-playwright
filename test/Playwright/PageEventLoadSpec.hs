module Playwright.PageEventLoadSpec
  ( tests
  ) where

import UnliftIO.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  )
import Playwright
  ( defaultGotoOptions
  , goto
  , milliseconds
  , onLoad
  , urlFromText
  , waitForTimeout
  )
import Playwright.Harness
  ( withAssets
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
    "page-event-load"
    [ testCase "should fire once" $
        withAssets $ \prefix page -> do
          count <- newIORef (0 :: Int)
          onLoad page (modifyIORef' count (+ 1))
          _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
          case milliseconds 1000 of
            Just timeout ->
              waitForTimeout page timeout
            Nothing ->
              assertFailure "expected timeout"
          fired <- readIORef count
          assertEqual "load" 1 fired
    ]
