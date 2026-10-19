module Playwright.PageTransitionSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( forM_
  , unless
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text qualified as Text
import Playwright
  ( evaluate
  , onPageCrash
  , setContent
  )
import Playwright.Harness
  ( assertEqual
  , assertFailure
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( race
  )
import UnliftIO.IORef
  ( newIORef
  , readIORef
  , writeIORef
  )

tests :: TestTree
tests =
  testGroup
    "page-transition"
    [ testCase "should not crash when filter transition completes" $
        withChromiumPage $ \page -> do
          setContent
            page
            ( "<style>div { transition: filter 0.1s; } .dim { filter: brightness(0.95); }</style>"
                <> Text.concat (replicate 10 "<div>hello</div>")
            )
          crashed <- newIORef False
          onPageCrash page (writeIORef crashed True)
          forM_ [1 .. 10 :: Int] $ \_ -> do
            done <- readIORef crashed
            unless done $ do
              -- Bounded: a transitionend that never fires must fail the test, not hang the run.
              outcome <- race (liftIO (threadDelay 30000000)) (evaluate page toggleFilter)
              case outcome of
                Left () ->
                  assertFailure "transitionend never fired"
                Right _ ->
                  pure ()
          assertEqual "crashed" False =<< readIORef crashed
    ]

-- | Adds and removes the dim class on every div, waiting for each transition.
-- Failures are swallowed, as in the upstream test.
toggleFilter :: Text.Text
toggleFilter =
  "() => (async () => { const divs = [...document.querySelectorAll('div')];"
    <> " const settled = () => Promise.all(divs.map(div => new Promise(f => div.addEventListener('transitionend', f, { once: true }))));"
    <> " let promise = settled(); divs.forEach(div => div.classList.add('dim')); await promise;"
    <> " promise = settled(); divs.forEach(div => div.classList.remove('dim')); await promise;"
    <> " })().catch(() => {})"
