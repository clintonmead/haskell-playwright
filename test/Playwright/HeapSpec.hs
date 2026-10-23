module Playwright.HeapSpec
  ( tests
  ) where

-- Port of upstream library/heap.spec.ts, behavioural cases only.
--
-- Ported:
--   * "cycle handles"
--
-- Not ported:
--   * "test #${i} to request page and context" and "test to request page and context":
--     upstream fixture-only placeholders that exist to create a page instance for the
--     heap counters; they assert nothing.
--   * "should not leak fixtures w/ page", "should not leak fixtures w/o page":
--     count JS client objects (Page, BrowserContext, Browser) in the test runner's own
--     V8 heap through the inspector. The Haskell client has no such in-process objects.
--   * "should not leak server-side objects", "should not leak dispatchers after closing page",
--     "should not leak workers": count driver-side server objects and dispatchers through the
--     inspector and the upstream-only `coreServer` import. The driver is a separate Node
--     process, so these counters are not reachable from Haskell.
--   * "should collect stale handles": needs the driver's `setMaxDispatchersForTest` hook,
--     the "object has been collected" error, and client and server object counts.
--   * "should collect frames": needs a `frameattached` handler that awaits `page.title()`
--     while 310 iframes are added. Not verified against the Haskell event handler API, so
--     deferred rather than forced.

import Control.Monad
  ( forM_
  )
import Data.Text
  ( Text
  )
import Playwright
  ( querySelectorAll
  , querySelectorHandle
  , selectorFromText
  , setContent
  , textContentHandle
  )
import Playwright.Harness
  ( assertEqual
  , requireHandle
  , withChromiumPage
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
    "heap"
    [ testCase "cycle handles" $
        withChromiumPage $ \page -> do
          setContent page (divMarkup 2000)
          divs <- querySelectorAll page (selectorFromText "div")
          assertEqual "div count" 2000 (length divs)
          forM_ divs $ \element -> do
            span' <- requireHandle =<< querySelectorHandle element (selectorFromText "span")
            text <- textContentHandle span'
            assertEqual "span text" (Just "hi") text
    ]

divMarkup :: Int -> Text
divMarkup n = mconcat (replicate n "<div><span>hi</span></div>")
