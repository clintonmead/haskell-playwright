module Playwright.PageCacheStorageSpec
  ( tests
  ) where

import Playwright
  ( defaultGotoOptions
  , evaluate
  , evaluateText
  , goto
  , reload
  , urlFromText
  )
import Playwright.Harness
  ( assertEqual
  , withAssets
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
    "page-cache-storage"
    [ testCase "CacheStorage entry should survive page.reload()" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "async () => { const cache = await caches.open('repro-cache'); await cache.put('/meta', new Response('payload')); }"
          _ <- reload page
          after <-
            evaluateText
              page
              "async () => { const cache = await caches.open('repro-cache'); const resp = await cache.match('/meta'); return resp ? await resp.text() : null; }"
          assertEqual "after reload" "payload" after
    ]
