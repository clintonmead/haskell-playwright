module Playwright.PageNetworkIdleSpec
  ( tests
  ) where

import Playwright
  ( GotoOptions (gotoOptionsWaitUntil)
  , HttpStatus (HttpStatus)
  , LoadState (LoadStateNetworkIdle)
  , defaultGotoOptions
  , goto
  , responseStatus
  , urlFromText
  )
import Playwright.Harness
  ( withAssets
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
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
    ]
