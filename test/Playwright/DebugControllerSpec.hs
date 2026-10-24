module Playwright.DebugControllerSpec
  ( tests
  ) where

-- Port of upstream library/debug-controller.spec.ts, behavioural cases only.
--
-- Ported:
--   * "should report error in aria template"
--
-- Not ported (each needs a connected browser through `browserType.connect`, which the
-- Haskell client does not implement yet: upstream reaches the browser via
-- `localUtils.connect`, a JsonPipe and a nested Connection):
--   * "should pick element", "should report pages", "should reset for reuse",
--     "should highlight all", "should record", "should record expect signal",
--     "should record custom data-testid", "should not leak actions from the previous
--     recording session", "should reset routes before reuse", "should highlight inside
--     iframe", "should highlight aria template": each opens a page through
--     `connectedBrowser.newContextForReuse()`.

import Data.Text qualified as Text
import Playwright
  ( debugHighlight
  , debugInitialize
  , HighlightTarget (HighlightAriaTemplate)
  , withDebugController
  )
import Playwright.Driver
  ( findDriver
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertBool
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "debug-controller"
    [ testCase "should report error in aria template" $ do
        paths <- findDriver
        withDebugController paths $ \dc -> do
          initResult <- debugInitialize dc
          assertBool "initialize succeeds" (either (const False) (const True) initResult)
          result <- debugHighlight dc (HighlightAriaTemplate "- button \"Submit")
          case result of
            Left message ->
              assertBool
                ("error mentions unterminated string, got: " <> show message)
                (Text.pack "Unterminated string:" `Text.isInfixOf` message)
            Right _ -> assertFailure "expected highlight to fail for an unterminated aria template"
    ]
