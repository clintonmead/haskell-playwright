module Playwright.PageFileChooserSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Playwright
  ( armFileChooser
  , click
  , evaluate
  , fileChooserMultiple
  , milliseconds
  , setContent
  , setDefaultTimeout
  , waitForFileChooser
  , waitForFileChooserWith
  )
import Playwright.Harness
  ( assertWaitTimeout
  , locate
  , withChromiumPage
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
    "page-filechooser"
    [ testCase "should emit event once" $
        withChromiumPage $ \page -> do
          setContent page "<input type=file>"
          takeChooser <- armFileChooser page
          withAsync takeChooser $ \handle -> do
            click (locate page "input")
            chooser <- wait handle
            assertEqual "single" False (fileChooserMultiple chooser)
    , testCase "should respect default timeout when there is no custom timeout" $
        withChromiumPage $ \page ->
          case milliseconds 1 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              setDefaultTimeout page timeout
              assertWaitTimeout (waitForFileChooser page)
    , testCase "should prioritize exact timeout over default timeout" $
        withChromiumPage $ \page ->
          case (milliseconds 0, milliseconds 1) of
            (Just disabled, Just timeout) -> do
              setDefaultTimeout page disabled
              assertWaitTimeout (waitForFileChooserWith page timeout)
            _ ->
              assertFailure "timeout"
    , testCase "should work with no timeout" $
        withChromiumPage $ \page ->
          case milliseconds 0 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              withAsync (waitForFileChooserWith page timeout) $ \pending -> do
                _ <-
                  evaluate
                    page
                    "() => { window.setTimeout(() => { const el = document.createElement('input'); el.type = 'file'; el.click(); }, 50); }"
                chooser <- wait pending
                assertEqual "chooser" False (fileChooserMultiple chooser)
    ]
