module Playwright.PageEmulateMediaSpec
  ( tests
  ) where

import Playwright
  ( ColorScheme (ColorSchemeDark, ColorSchemeLight, ColorSchemeNoOverride)
  , EmulateMedia (emulateMediaColorScheme, emulateMediaType)
  , MediaType (MediaNoOverride, MediaPrint)
  , defaultEmulateMedia
  , emulateMedia
  , evaluateBool
  )
import Playwright.Harness
  ( withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-emulate-media"
    [ testCase "should emulate type" $
        withChromiumPage $ \page -> do
          assertEqual "screen" True =<< evaluateBool page "() => matchMedia('screen').matches"
          assertEqual "print" False =<< evaluateBool page "() => matchMedia('print').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaType = Just MediaPrint}
          assertEqual "screen after print" False =<< evaluateBool page "() => matchMedia('screen').matches"
          assertEqual "print after print" True =<< evaluateBool page "() => matchMedia('print').matches"
          emulateMedia page defaultEmulateMedia
          assertEqual "screen after empty" False =<< evaluateBool page "() => matchMedia('screen').matches"
          assertEqual "print after empty" True =<< evaluateBool page "() => matchMedia('print').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaType = Just MediaNoOverride}
          assertEqual "screen restored" True =<< evaluateBool page "() => matchMedia('screen').matches"
          assertEqual "print restored" False =<< evaluateBool page "() => matchMedia('print').matches"
    , testCase "should emulate colorScheme" $
        withChromiumPage $ \page -> do
          emulateMedia page defaultEmulateMedia {emulateMediaColorScheme = Just ColorSchemeLight}
          assertEqual "light" True =<< evaluateBool page "() => matchMedia('(prefers-color-scheme: light)').matches"
          assertEqual "dark" False =<< evaluateBool page "() => matchMedia('(prefers-color-scheme: dark)').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaColorScheme = Just ColorSchemeDark}
          assertEqual "dark on" True =<< evaluateBool page "() => matchMedia('(prefers-color-scheme: dark)').matches"
          assertEqual "light off" False =<< evaluateBool page "() => matchMedia('(prefers-color-scheme: light)').matches"
    , testCase "should default to light" $
        withChromiumPage $ \page -> do
          assertEqual "default light" True =<< evaluateBool page "() => matchMedia('(prefers-color-scheme: light)').matches"
          assertEqual "default not dark" False =<< evaluateBool page "() => matchMedia('(prefers-color-scheme: dark)').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaColorScheme = Just ColorSchemeDark}
          assertEqual "dark" True =<< evaluateBool page "() => matchMedia('(prefers-color-scheme: dark)').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaColorScheme = Just ColorSchemeNoOverride}
          assertEqual "restored light" True =<< evaluateBool page "() => matchMedia('(prefers-color-scheme: light)').matches"
    ]
