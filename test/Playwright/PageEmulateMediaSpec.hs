module Playwright.PageEmulateMediaSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Playwright
  ( ColorScheme (ColorSchemeDark, ColorSchemeLight, ColorSchemeNoOverride)
  , Contrast (ContrastMore, ContrastNoOverride, ContrastNoPreference)
  , EmulateMedia (emulateMediaColorScheme, emulateMediaContrast, emulateMediaForcedColors, emulateMediaReducedMotion, emulateMediaType)
  , ForcedColors (ForcedColorsActive, ForcedColorsNone, ForcedColorsNoOverride)
  , MediaType (MediaNoOverride, MediaPrint)
  , ReducedMotion (ReducedMotionNoOverride, ReducedMotionNoPreference, ReducedMotionReduce)
  , defaultEmulateMedia
  , defaultGotoOptions
  , emulateMedia
  , evalOnSelectorText
  , evaluateBool
  , goto
  , selectorFromText
  , setContent
  , urlFromText
  )
import Playwright.Harness
  ( withAssets
  , withChromiumPage
  , assertEqual
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
    , testCase "should work during navigation" $
        withAssets $ \prefix page -> do
          emulateMedia page defaultEmulateMedia {emulateMediaColorScheme = Just ColorSchemeLight}
          withAsync (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions) $ \pending -> do
            mapM_
              ( \i -> do
                  let
                    scheme =
                      if even i
                        then ColorSchemeDark
                        else ColorSchemeLight
                  emulateMedia page defaultEmulateMedia {emulateMediaColorScheme = Just scheme}
              )
              [0 .. 8 :: Int]
            _ <- wait pending
            assertEqual "dark" True =<< evaluateBool page "() => matchMedia('(prefers-color-scheme: dark)').matches"
    , testCase "should change the actual colors in css" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>@media (prefers-color-scheme: dark) { div { background: black; color: white; } } @media (prefers-color-scheme: light) { div { background: white; color: black; } }</style><div>Hello</div>"
          emulateMedia page defaultEmulateMedia {emulateMediaColorScheme = Just ColorSchemeLight}
          assertEqual "light" "rgb(255, 255, 255)"
            =<< evalOnSelectorText page (selectorFromText "div") "div => window.getComputedStyle(div).backgroundColor"
          emulateMedia page defaultEmulateMedia {emulateMediaColorScheme = Just ColorSchemeDark}
          assertEqual "dark" "rgb(0, 0, 0)"
            =<< evalOnSelectorText page (selectorFromText "div") "div => window.getComputedStyle(div).backgroundColor"
          emulateMedia page defaultEmulateMedia {emulateMediaColorScheme = Just ColorSchemeLight}
          assertEqual "light again" "rgb(255, 255, 255)"
            =<< evalOnSelectorText page (selectorFromText "div") "div => window.getComputedStyle(div).backgroundColor"
    , testCase "should emulate reduced motion" $
        withChromiumPage $ \page -> do
          assertEqual "default" True
            =<< evaluateBool page "() => matchMedia('(prefers-reduced-motion: no-preference)').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaReducedMotion = Just ReducedMotionReduce}
          assertEqual "reduce" True
            =<< evaluateBool page "() => matchMedia('(prefers-reduced-motion: reduce)').matches"
          assertEqual "not default" False
            =<< evaluateBool page "() => matchMedia('(prefers-reduced-motion: no-preference)').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaReducedMotion = Just ReducedMotionNoPreference}
          assertEqual "restore" False
            =<< evaluateBool page "() => matchMedia('(prefers-reduced-motion: reduce)').matches"
          assertEqual "preference" True
            =<< evaluateBool page "() => matchMedia('(prefers-reduced-motion: no-preference)').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaReducedMotion = Just ReducedMotionNoOverride}
    , testCase "should keep reduced motion and color emulation after reload" $
        withAssets $ \prefix page -> do
          assertEqual "motion off" False
            =<< evaluateBool page "() => matchMedia('(prefers-reduced-motion: reduce)').matches"
          assertEqual "forced off" False
            =<< evaluateBool page "() => matchMedia('(forced-colors: active)').matches"
          emulateMedia
            page
            defaultEmulateMedia
              { emulateMediaForcedColors = Just ForcedColorsActive
              , emulateMediaReducedMotion = Just ReducedMotionReduce
              }
          assertEqual "motion on" True
            =<< evaluateBool page "() => matchMedia('(prefers-reduced-motion: reduce)').matches"
          assertEqual "forced on" True
            =<< evaluateBool page "() => matchMedia('(forced-colors: active)').matches"
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "motion kept" True
            =<< evaluateBool page "() => matchMedia('(prefers-reduced-motion: reduce)').matches"
          assertEqual "forced kept" True
            =<< evaluateBool page "() => matchMedia('(forced-colors: active)').matches"
    , testCase "should emulate forcedColors" $
        withChromiumPage $ \page -> do
          assertEqual "none" True =<< evaluateBool page "() => matchMedia('(forced-colors: none)').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaForcedColors = Just ForcedColorsNone}
          assertEqual "none set" True =<< evaluateBool page "() => matchMedia('(forced-colors: none)').matches"
          assertEqual "not active" False =<< evaluateBool page "() => matchMedia('(forced-colors: active)').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaForcedColors = Just ForcedColorsActive}
          assertEqual "not none" False =<< evaluateBool page "() => matchMedia('(forced-colors: none)').matches"
          assertEqual "active" True =<< evaluateBool page "() => matchMedia('(forced-colors: active)').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaForcedColors = Just ForcedColorsNoOverride}
          assertEqual "restored" True =<< evaluateBool page "() => matchMedia('(forced-colors: none)').matches"
    , testCase "should emulate contrast" $
        withChromiumPage $ \page -> do
          assertEqual "default" True
            =<< evaluateBool page "() => matchMedia('(prefers-contrast: no-preference)').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaContrast = Just ContrastNoPreference}
          assertEqual "none" True
            =<< evaluateBool page "() => matchMedia('(prefers-contrast: no-preference)').matches"
          assertEqual "not more" False
            =<< evaluateBool page "() => matchMedia('(prefers-contrast: more)').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaContrast = Just ContrastMore}
          assertEqual "not none" False
            =<< evaluateBool page "() => matchMedia('(prefers-contrast: no-preference)').matches"
          assertEqual "more" True
            =<< evaluateBool page "() => matchMedia('(prefers-contrast: more)').matches"
          emulateMedia page defaultEmulateMedia {emulateMediaContrast = Just ContrastNoOverride}
          assertEqual "restored" True
            =<< evaluateBool page "() => matchMedia('(prefers-contrast: no-preference)').matches"
    , testCase "should report hover and fine pointer for desktop" $
        withChromiumPage $ \page -> do
          assertEqual "hover" True =<< evaluateBool page "() => matchMedia('(hover: hover)').matches"
          assertEqual "pointer" True =<< evaluateBool page "() => matchMedia('(pointer: fine)').matches"
    ]
