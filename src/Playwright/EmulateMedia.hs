module Playwright.EmulateMedia
  ( EmulateMedia (..)
  , defaultEmulateMedia
  , emulateMedia
  ) where

import Playwright.Monad
  ( Playwright
  )
import Playwright.Channel
  ( send
  )
import Playwright.ColorScheme
  ( ColorScheme (ColorSchemeDark, ColorSchemeLight, ColorSchemeNoOverride, ColorSchemeNoPreference)
  )
import Playwright.Contrast
  ( Contrast (ContrastMore, ContrastNoOverride, ContrastNoPreference)
  )
import Playwright.ForcedColors
  ( ForcedColors (ForcedColorsActive, ForcedColorsNoOverride, ForcedColorsNone)
  )
import Playwright.MediaType
  ( MediaType (MediaNoOverride, MediaPrint, MediaScreen)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Protocol
  ( PageEmulateMediaParams (pageEmulateMediaParamsColorScheme, pageEmulateMediaParamsContrast, pageEmulateMediaParamsForcedColors, pageEmulateMediaParamsMedia, pageEmulateMediaParamsReducedMotion)
  , PageEmulateMediaParamsColorScheme (PageEmulateMediaParamsColorSchemeDark, PageEmulateMediaParamsColorSchemeLight, PageEmulateMediaParamsColorSchemeNoOverride, PageEmulateMediaParamsColorSchemeNoPreference)
  , PageEmulateMediaParamsContrast (PageEmulateMediaParamsContrastMore, PageEmulateMediaParamsContrastNoOverride, PageEmulateMediaParamsContrastNoPreference)
  , PageEmulateMediaParamsForcedColors (PageEmulateMediaParamsForcedColorsActive, PageEmulateMediaParamsForcedColorsNoOverride, PageEmulateMediaParamsForcedColorsNone)
  , PageEmulateMediaParamsMedia (PageEmulateMediaParamsMediaNoOverride, PageEmulateMediaParamsMediaPrint, PageEmulateMediaParamsMediaScreen)
  , PageEmulateMediaParamsReducedMotion (PageEmulateMediaParamsReducedMotionNoOverride, PageEmulateMediaParamsReducedMotionNoPreference, PageEmulateMediaParamsReducedMotionReduce)
  , PageEmulateMediaResult
  , mkPageEmulateMediaParams
  )
import Playwright.ReducedMotion
  ( ReducedMotion (ReducedMotionNoOverride, ReducedMotionNoPreference, ReducedMotionReduce)
  )

data EmulateMedia = EmulateMedia
  { emulateMediaType :: Maybe MediaType
  , emulateMediaColorScheme :: Maybe ColorScheme
  , emulateMediaContrast :: Maybe Contrast
  , emulateMediaForcedColors :: Maybe ForcedColors
  , emulateMediaReducedMotion :: Maybe ReducedMotion
  }
  deriving stock (Eq, Show)

defaultEmulateMedia :: EmulateMedia
defaultEmulateMedia =
  EmulateMedia
    { emulateMediaType = Nothing
    , emulateMediaColorScheme = Nothing
    , emulateMediaContrast = Nothing
    , emulateMediaForcedColors = Nothing
    , emulateMediaReducedMotion = Nothing
    }

emulateMedia :: Page -> EmulateMedia -> Playwright ()
emulateMedia page opts = do
  _ :: PageEmulateMediaResult <-
    send
      (pageChannel page)
      (MethodName "emulateMedia")
      mkPageEmulateMediaParams
        { pageEmulateMediaParamsMedia = fmap toProtocolMedia (emulateMediaType opts)
        , pageEmulateMediaParamsColorScheme = fmap toProtocolColor (emulateMediaColorScheme opts)
        , pageEmulateMediaParamsContrast = fmap toProtocolContrast (emulateMediaContrast opts)
        , pageEmulateMediaParamsForcedColors = fmap toProtocolForced (emulateMediaForcedColors opts)
        , pageEmulateMediaParamsReducedMotion = fmap toProtocolMotion (emulateMediaReducedMotion opts)
        }
  pure ()

toProtocolMedia :: MediaType -> PageEmulateMediaParamsMedia
toProtocolMedia media =
  case media of
    MediaScreen ->
      PageEmulateMediaParamsMediaScreen
    MediaPrint ->
      PageEmulateMediaParamsMediaPrint
    MediaNoOverride ->
      PageEmulateMediaParamsMediaNoOverride

toProtocolColor :: ColorScheme -> PageEmulateMediaParamsColorScheme
toProtocolColor scheme =
  case scheme of
    ColorSchemeDark ->
      PageEmulateMediaParamsColorSchemeDark
    ColorSchemeLight ->
      PageEmulateMediaParamsColorSchemeLight
    ColorSchemeNoPreference ->
      PageEmulateMediaParamsColorSchemeNoPreference
    ColorSchemeNoOverride ->
      PageEmulateMediaParamsColorSchemeNoOverride

toProtocolMotion :: ReducedMotion -> PageEmulateMediaParamsReducedMotion
toProtocolMotion motion =
  case motion of
    ReducedMotionReduce ->
      PageEmulateMediaParamsReducedMotionReduce
    ReducedMotionNoPreference ->
      PageEmulateMediaParamsReducedMotionNoPreference
    ReducedMotionNoOverride ->
      PageEmulateMediaParamsReducedMotionNoOverride

toProtocolForced :: ForcedColors -> PageEmulateMediaParamsForcedColors
toProtocolForced colors =
  case colors of
    ForcedColorsActive ->
      PageEmulateMediaParamsForcedColorsActive
    ForcedColorsNone ->
      PageEmulateMediaParamsForcedColorsNone
    ForcedColorsNoOverride ->
      PageEmulateMediaParamsForcedColorsNoOverride

toProtocolContrast :: Contrast -> PageEmulateMediaParamsContrast
toProtocolContrast contrast =
  case contrast of
    ContrastNoPreference ->
      PageEmulateMediaParamsContrastNoPreference
    ContrastMore ->
      PageEmulateMediaParamsContrastMore
    ContrastNoOverride ->
      PageEmulateMediaParamsContrastNoOverride
