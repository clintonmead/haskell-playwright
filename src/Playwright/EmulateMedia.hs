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
  ( PageEmulateMediaParams (pageEmulateMediaParamsColorScheme, pageEmulateMediaParamsMedia)
  , PageEmulateMediaParamsColorScheme (PageEmulateMediaParamsColorSchemeDark, PageEmulateMediaParamsColorSchemeLight, PageEmulateMediaParamsColorSchemeNoOverride, PageEmulateMediaParamsColorSchemeNoPreference)
  , PageEmulateMediaParamsMedia (PageEmulateMediaParamsMediaNoOverride, PageEmulateMediaParamsMediaPrint, PageEmulateMediaParamsMediaScreen)
  , PageEmulateMediaResult
  , mkPageEmulateMediaParams
  )

data EmulateMedia = EmulateMedia
  { emulateMediaType :: Maybe MediaType
  , emulateMediaColorScheme :: Maybe ColorScheme
  }
  deriving stock (Eq, Show)

defaultEmulateMedia :: EmulateMedia
defaultEmulateMedia =
  EmulateMedia
    { emulateMediaType = Nothing
    , emulateMediaColorScheme = Nothing
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
