module Playwright.Harness
  ( withChromiumPage
  , withAssetPage
  ) where

import Data.Text
  ( pack
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( Page
  , chromium
  , defaultGotoOptions
  , defaultLaunchOptions
  , goto
  , urlFromText
  , withBrowser
  , withPage
  , withPlaywright
  )
import System.Directory
  ( doesDirectoryExist
  )
import System.Environment
  ( lookupEnv
  )
import Test.Tasty.HUnit
  ( assertFailure
  )

withChromiumPage :: (Page -> IO a) -> IO a
withChromiumPage action = do
  _ <- requireNonEmptyEnv "PLAYWRIGHT_BROWSERS_PATH"
  withPlaywright $ \pw ->
    withBrowser (chromium pw) defaultLaunchOptions $ \browser ->
      withPage browser action

withAssetPage :: FilePath -> (Page -> IO a) -> IO a
withAssetPage relativePath action = do
  assets <- requireAssets
  withApplication (pure (staticApp (defaultFileServerSettings assets))) $ \port ->
    withChromiumPage $ \page -> do
      let
        url =
          urlFromText
            ("http://127.0.0.1:" <> pack (show port) <> "/" <> pack relativePath)
      _ <- goto page url defaultGotoOptions
      action page

requireAssets :: IO FilePath
requireAssets = do
  dir <- requireNonEmptyEnv "PLAYWRIGHT_TEST_ASSETS"
  exists <- doesDirectoryExist dir
  if exists
    then pure dir
    else assertFailure ("PLAYWRIGHT_TEST_ASSETS does not exist: " <> dir)

requireNonEmptyEnv :: String -> IO String
requireNonEmptyEnv name = do
  value <- lookupEnv name
  case value of
    Nothing ->
      assertFailure (name <> " is not set")
    Just "" ->
      assertFailure (name <> " is empty")
    Just path ->
      pure path
