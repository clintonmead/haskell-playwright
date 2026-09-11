module Playwright.Harness
  ( assertProtocolContains
  , locate
  , withAssetPage
  , withAssets
  , withChromiumPage
  ) where

import Control.Exception
  ( try
  )
import Data.Text
  ( Text
  , isInfixOf
  , pack
  , unpack
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( Locator
  , Page
  , chromium
  , defaultGotoOptions
  , defaultLaunchOptions
  , goto
  , locator
  , selectorFromText
  , urlFromText
  , withBrowser
  , withPage
  , withPlaywright
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
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

locate :: Page -> Text -> Locator
locate page =
  locator page . selectorFromText

withChromiumPage :: (Page -> IO a) -> IO a
withChromiumPage action = do
  _ <- requireNonEmptyEnv "PLAYWRIGHT_BROWSERS_PATH"
  withPlaywright $ \pw ->
    withBrowser (chromium pw) defaultLaunchOptions $ \browser ->
      withPage browser action

withAssetPage :: FilePath -> (Page -> IO a) -> IO a
withAssetPage relativePath action =
  withAssets $ \prefix page -> do
    _ <-
      goto
        page
        (urlFromText (prefix <> "/" <> pack relativePath))
        defaultGotoOptions
    action page

withAssets :: (Text -> Page -> IO a) -> IO a
withAssets action = do
  assets <- requireAssets
  withApplication (pure (staticApp (defaultFileServerSettings assets))) $ \port ->
    withChromiumPage $ \page ->
      action ("http://127.0.0.1:" <> pack (show port)) page

requireAssets :: IO FilePath
requireAssets = do
  dir <- requireNonEmptyEnv "PLAYWRIGHT_TEST_ASSETS"
  exists <- doesDirectoryExist dir
  if exists
    then pure dir
    else assertFailure ("PLAYWRIGHT_TEST_ASSETS does not exist: " <> dir)

assertProtocolContains :: Text -> IO a -> IO ()
assertProtocolContains needle action = do
  result <- try action
  case result of
    Left (ProtocolRpc failure) ->
      if needle `isInfixOf` protocolFailureMessage failure
        then pure ()
        else
          assertFailure
            ( "expected protocol error to contain "
                <> unpack needle
                <> ", got: "
                <> unpack (protocolFailureMessage failure)
            )
    Left err ->
      assertFailure ("expected ProtocolRpc, got: " <> show err)
    Right _ ->
      assertFailure ("expected protocol error containing " <> unpack needle)

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
