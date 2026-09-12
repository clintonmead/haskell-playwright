module Playwright.Harness
  ( assertProtocolContains
  , assertWaitTimeout
  , locate
  , readTestAsset
  , requireHandle
  , withAssetPage
  , withAssets
  , withAssetsContextWith
  , withAssetsPrefix
  , withAssetsWith
  , withChromiumBrowser
  , withChromiumContext
  , withChromiumContextWith
  , withChromiumPage
  ) where

import Control.Exception
  ( bracket
  , try
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as ByteString
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
  ( Browser
  , BrowserContext
  , ElementHandle
  , Locator
  , NewContextOptions
  , Page
  , closeContext
  , chromium
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , goto
  , locator
  , newContextWith
  , newPage
  , selectorFromText
  , urlFromText
  , withBrowser
  , withPage
  , withPlaywright
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc, WaitTimeout)
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

requireHandle :: Maybe ElementHandle -> IO ElementHandle
requireHandle handle =
  case handle of
    Nothing ->
      assertFailure "expected an element handle"
    Just value ->
      pure value

withChromiumBrowser :: (Browser -> IO a) -> IO a
withChromiumBrowser action = do
  _ <- requireNonEmptyEnv "PLAYWRIGHT_BROWSERS_PATH"
  withPlaywright $ \pw ->
    withBrowser (chromium pw) defaultLaunchOptions action

withChromiumPage :: (Page -> IO a) -> IO a
withChromiumPage action =
  withChromiumBrowser $ \browser ->
    withPage browser action

withChromiumContextWith :: NewContextOptions -> (BrowserContext -> Page -> IO a) -> IO a
withChromiumContextWith opts action =
  withChromiumBrowser $ \browser ->
    bracket
      ( do
          ctx <- newContextWith browser opts
          page <- newPage ctx
          pure (ctx, page)
      )
      (\(ctx, _) -> closeContext ctx)
      (\(ctx, page) -> action ctx page)

withChromiumContext :: (BrowserContext -> Page -> IO a) -> IO a
withChromiumContext =
  withChromiumContextWith defaultNewContextOptions

withAssetPage :: FilePath -> (Page -> IO a) -> IO a
withAssetPage relativePath action =
  withAssets $ \prefix page -> do
    _ <-
      goto
        page
        (urlFromText (prefix <> "/" <> pack relativePath))
        defaultGotoOptions
    action page

withAssetsPrefix :: (Text -> IO a) -> IO a
withAssetsPrefix action = do
  assets <- requireAssets
  withApplication (pure (staticApp (defaultFileServerSettings assets))) $ \port ->
    action ("http://127.0.0.1:" <> pack (show port))

withAssetsContextWith :: NewContextOptions -> (Text -> BrowserContext -> Page -> IO a) -> IO a
withAssetsContextWith opts action =
  withAssetsPrefix $ \prefix ->
    withChromiumContextWith opts $ \ctx page ->
      action prefix ctx page

withAssetsWith :: NewContextOptions -> (Text -> Page -> IO a) -> IO a
withAssetsWith opts action =
  withAssetsContextWith opts (\prefix _ page -> action prefix page)

withAssets :: (Text -> Page -> IO a) -> IO a
withAssets =
  withAssetsWith defaultNewContextOptions

readTestAsset :: FilePath -> IO ByteString
readTestAsset relativePath = do
  dir <- requireAssets
  ByteString.readFile (dir <> "/" <> relativePath)

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

assertWaitTimeout :: IO a -> IO ()
assertWaitTimeout action = do
  result <- try action
  case result of
    Left (WaitTimeout _) ->
      pure ()
    Left err ->
      assertFailure ("expected WaitTimeout, got: " <> show err)
    Right _ ->
      assertFailure "expected WaitTimeout"

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
