module Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , assertProtocolContains
  , assertWaitTimeout
  , locate
  , readTestAsset
  , requireHandle
  , withAssetPage
  , withAssets
  , withAssetsContextWith
  , withAssetsPrefix
  , withAssetsWith
  , testAssetsDir
  , withChromiumBrowser
  , withChromiumContext
  , withChromiumContextWith
  , withChromiumPage
  , withLocalApp
  ) where

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
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
import Network.Wai
  ( Application
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
  , Playwright
  , closeContext
  , chromium
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , goto
  , locator
  , newContextWith
  , newPage
  , runPlaywright
  , selectorFromText
  , urlFromText
  , withBrowser
  , withPage
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
import Test.Tasty.HUnit qualified as HUnit
import UnliftIO
  ( bracket
  , try
  , withRunInIO
  )

locate :: Page -> Text -> Locator
locate page =
  locator page . selectorFromText

requireHandle :: Maybe ElementHandle -> Playwright ElementHandle
requireHandle handle =
  case handle of
    Nothing ->
      assertFailure "expected an element handle"
    Just value ->
      pure value

withChromiumBrowser :: (Browser -> Playwright a) -> IO a
withChromiumBrowser action = do
  _ <- requireNonEmptyEnv "PLAYWRIGHT_BROWSERS_PATH"
  runPlaywright $ do
    browserType <- chromium
    withBrowser browserType defaultLaunchOptions action

withChromiumPage :: (Page -> Playwright a) -> IO a
withChromiumPage action =
  withChromiumBrowser $ \browser ->
    withPage browser action

withChromiumContextWith :: NewContextOptions -> (BrowserContext -> Page -> Playwright a) -> IO a
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

withChromiumContext :: (BrowserContext -> Page -> Playwright a) -> IO a
withChromiumContext =
  withChromiumContextWith defaultNewContextOptions

withAssetPage :: FilePath -> (Page -> Playwright a) -> IO a
withAssetPage relativePath action =
  withAssets $ \prefix page -> do
    _ <-
      goto
        page
        (urlFromText (prefix <> "/" <> pack relativePath))
        defaultGotoOptions
    action page

withLocalApp :: Application -> (Text -> Playwright a) -> Playwright a
withLocalApp app action =
  withRunInIO $ \run ->
    withApplication (pure app) $ \port ->
      run (action ("http://127.0.0.1:" <> pack (show port)))

withAssetsPrefix :: (Text -> IO a) -> IO a
withAssetsPrefix action = do
  assets <- requireAssets
  withApplication (pure (staticApp (defaultFileServerSettings assets))) $ \port ->
    action ("http://127.0.0.1:" <> pack (show port))

withAssetsContextWith :: NewContextOptions -> (Text -> BrowserContext -> Page -> Playwright a) -> IO a
withAssetsContextWith opts action =
  withAssetsPrefix $ \prefix ->
    withChromiumContextWith opts $ \ctx page ->
      action prefix ctx page

withAssetsWith :: NewContextOptions -> (Text -> Page -> Playwright a) -> IO a
withAssetsWith opts action =
  withAssetsContextWith opts (\prefix _ page -> action prefix page)

withAssets :: (Text -> Page -> Playwright a) -> IO a
withAssets =
  withAssetsWith defaultNewContextOptions

readTestAsset :: (MonadIO m) => FilePath -> m ByteString
readTestAsset relativePath =
  liftIO $ do
    dir <- requireAssets
    ByteString.readFile (dir <> "/" <> relativePath)

testAssetsDir :: (MonadIO m) => m FilePath
testAssetsDir =
  liftIO requireAssets

requireAssets :: IO FilePath
requireAssets = do
  dir <- requireNonEmptyEnv "PLAYWRIGHT_TEST_ASSETS"
  exists <- doesDirectoryExist dir
  if exists
    then pure dir
    else HUnit.assertFailure ("PLAYWRIGHT_TEST_ASSETS does not exist: " <> dir)

assertProtocolContains :: Text -> Playwright a -> Playwright ()
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

assertWaitTimeout :: Playwright a -> Playwright ()
assertWaitTimeout action = do
  result <- try action
  case result of
    Left (WaitTimeout _) ->
      pure ()
    Left err ->
      assertFailure ("expected WaitTimeout, got: " <> show err)
    Right _ ->
      assertFailure "expected WaitTimeout"

assertEqual :: (MonadIO m, Eq a, Show a) => String -> a -> a -> m ()
assertEqual label expected actual =
  liftIO (HUnit.assertEqual label expected actual)

assertBool :: (MonadIO m) => String -> Bool -> m ()
assertBool label value =
  liftIO (HUnit.assertBool label value)

assertFailure :: (MonadIO m) => String -> m a
assertFailure message =
  liftIO (HUnit.assertFailure message)

requireNonEmptyEnv :: String -> IO String
requireNonEmptyEnv name = do
  value <- lookupEnv name
  case value of
    Nothing ->
      HUnit.assertFailure (name <> " is not set")
    Just "" ->
      HUnit.assertFailure (name <> " is empty")
    Just path ->
      pure path
