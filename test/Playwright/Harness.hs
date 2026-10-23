module Playwright.Harness
  ( Recorded (..)
  , assertBool
  , assertEqual
  , assertFailure
  , assertStillPending
  , assertProtocolContains
  , assertProtocolContainsAll
  , assertWaitTimeout
  , attachFrame
  , locate
  , rafraf
  , readTestAsset
  , recordRequest
  , recordingPageApp
  , requireHandle
  , requireTimeout
  , withAssetPage
  , withAssets
  , withAssetsContextWith
  , withAssetsLaunchWith
  , withAssetsPrefix
  , withAssetsWith
  , testAssetsDir
  , withChromiumBrowser
  , withChromiumBrowserWith
  , withChromiumContext
  , withChromiumContextLaunchWith
  , withChromiumContextWith
  , withChromiumPage
  , stallingStylesheetApp
  , withLocalApp
  , withPersistentContext
  , withPersistentContextWith
  , withPersistentLaunchWith
  , withPersistentUserDataLaunchWith
  , withPersistentUserDataWith
  ) where

import Control.Monad
  ( forM_
  )
import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.CaseInsensitive qualified as CI
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
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Network.Wai qualified as Wai
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( Browser
  , evaluate
  , BrowserContext
  , ElementHandle
  , Frame
  , Locator
  , LaunchOptions
  , NewContextOptions
  , Page
  , Playwright
  , Timeout
  , asElement
  , milliseconds
  , closeContext
  , chromium
  , contextPages
  , contentFrameHandle
  , evaluateHandle
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , goto
  , launchPersistentContext
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
  ( PlaywrightError (ProtocolRpc, WaitTimeout, WaitTimeoutLog)
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
  ( IORef
  , bracket
  , modifyIORef'
  , try
  , withRunInIO
  )
import UnliftIO.Async
  ( Async
  , poll
  )
import UnliftIO.MVar
  ( MVar
  , putMVar
  , takeMVar
  )
import UnliftIO.Temporary
  ( withSystemTempDirectory
  )

locate :: Page -> Text -> Locator
locate page =
  locator page . selectorFromText

-- | Appends an iframe with the given id and URL, waits for it to load, and
-- returns its frame.
attachFrame :: Page -> Text -> Text -> Playwright Frame
attachFrame page frameId url = do
  handle <-
    evaluateHandle
      page
      ( "async () => { const frame = document.createElement('iframe'); frame.src = '"
          <> url
          <> "'; frame.id = '"
          <> frameId
          <> "'; document.body.appendChild(frame); await new Promise(x => frame.onload = x); return frame; }"
      )
  element <- requireHandle =<< asElement handle
  found <- contentFrameHandle element
  case found of
    Nothing ->
      assertFailure "expected frame"
    Just frame ->
      pure frame

-- | Converts a number of milliseconds into a timeout, failing on negative input.
requireTimeout :: (MonadIO m) => Double -> m Timeout
requireTimeout n =
  case milliseconds n of
    Nothing ->
      assertFailure ("milliseconds " <> show n)
    Just timeout ->
      pure timeout

-- | Serves the test assets, but holds one-style.css until the test releases it.
-- The app puts to arrived when the stylesheet request comes in.
stallingStylesheetApp :: FilePath -> MVar () -> MVar () -> Application
stallingStylesheetApp assets arrived release =
  let
    static = staticApp (defaultFileServerSettings assets)
  in
    \request respond ->
      case pathInfo request of
        ["one-style.css"] -> do
          putMVar arrived ()
          takeMVar release
          respond (responseLBS status200 [(hContentType, "text/css")] "")
        _ ->
          static request respond

-- | Waits for two animation frames, five times over, so in-flight work gets a
-- chance to run without a sleep.
rafraf :: Page -> Int -> Playwright ()
rafraf page count =
  forM_ [1 .. count :: Int] $ \_ -> do
    _ <-
      evaluate
        page
        "async () => { await new Promise(f => window.requestAnimationFrame(() => window.requestAnimationFrame(f))); }"
    pure ()

-- | Fails if the async action has already finished.
assertStillPending :: Async a -> Playwright ()
assertStillPending pending = do
  status <- poll pending
  case status of
    Nothing ->
      pure ()
    Just _ ->
      assertFailure "expected the operation to still be pending"

requireHandle :: Maybe ElementHandle -> Playwright ElementHandle
requireHandle handle =
  case handle of
    Nothing ->
      assertFailure "expected an element handle"
    Just value ->
      pure value

withChromiumBrowser :: (Browser -> Playwright a) -> IO a
withChromiumBrowser =
  withChromiumBrowserWith defaultLaunchOptions

withChromiumBrowserWith :: LaunchOptions -> (Browser -> Playwright a) -> IO a
withChromiumBrowserWith opts action = do
  _ <- requireNonEmptyEnv "PLAYWRIGHT_BROWSERS_PATH"
  runPlaywright $ do
    browserType <- chromium
    withBrowser browserType opts action

withChromiumPage :: (Page -> Playwright a) -> IO a
withChromiumPage action =
  withChromiumBrowser $ \browser ->
    withPage browser action

withChromiumContextLaunchWith :: LaunchOptions -> NewContextOptions -> (BrowserContext -> Page -> Playwright a) -> IO a
withChromiumContextLaunchWith launchOpts opts action =
  withChromiumBrowserWith launchOpts $ \browser ->
    bracket
      ( do
          ctx <- newContextWith browser opts
          page <- newPage ctx
          pure (ctx, page)
      )
      (\(ctx, _) -> closeContext ctx)
      (\(ctx, page) -> action ctx page)

withChromiumContextWith :: NewContextOptions -> (BrowserContext -> Page -> Playwright a) -> IO a
withChromiumContextWith =
  withChromiumContextLaunchWith defaultLaunchOptions

withChromiumContext :: (BrowserContext -> Page -> Playwright a) -> IO a
withChromiumContext =
  withChromiumContextWith defaultNewContextOptions

withPersistentContextWith :: NewContextOptions -> (BrowserContext -> Page -> Playwright a) -> IO a
withPersistentContextWith =
  withPersistentLaunchWith defaultLaunchOptions

withPersistentLaunchWith :: LaunchOptions -> NewContextOptions -> (BrowserContext -> Page -> Playwright a) -> IO a
withPersistentLaunchWith launchOpts opts action = do
  _ <- requireNonEmptyEnv "PLAYWRIGHT_BROWSERS_PATH"
  withSystemTempDirectory "pw-user-data" $ \userDataDir ->
    runPlaywright (withPersistentUserDataLaunchWith userDataDir launchOpts opts action)

withPersistentContext :: (BrowserContext -> Page -> Playwright a) -> IO a
withPersistentContext =
  withPersistentContextWith defaultNewContextOptions

withPersistentUserDataWith :: FilePath -> NewContextOptions -> (BrowserContext -> Page -> Playwright a) -> Playwright a
withPersistentUserDataWith userDataDir =
  withPersistentUserDataLaunchWith userDataDir defaultLaunchOptions

withPersistentUserDataLaunchWith :: FilePath -> LaunchOptions -> NewContextOptions -> (BrowserContext -> Page -> Playwright a) -> Playwright a
withPersistentUserDataLaunchWith userDataDir launchOpts opts action = do
  browserType <- chromium
  bracket
    (launchPersistentContext browserType userDataDir launchOpts opts)
    closeContext
    ( \ctx -> do
        page <- requirePersistentPage ctx
        action ctx page
    )

requirePersistentPage :: BrowserContext -> Playwright Page
requirePersistentPage ctx = do
  pages <- contextPages ctx
  case pages of
    page : _ ->
      pure page
    [] ->
      assertFailure "expected the persistent context default page"

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

withAssetsLaunchWith :: LaunchOptions -> NewContextOptions -> (Text -> BrowserContext -> Page -> Playwright a) -> IO a
withAssetsLaunchWith launchOpts opts action =
  withAssetsPrefix $ \prefix ->
    withChromiumContextLaunchWith launchOpts opts $ \ctx page ->
      action prefix ctx page

withAssetsContextWith :: NewContextOptions -> (Text -> BrowserContext -> Page -> Playwright a) -> IO a
withAssetsContextWith =
  withAssetsLaunchWith defaultLaunchOptions

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

assertProtocolContainsAll :: [Text] -> Playwright a -> Playwright ()
assertProtocolContainsAll needles action = do
  result <- try action
  case result of
    Left (ProtocolRpc failure) ->
      case filter (\needle -> not (needle `isInfixOf` protocolFailureMessage failure)) needles of
        [] -> pure ()
        missing ->
          assertFailure
            ( "expected protocol error to contain "
                <> show (fmap unpack missing)
                <> ", got: "
                <> unpack (protocolFailureMessage failure)
            )
    Left err ->
      assertFailure ("expected ProtocolRpc, got: " <> show err)
    Right _ ->
      assertFailure ("expected protocol error containing " <> show (fmap unpack needles))

assertWaitTimeout :: Playwright a -> Playwright ()
assertWaitTimeout action = do
  result <- try action
  case result of
    Left (WaitTimeout _) ->
      pure ()
    Left (WaitTimeoutLog _ _) ->
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

-- | One request a recording server saw: method, path, body, and headers with
-- their names folded to lower case.
data Recorded = Recorded
  { recordedMethod :: BS8.ByteString
  , recordedPath :: BS8.ByteString
  , recordedBody :: BS8.ByteString
  , recordedHeaders :: [(BS8.ByteString, BS8.ByteString)]
  }

recordRequest :: IORef [Recorded] -> Wai.Request -> IO ()
recordRequest requests request = do
  body <- Wai.strictRequestBody request
  modifyIORef' requests
    ( Recorded
        (Wai.requestMethod request)
        (Wai.rawPathInfo request)
        (LBS8.toStrict body)
        (map (\(name, value) -> (CI.foldedCase name, value)) (Wai.requestHeaders request))
        :
    )

-- | Records every request, and answers with an empty page.
recordingPageApp :: IORef [Recorded] -> Application
recordingPageApp requests request respond = do
  recordRequest requests request
  respond (responseLBS status200 [(hContentType, "text/html")] "")
