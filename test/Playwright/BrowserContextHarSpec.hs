module Playwright.BrowserContextHarSpec
  ( tests
  ) where

import Control.Monad
  ( void
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Playwright
  ( Browser
  , RecordHar (RecordHar)
  , RecordHarContent (RecordHarContentAttach)
  , RecordHarMode (RecordHarModeMinimal)
  , Page
  , Playwright
  , closeContext
  , content
  , contextRouteFromHar
  , defaultGotoOptions
  , NewContextOptions (newContextRecordHar)
  , defaultNewContextOptions
  , evaluateText
  , goto
  , newContextWith
  , newPage
  , urlFromText
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , testAssetsDir
  , withAssetsPrefix
  , withChromiumBrowser
  )
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removePathForcibly
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( bracket
  )

tests :: TestTree
tests =
  testGroup
    "browsercontext-har"
    [ testCase "should round-trip har.zip" $
        withTempDir "round-trip-zip" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              let harPath = dir <> "/har.zip"
              recordOnClose browser (RecordHar harPath Nothing (Just RecordHarModeMinimal) Nothing) (prefix <> "/one-style.html")
              written <- liftIO (doesFileExist harPath)
              assertBool "har.zip should be written on context close" written
              replayFrom browser harPath (prefix <> "/one-style.html")
    , testCase "should produce extracted zip" $
        withTempDir "extracted-zip" $ \dir ->
          withAssetsPrefix $ \prefix ->
            withChromiumBrowser $ \browser -> do
              let harPath = dir <> "/har.har"
                  opts = RecordHar harPath (Just RecordHarContentAttach) (Just RecordHarModeMinimal) Nothing
              recordOnClose browser opts (prefix <> "/one-style.html")
              written <- liftIO (doesFileExist harPath)
              assertBool "har should be written on context close" written
              har <- liftIO (TextIO.readFile harPath)
              assertBool "har should not embed the stylesheet body" (not (Text.isInfixOf "background-color" har))
              replayFrom browser harPath (prefix <> "/one-style.html")
    , testCase "should fulfill from har with content in a file" $
        withChromiumBrowser $ \browser -> do
          dir <- testAssetsDir
          ctx <- newContextWith browser defaultNewContextOptions
          contextRouteFromHar ctx (dir <> "/har-sha1.har")
          page <- newPage ctx
          gotoUrl page "http://no.playwright/"
          assertEqual "page content" "<html><head></head><body>Hello, world</body></html>" =<< content page
          closeContext ctx
    ]

-- | Records one navigation with the given HAR options, closing the context so the HAR is written.
recordOnClose :: Browser -> RecordHar -> Text -> Playwright ()
recordOnClose browser harOpts url = do
  ctx <- newContextWith browser defaultNewContextOptions {newContextRecordHar = Just harOpts}
  page <- newPage ctx
  gotoUrl page url
  closeContext ctx

-- | Replays a recorded HAR in a fresh context and checks the page rendered from it.
replayFrom :: Browser -> FilePath -> Text -> Playwright ()
replayFrom browser harPath url = do
  ctx <- newContextWith browser defaultNewContextOptions
  contextRouteFromHar ctx harPath
  page <- newPage ctx
  gotoUrl page url
  body <- content page
  assertBool "replayed body" (Text.isInfixOf "hello, world!" body)
  assertEqual "background" "rgb(255, 192, 203)" =<< pageBackground page
  closeContext ctx

gotoUrl :: Page -> Text -> Playwright ()
gotoUrl page url =
  void (goto page (urlFromText url) defaultGotoOptions)

pageBackground :: Page -> Playwright Text
pageBackground page =
  evaluateText page "() => getComputedStyle(document.body).backgroundColor"

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir name action = do
  root <- getTemporaryDirectory
  let dir = root <> "/pw-har-" <> name
  bracket
    (removePathForcibly dir >> createDirectoryIfMissing True dir >> pure dir)
    removeDirectoryRecursive
    action
