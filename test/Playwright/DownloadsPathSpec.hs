module Playwright.DownloadsPathSpec
  ( tests
  ) where

import Data.List
  ( isPrefixOf
  )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text
  ( Text
  , pack
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( Download
  , LaunchOptions (launchOptionsDownloadsPath)
  , Page
  , Playwright
  , SuggestedFilename (suggestedFilenameToText)
  , click
  , closeBrowser
  , closePage
  , defaultLaunchOptions
  , defaultNewContextOptions
  , downloadPath
  , downloadSuggestedFilename
  , downloadUrl
  , newBrowserPage
  , setContent
  , urlToText
  , waitForDownload
  )
import Playwright.Harness
  ( assertEqual
  , locate
  , withChromiumBrowserWith
  , withPersistentLaunchWith
  )
import System.Directory
  ( makeRelativeToCurrentDirectory
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.Directory
  ( doesDirectoryExist
  , doesFileExist
  )
import UnliftIO.Temporary
  ( withSystemTempDirectory
  )

tests :: TestTree
tests =
  testGroup
    "downloads-path"
    [ testCase "should keep downloadsPath folder" $
        withDownloadsDir $ \downloadsDir prefix ->
          withChromiumBrowserWith (launchDownloads downloadsDir) $ \browser -> do
            page <- newBrowserPage browser
            download <- clickDownload page prefix
            assertEqual "url" (prefix <> "/download") (urlToText (downloadUrl download))
            assertEqual "name" "file.txt" (suggestedFilenameToText (downloadSuggestedFilename download))
            _ <- downloadPath download
            closePage page
            closeBrowser browser
            exists <- doesDirectoryExist downloadsDir
            assertEqual "folder" True exists
    , testCase "should delete downloads when context closes" $
        withDownloadsDir $ \downloadsDir prefix ->
          withChromiumBrowserWith (launchDownloads downloadsDir) $ \browser -> do
            page <- newBrowserPage browser
            download <- clickDownload page prefix
            path <- downloadPath download
            existsBefore <- doesFileExist path
            assertEqual "before" True existsBefore
            closePage page
            existsAfter <- doesFileExist path
            assertEqual "after" False existsAfter
    , testCase "should report downloads in downloadsPath folder" $
        withDownloadsDir $ \downloadsDir prefix ->
          withChromiumBrowserWith (launchDownloads downloadsDir) $ \browser -> do
            page <- newBrowserPage browser
            download <- clickDownload page prefix
            path <- downloadPath download
            assertEqual "in folder" True (downloadsDir `isPrefixOf` path)
    , testCase "should report downloads in downloadsPath folder with a relative path" $
        withDownloadsDir $ \downloadsDir prefix -> do
          relative <- makeRelativeToCurrentDirectory downloadsDir
          withChromiumBrowserWith (launchDownloads relative) $ \browser -> do
            page <- newBrowserPage browser
            download <- clickDownload page prefix
            path <- downloadPath download
            assertEqual "in folder" True (downloadsDir `isPrefixOf` path)
    , testCase "should accept downloads in persistent context" $
        withDownloadsDir $ \downloadsDir prefix ->
          withPersistentLaunchWith
              (launchDownloads downloadsDir)
              defaultNewContextOptions
              $ \_ page -> do
                download <- clickDownload page prefix
                assertEqual "url" (prefix <> "/download") (urlToText (downloadUrl download))
                assertEqual "name" "file.txt" (suggestedFilenameToText (downloadSuggestedFilename download))
                path <- downloadPath download
                assertEqual "in folder" True (downloadsDir `isPrefixOf` path)
    , testCase "should delete downloads when persistent context closes" $
        withDownloadsDir $ \downloadsDir prefix -> do
          path <-
            withPersistentLaunchWith
              (launchDownloads downloadsDir)
              defaultNewContextOptions
              $ \_ctx page -> do
                download <- clickDownload page prefix
                saved <- downloadPath download
                existsBefore <- doesFileExist saved
                assertEqual "before" True existsBefore
                pure saved
          existsAfter <- doesFileExist path
          assertEqual "after" False existsAfter
    ]

launchDownloads :: FilePath -> LaunchOptions
launchDownloads downloadsDir =
  defaultLaunchOptions {launchOptionsDownloadsPath = Just downloadsDir}

withDownloadsDir :: (FilePath -> Text -> IO a) -> IO a
withDownloadsDir action =
  withSystemTempDirectory "pw-downloads" $ \downloadsDir ->
    withApplication (pure downloadApp) $ \port ->
      action downloadsDir ("http://127.0.0.1:" <> pack (show port))

clickDownload :: Page -> Text -> Playwright Download
clickDownload page prefix = do
  setContent page ("<a href=\"" <> prefix <> "/download\">download</a>")
  withAsync (waitForDownload page) $ \handle -> do
    click (locate page "a")
    wait handle

downloadApp :: Application
downloadApp _ respond =
  respond $
    responseLBS
      status200
      [ (hContentType, "application/octet-stream")
      , ("Content-Disposition", "attachment; filename=file.txt")
      ]
      (LBS8.pack "Hello world")
