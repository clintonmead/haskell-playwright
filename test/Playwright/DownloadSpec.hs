module Playwright.DownloadSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( forever
  , void
  )
import System.Timeout
  ( timeout
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.Directory
  ( doesFileExist
  , listDirectory
  )
import UnliftIO.Temporary
  ( withSystemTempDirectory
  )
import Data.ByteString.Builder
  ( byteString
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe
  ( isNothing
  )
import Data.Text
  ( Text
  , isInfixOf
  , isSuffixOf
  , pack
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , Response
  , ResponseReceived
  , pathInfo
  , responseLBS
  , responseStream
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( AcceptDownloads (AcceptDownloadsAccept, AcceptDownloadsDeny)
  , ClickOptions (clickOptionsModifiers)
  , Download
  , Exactness (Inexact)
  , Frame
  , KeyboardModifier (KeyboardModifierAlt)
  , NewContextOptions (newContextAcceptDownloads)
  , Page
  , Playwright
  , UrlMatch (UrlMatchGlob)
  , click
  , clickWith
  , closeBrowser
  , closeContext
  , continueRoute
  , currentUrl
  , defaultClickOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , downloadCancel
  , downloadDelete
  , downloadFailure
  , downloadPage
  , downloadPath
  , downloadSaveAs
  , downloadSuggestedFilename
  , downloadUrl
  , frames
  , getByText
  , goto
  , locatorOnFrame
  , newBrowserPageWith
  , pageContext
  , pagesEqual
  , route
  , selectorFromText
  , setContent
  , suggestedFilenameToText
  , url
  , urlFromText
  , urlToText
  , waitForDownload
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withAssetPage
  , withChromiumBrowser
  , withChromiumContextWith
  , assertEqual
  , assertFailure
  )
import Test.Tasty
  ( DependencyType (AllFinish)
  , TestTree
  , sequentialTestGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

tests :: TestTree
tests =
  sequentialTestGroup
    "download"
    AllFinish
    [ testCase "should report download when navigation turns into download" $
        withDownloadServer $ \prefix page -> do
          download <- gotoDownload page (prefix <> "/download")
          assertEqual "page" True (pagesEqual page (downloadPage download))
          assertEqual "url" (prefix <> "/download") (urlToText (downloadUrl download))
          assertDownloadBody download
          assertEqual "blank" "about:blank" . urlToText =<< url page
    , testCase "should work with Cross-Origin-Opener-Policy" $
        withDownloadServer $ \prefix page -> do
          download <- gotoDownload page (prefix <> "/downloadWithCOOP")
          assertEqual "page" True (pagesEqual page (downloadPage download))
          assertEqual "url" (prefix <> "/downloadWithCOOP") (urlToText (downloadUrl download))
          assertDownloadBody download
          assertEqual "blank" "about:blank" . urlToText =<< url page
    , testCase "should report downloads with acceptDownloads: false" $
        withApplication (pure downloadSuiteApp) $ \port ->
          withChromiumContextWith
            defaultNewContextOptions {newContextAcceptDownloads = Just AcceptDownloadsDeny}
            $ \_ page -> do
              let
                prefix = "http://127.0.0.1:" <> pack (show port)
              download <- clickDownload page prefix
              assertEqual "page" True (pagesEqual page (downloadPage download))
              assertEqual "url" (prefix <> "/downloadWithFilename") (urlToText (downloadUrl download))
              assertEqual "name" "file.txt" (suggestedFilenameToText (downloadSuggestedFilename download))
              assertProtocolContains "acceptDownloads" (downloadPath download)
              failure <- downloadFailure download
              case failure of
                Just message ->
                  assertEqual "failure" True ("acceptDownloads" `isInfixOf` message)
                Nothing ->
                  assertFailure "expected download failure"
    , testCase "should report downloads with acceptDownloads: true" $
        withDownloadServer $ \prefix page -> do
          download <- clickHref page (prefix <> "/download")
          assertDownloadBody download
    , testCase "should report proper download url when download is from download attribute" $
        withDownloadServer $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty")) defaultGotoOptions
          download <-
            clickHrefWith
              page
              ("<a href=\"" <> prefix <> "/download\" download=\"foo.zip\">download</a>")
          assertEqual "url" (prefix <> "/download") (urlToText (downloadUrl download))
    , testCase "should report downloads for download attribute" $
        withDownloadServer $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty")) defaultGotoOptions
          download <-
            clickHrefWith
              page
              ("<a href=\"" <> prefix <> "/download\" download=\"foo.zip\">download</a>")
          assertEqual "name" "foo.zip" (suggestedFilenameToText (downloadSuggestedFilename download))
          path <- downloadPath download
          exists <- doesFileExist path
          assertEqual "exists" True exists
    , testCase "should save to user-specified path without updating original path" $
        withDownloadServer $ \prefix page ->
          withSystemTempDirectory "pw-save-as" $ \dir -> do
            download <- clickHref page (prefix <> "/download")
            let
              userPath = dir <> "/download.txt"
            downloadSaveAs download userPath
            assertFileBody userPath
            original <- downloadPath download
            assertFileBody original
    , testCase "should save to two different paths with multiple saveAs calls" $
        withDownloadServer $ \prefix page ->
          withSystemTempDirectory "pw-save-as" $ \dir -> do
            download <- clickHref page (prefix <> "/download")
            downloadSaveAs download (dir <> "/download.txt")
            downloadSaveAs download (dir <> "/download (2).txt")
            assertFileBody (dir <> "/download.txt")
            assertFileBody (dir <> "/download (2).txt")
    , testCase "should save to overwritten filepath" $
        withDownloadServer $ \prefix page ->
          withSystemTempDirectory "pw-save-as" $ \dir -> do
            download <- clickHref page (prefix <> "/download")
            let
              nested = dir <> "/downloads"
              userPath = nested <> "/download.txt"
            downloadSaveAs download userPath
            names <- listDirectory nested
            assertEqual "once" 1 (length names)
            downloadSaveAs download userPath
            namesAgain <- listDirectory nested
            assertEqual "still one" 1 (length namesAgain)
            assertFileBody userPath
    , testCase "should create subdirectories when saving to non-existent user-specified path" $
        withDownloadServer $ \prefix page ->
          withSystemTempDirectory "pw-save-as" $ \dir -> do
            download <- clickHref page (prefix <> "/download")
            let
              nested = dir <> "/these/are/directories/download.txt"
            downloadSaveAs download nested
            assertFileBody nested
    , testCase "should error when saving with downloads disabled" $
        withApplication (pure downloadSuiteApp) $ \port ->
          withChromiumContextWith
            defaultNewContextOptions {newContextAcceptDownloads = Just AcceptDownloadsDeny}
            $ \_ page -> do
              let
                prefix = "http://127.0.0.1:" <> pack (show port)
              download <- clickHref page (prefix <> "/download")
              withSystemTempDirectory "pw-save-as" $ \dir ->
                assertProtocolContains
                  "acceptDownloads"
                  (downloadSaveAs download (dir <> "/download.txt"))
    , testCase "should error when saving after deletion" $
        withDownloadServer $ \prefix page ->
          withSystemTempDirectory "pw-save-as" $ \dir -> do
            download <- clickHref page (prefix <> "/download")
            downloadDelete download
            assertProtocolContains
              "Target page, context or browser has been closed"
              (downloadSaveAs download (dir <> "/download.txt"))
    , testCase "should report non-navigation downloads" $
        withDownloadServer $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty")) defaultGotoOptions
          download <-
            clickHrefWith
              page
              ("<a download=\"file.txt\" href=\"" <> prefix <> "/download\">download</a>")
          assertEqual "name" "file.txt" (suggestedFilenameToText (downloadSuggestedFilename download))
          assertDownloadBody download
    , testCase "should report download path within page.on('download') handler for Blobs" $
        withAssetPage "download-blob.html" $ \page -> do
          withAsync (waitForDownload page) $ \handle -> do
            click (locate page "a")
            download <- wait handle
            assertDownloadBody download
    , testCase "should report alt-click downloads" $
        withDownloadServer $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty")) defaultGotoOptions
          setContent page ("<a href=\"" <> prefix <> "/download\">download</a>")
          withAsync (waitForDownload page) $ \handle -> do
            clickWith
              (locate page "a")
              defaultClickOptions {clickOptionsModifiers = Just [KeyboardModifierAlt]}
            download <- wait handle
            path <- downloadPath download
            exists <- doesFileExist path
            assertEqual "exists" True exists
            assertFileBody path
    , testCase "should report new window downloads" $
        withDownloadServer $ \prefix page -> do
          download <-
            clickHrefWith
              page
              ("<a target=_blank href=\"" <> prefix <> "/download\">download</a>")
          path <- downloadPath download
          exists <- doesFileExist path
          assertEqual "exists" True exists
    , testCase "should delete file" $
        withDownloadServer $ \prefix page -> do
          download <- clickHref page (prefix <> "/download")
          path <- downloadPath download
          exists <- doesFileExist path
          assertEqual "exists" True exists
          downloadDelete download
          gone <- doesFileExist path
          assertEqual "deleted" False gone
    , testCase "should delete downloads on context destruction" $
        withDownloadServer $ \prefix page -> do
          first <- clickHref page (prefix <> "/download")
          second <- clickHref page (prefix <> "/download")
          path1 <- downloadPath first
          path2 <- downloadPath second
          closeContext (pageContext page)
          gone1 <- doesFileExist path1
          gone2 <- doesFileExist path2
          assertEqual "first" False gone1
          assertEqual "second" False gone2
    , testCase "should delete downloads on browser gone" $
        withApplication (pure downloadSuiteApp) $ \port ->
          withChromiumBrowser $ \browser -> do
            let
              prefix = "http://127.0.0.1:" <> pack (show port)
            page <-
              newBrowserPageWith
                browser
                defaultNewContextOptions {newContextAcceptDownloads = Just AcceptDownloadsAccept}
            first <- clickHref page (prefix <> "/download")
            second <- clickHref page (prefix <> "/download")
            path1 <- downloadPath first
            path2 <- downloadPath second
            closeBrowser browser
            gone1 <- doesFileExist path1
            gone2 <- doesFileExist path2
            assertEqual "first" False gone1
            assertEqual "second" False gone2
    , testCase "should be able to cancel pending downloads" $
        withDownloadServer $ \prefix page -> do
          download <- clickHref page (prefix <> "/downloadWithDelay")
          downloadCancel download
          assertEqual "canceled" (Just "canceled") =<< downloadFailure download
    , testCase "should not fail explicitly to cancel a download even if that is already finished" $
        withDownloadServer $ \prefix page -> do
          download <- clickHref page (prefix <> "/download")
          assertDownloadBody download
          downloadCancel download
          assertEqual "no failure" True . isNothing =<< downloadFailure download
    , testCase "should report downloads with interception" $
        withDownloadServer $ \prefix page -> do
          route page (UrlMatchGlob "**/*") continueRoute
          download <- clickHref page (prefix <> "/download")
          assertDownloadBody download
    , testCase "should emit download event from nested iframes" $
        withDownloadServer $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/1")) defaultGotoOptions
          frame <- frameEnding page "/3"
          withSystemTempDirectory "pw-save-as" $ \dir -> do
            withAsync (waitForDownload page) $ \handle -> do
              click (locatorOnFrame page frame (selectorFromText "a"))
              download <- wait handle
              downloadSaveAs download (dir <> "/download.txt")
              assertFileBody (dir <> "/download.txt")
    , testCase "should download even if there is no attachment value" $
        withDownloadServer $ \prefix page -> do
          _ <- clickHref page (prefix <> "/downloadFilenameOnly")
          pure ()
    , testCase "should convert navigation to a resource with unsupported mime type into download" $
        withDownloadServer $ \prefix page -> do
          _ <- gotoDownload page (prefix <> "/octet")
          pure ()
    , testCase "should download links with data url" $
        withAcceptedPage $ \page -> do
          setContent
            page
            "<a download=\"SomeFile.txt\" href=\"data:text/plain;charset=utf8;,hello world\">Download!</a>"
          withAsync (waitForDownload page) $ \handle -> do
            click (getByText page "Download" Inexact)
            download <- wait handle
            assertEqual "name" "SomeFile.txt" (suggestedFilenameToText (downloadSuggestedFilename download))
    ]

withAcceptedPage :: (Page -> Playwright a) -> IO a
withAcceptedPage action =
  withChromiumContextWith
    defaultNewContextOptions {newContextAcceptDownloads = Just AcceptDownloadsAccept}
    (\_ page -> action page)

withDownloadServer :: (Text -> Page -> Playwright a) -> IO a
withDownloadServer action =
  withApplication (pure downloadSuiteApp) $ \port ->
    withAcceptedPage $ \page ->
      action ("http://127.0.0.1:" <> pack (show port)) page

gotoDownload :: Page -> Text -> Playwright Download
gotoDownload page path = do
  withAsync (waitForDownload page) $ \handle -> do
    assertProtocolContains
      "Download is starting"
      (goto page (urlFromText path) defaultGotoOptions)
    wait handle

clickDownload :: Page -> Text -> Playwright Download
clickDownload page prefix =
  clickHrefWith page ("<a href=\"" <> prefix <> "/downloadWithFilename\">download</a>")

clickHref :: Page -> Text -> Playwright Download
clickHref page href =
  clickHrefWith page ("<a href=\"" <> href <> "\">download</a>")

clickHrefWith :: Page -> Text -> Playwright Download
clickHrefWith page markup = do
  setContent page markup
  withAsync (waitForDownload page) $ \handle -> do
    click (locate page "a")
    wait handle

assertDownloadBody :: Download -> Playwright ()
assertDownloadBody download = do
  path <- downloadPath download
  assertFileBody path

assertFileBody :: FilePath -> Playwright ()
assertFileBody path = do
  exists <- doesFileExist path
  assertEqual "exists" True exists
  contents <- liftIO (BS8.readFile path)
  assertEqual "body" (BS8.pack "Hello world") contents

frameEnding :: Page -> Text -> Playwright Frame
frameEnding page suffix = do
  allFrames <- frames page
  matched <- traverse (matchesSuffix suffix) allFrames
  case [frame | (frame, True) <- matched] of
    frame : _ ->
      pure frame
    [] ->
      assertFailure "expected nested iframe"

matchesSuffix :: Text -> Frame -> Playwright (Frame, Bool)
matchesSuffix suffix frame = do
  frameUrl <- currentUrl frame
  pure (frame, suffix `isSuffixOf` urlToText frameUrl)

downloadSuiteApp :: Application
downloadSuiteApp request respond =
  case pathInfo request of
    ["download"] ->
      attachment respond Nothing
    ["downloadWithFilename"] ->
      attachment respond (Just "attachment; filename=file.txt")
    ["downloadWithCOOP"] ->
      respond $
        responseLBS
          status200
          [ (hContentType, "application/octet-stream")
          , ("Content-Disposition", "attachment")
          , ("Cross-Origin-Opener-Policy", "same-origin")
          ]
          (LBS8.pack "Hello world")
    ["downloadFilenameOnly"] ->
      attachment respond (Just "filename=foo.txt")
    ["octet"] ->
      respond $
        responseLBS
          status200
          [(hContentType, "application/octet-stream")]
          (LBS8.pack "Hello world")
    ["downloadWithDelay"] ->
      respond $
        responseStream
          status200
          [ (hContentType, "application/octet-stream")
          , ("Content-Disposition", "attachment; filename=file.txt")
          ]
          $ \write flush -> do
            write (byteString (BS8.replicate 4096 'a' <> "foo"))
            flush
            void (timeout 5000000 (forever (threadDelay 100000)))
    ["empty"] ->
      respond $
        responseLBS status200 [(hContentType, "text/html")] (LBS8.pack "")
    ["1"] ->
      html respond "<iframe src=\"/2\"></iframe>"
    ["2"] ->
      html respond "<iframe src=\"/3\"></iframe>"
    ["3"] ->
      html respond "<a href=\"/download\">download</a>"
    _ ->
      respond (responseLBS status200 [(hContentType, "text/plain")] (LBS8.pack "missing"))

attachment :: (Response -> IO ResponseReceived) -> Maybe BS8.ByteString -> IO ResponseReceived
attachment respond disposition =
  respond $
    responseLBS
      status200
      ( (hContentType, "application/octet-stream")
          : case disposition of
            Just value ->
              [("Content-Disposition", value)]
            Nothing ->
              [("Content-Disposition", "attachment")]
      )
      (LBS8.pack "Hello world")

html :: (Response -> IO ResponseReceived) -> LBS8.ByteString -> IO ResponseReceived
html respond body =
  respond (responseLBS status200 [(hContentType, "text/html")] body)
