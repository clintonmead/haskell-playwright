module Playwright.PageSetInputFilesSpec
  ( tests
  ) where

import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text
  ( Text
  , isInfixOf
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , consumeRequestBodyStrict
  , requestMethod
  , responseLBS
  )
import Playwright
  ( InputFile (InputFile)
  , InputFileName (InputFileName)
  , Playwright
  , click
  , evaluate
  , evaluateOnLocatorNumber
  , evaluateOnLocatorText
  , evaluateOnElementText
  , evaluateTexts
  , defaultGotoOptions
  , goto
  , setContent
  , setInputFiles
  , setInputFilesPaths
  , querySelector
  , selectorFromText
  , setInputFilesHandle
  , urlFromText
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( locate
  , readTestAsset
  , requireHandle
  , testAssetsDir
  , withAssetPage
  , withChromiumPage
  , withLocalApp
  , assertBool
  , assertEqual
  , assertFailure
  )
import System.Directory
  ( createDirectoryIfMissing
  , makeRelativeToCurrentDirectory
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( try
  )
import UnliftIO.Temporary
  ( withSystemTempDirectory
  )

tests :: TestTree
tests =
  testGroup
    "page-set-input-files"
    [ testCase "should upload the file" $
        withAssetPage "input/fileupload.html" $ \page -> do
          bytes <- readTestAsset "file-to-upload.txt"
          handle <- requireHandle =<< querySelector page (selectorFromText "input")
          setInputFilesHandle
            handle
            [InputFile (InputFileName "file-to-upload.txt") bytes]
          assertEqual "name" "file-to-upload.txt"
            =<< evaluateOnElementText handle "e => e.files[0].name"
          assertEqual "contents" "contents of the file"
            =<< evaluateOnElementText
              handle
              "e => { const reader = new FileReader(); const promise = new Promise(fulfill => reader.onload = fulfill); reader.readAsText(e.files[0]); return promise.then(() => reader.result); }"
    , testCase "should emit input and change events" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<input id=input type=file></input><script>window.events = []; const input = document.getElementById('input'); input.addEventListener('input', e => window.events.push(e.type)); input.addEventListener('change', e => window.events.push(e.type));</script>"
          bytes <- readTestAsset "file-to-upload.txt"
          setInputFiles (locate page "input") [InputFile (InputFileName "file-to-upload.txt") bytes]
          assertEqual "events" ["input", "change"] =<< evaluateTexts page "() => window.events"
    , testCase "input should trigger events when files changed second time" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<input type=file multiple=true/><script>window.events = []; const input = document.querySelector('input'); input.addEventListener('input', () => window.events.push('input')); input.addEventListener('change', () => window.events.push('change'));</script>"
          let input = locate page "input"
          textBytes <- readTestAsset "file-to-upload.txt"
          setInputFiles input [InputFile (InputFileName "file-to-upload.txt") textBytes]
          assertEqual "first name" "file-to-upload.txt"
            =<< evaluateOnLocatorText input "e => e.files[0].name"
          assertEqual "first events" ["input", "change"] =<< evaluateTexts page "() => window.events"
          _ <- evaluate page "() => { window.events.length = 0; }"
          imageBytes <- readTestAsset "pptr.png"
          setInputFiles input [InputFile (InputFileName "pptr.png") imageBytes]
          assertEqual "second name" "pptr.png"
            =<< evaluateOnLocatorText input "e => e.files[0].name"
          assertEqual "second events" ["input", "change"] =<< evaluateTexts page "() => window.events"
    , testCase "should set from memory" $
        withChromiumPage $ \page -> do
          setContent page "<input type=file>"
          setInputFiles
            (locate page "input")
            [InputFile (InputFileName "test.txt") "this is a test"]
          assertEqual "count" (1 :: Double) =<< evaluateOnLocatorNumber (locate page "input") "input => input.files.length"
          assertEqual "name" "test.txt" =<< evaluateOnLocatorText (locate page "input") "input => input.files[0].name"
    , testCase "should upload the file with spaces in name" $
        withAssetPage "input/fileupload.html" $ \page -> do
          bytes <- readTestAsset "file to upload.txt"
          handle <- requireHandle =<< querySelector page (selectorFromText "input")
          setInputFilesHandle
            handle
            [InputFile (InputFileName "file to upload.txt") bytes]
          assertEqual "name" "file to upload.txt"
            =<< evaluateOnElementText handle "e => e.files[0].name"
          assertEqual "contents" "contents of the file"
            =<< evaluateOnElementText
              handle
              "e => { const reader = new FileReader(); const promise = new Promise(fulfill => reader.onload = fulfill); reader.readAsText(e.files[0]); return promise.then(() => reader.result); }"
    , testCase "input event.composed should be true and cross shadow dom boundary" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<body><script>const div = document.createElement('div'); const shadowRoot = div.attachShadow({mode: 'open'}); shadowRoot.innerHTML = '<input type=file></input>'; document.body.appendChild(div); window.firedBodyEvents = []; window.firedEvents = []; for (const event of ['input', 'change']) { document.body.addEventListener(event, e => window.firedBodyEvents.push(e.type + ':' + e.composed), false); shadowRoot.querySelector('input').addEventListener(event, e => window.firedEvents.push(e.type + ':' + e.composed), false); }</script></body>"
          setInputFiles (locate page "input") [InputFile (InputFileName "test.txt") "this is a test"]
          assertEqual "input events" ["input:true", "change:false"] =<< evaluateTexts page "() => window.firedEvents"
          assertEqual "body events" ["input:true"] =<< evaluateTexts page "() => window.firedBodyEvents"
    , testCase "should work with CSP" $
        withChromiumPage $ \page ->
          withLocalApp cspEmptyApp $ \base -> do
            _ <- goto page (urlFromText (base <> "/empty.html")) defaultGotoOptions
            setContent page "<input type=file>"
            bytes <- readTestAsset "file-to-upload.txt"
            setInputFiles (locate page "input") [InputFile (InputFileName "file-to-upload.txt") bytes]
            assertEqual "count" (1 :: Double) =<< evaluateOnLocatorNumber (locate page "input") "input => input.files.length"
            assertEqual "name" "file-to-upload.txt" =<< evaluateOnLocatorText (locate page "input") "input => input.files[0].name"
    , testCase "should upload a folder" $
        withAssetPage "input/folderupload.html" $ \page ->
          withSystemTempDirectory "pw-folder-upload" $ \tmp -> do
            let dir = tmp <> "/file-upload-test"
            liftIO $ do
              createDirectoryIfMissing True (dir <> "/sub-dir")
              ByteString.writeFile (dir <> "/file1.txt") "file1 content"
              ByteString.writeFile (dir <> "/file2") "file2 content"
              ByteString.writeFile (dir <> "/sub-dir/really.txt") "sub-dir file content"
            setInputFilesPaths (locate page "input[type=file]") [dir]
            assertEqual "relative paths"
              [ "file-upload-test/file1.txt"
              , "file-upload-test/file2"
              , "file-upload-test/sub-dir/really.txt"
              ]
              =<< evaluateTexts page "() => [...document.querySelector('input[type=file]').files].map(f => f.webkitRelativePath).sort()"
            assertEqual "content" "file1 content"
              =<< evaluateOnLocatorText
                (locate page "input[type=file]")
                "input => { const f = [...input.files].find(f => f.webkitRelativePath === 'file-upload-test/file1.txt'); const reader = new FileReader(); const promise = new Promise(fulfill => reader.onload = fulfill); reader.readAsText(f); return promise.then(() => reader.result); }"
    , testCase "should upload a folder and throw for multiple directories" $
        withAssetPage "input/folderupload.html" $ \page ->
          withSystemTempDirectory "pw-folder-upload" $ \tmp -> do
            liftIO $ do
              createDirectoryIfMissing True (tmp <> "/folder1")
              ByteString.writeFile (tmp <> "/folder1/file1.txt") "file1 content"
              createDirectoryIfMissing True (tmp <> "/folder2")
              ByteString.writeFile (tmp <> "/folder2/file2.txt") "file2 content"
            expectProtocolError
              "Multiple directories are not supported"
              (setInputFilesPaths (locate page "input[type=file]") [tmp <> "/folder1", tmp <> "/folder2"])
    , testCase "should throw if a directory and files are passed" $
        withAssetPage "input/folderupload.html" $ \page ->
          withSystemTempDirectory "pw-folder-upload" $ \tmp -> do
            liftIO $ do
              createDirectoryIfMissing True (tmp <> "/folder1")
              ByteString.writeFile (tmp <> "/folder1/file1.txt") "file1 content"
            expectProtocolError
              "File paths must be all files or a single directory"
              (setInputFilesPaths (locate page "input[type=file]") [tmp <> "/folder1", tmp <> "/folder1/file1.txt"])
    , testCase "should throw when uploading a folder in a normal file upload input" $
        withAssetPage "input/fileupload.html" $ \page ->
          withSystemTempDirectory "pw-folder-upload" $ \tmp -> do
            let dir = tmp <> "/file-upload-test"
            liftIO $ do
              createDirectoryIfMissing True dir
              ByteString.writeFile (dir <> "/file1.txt") "file1 content"
            expectProtocolError
              "File input does not support directories, pass individual files instead"
              (setInputFilesPaths (locate page "input[type=file]") [dir])
    , testCase "should throw when uploading a file in a directory upload input" $
        withAssetPage "input/folderupload.html" $ \page -> do
          assetDir <- testAssetsDir
          expectProtocolError
            "[webkitdirectory] input requires passing a path to a directory"
            (setInputFilesPaths (locate page "input[type=file]") [assetDir <> "/file to upload.txt"])
    , testCase "should upload a file after popup" $
        withAssetPage "input/fileupload.html" $ \page -> do
          _ <- evaluate page "() => { window.__popup = window.open('about:blank'); }"
          _ <- evaluate page "() => window.__popup.close()"
          assetDir <- testAssetsDir
          filePath <- liftIO (makeRelativeToCurrentDirectory (assetDir <> "/file-to-upload.txt"))
          setInputFilesPaths (locate page "input[type=file]") [filePath]
          assertEqual "name" "file-to-upload.txt"
            =<< evaluateOnLocatorText (locate page "input[type=file]") "input => input.files[0].name"
    , testCase "should throw an error if the file does not exist" $
        withAssetPage "input/fileupload.html" $ \page ->
          expectProtocolError
            "ENOENT: no such file or directory, stat 'i actually do not exist.txt'"
            (setInputFilesPaths (locate page "input[type=file]") ["i actually do not exist.txt"])
    , testCase "should not trim big uploaded files" $
        withChromiumPage $ \page -> do
          uploads <- liftIO newEmptyMVar
          withLocalApp (uploadBodyApp uploads) $ \base -> do
            _ <- goto page (urlFromText (base <> "/empty.html")) defaultGotoOptions
            _ <- evaluate page "async () => { const body = new FormData(); body.set('file', new Blob([new Uint8Array(1048576)])); await fetch('/upload', { method: 'POST', body }); }"
            body <- liftIO (takeMVar uploads)
            assertEqual "file size" (1048576 :: Int) (multipartFileSize body)
    , testCase "should detect mime type" $
        withChromiumPage $ \page -> do
          uploads <- liftIO newEmptyMVar
          withLocalApp (uploadBodyApp uploads) $ \base -> do
            _ <- goto page (urlFromText (base <> "/empty.html")) defaultGotoOptions
            setContent
              page
              "<form action=\"/upload\" method=\"post\" enctype=\"multipart/form-data\"><input type=\"file\" name=\"file1\"><input type=\"file\" name=\"file2\"><input type=\"submit\" value=\"Submit\"></form>"
            assetDir <- testAssetsDir
            setInputFilesPaths (locate page "input[name=file1]") [assetDir <> "/file-to-upload.txt"]
            setInputFilesPaths (locate page "input[name=file2]") [assetDir <> "/pptr.png"]
            click (locate page "input[type=submit]")
            body <- liftIO (takeMVar uploads)
            assertBool
              "text part has name, filename and mime type"
              ("name=\"file1\"; filename=\"file-to-upload.txt\"\r\nContent-Type: text/plain\r\n" `ByteString.isInfixOf` body)
            assertBool
              "png part has name, filename and mime type"
              ("name=\"file2\"; filename=\"pptr.png\"\r\nContent-Type: image/png\r\n" `ByteString.isInfixOf` body)
    ]

-- | Serves an empty page whose Content-Security-Policy forbids every resource
-- type, the same header the upstream "should work with CSP" case sets.
cspEmptyApp :: Application
cspEmptyApp _ respond =
  respond
    ( responseLBS
        status200
        [ ("Content-Security-Policy", "default-src 'none'")
        , (hContentType, "text/html")
        ]
        ""
    )

-- | Hands the raw body of every POST to the test, so the test can inspect the
-- multipart encoding the browser produced.
uploadBodyApp :: MVar ByteString -> Application
uploadBodyApp uploads request respond
  | requestMethod request == "POST" = do
      body <- consumeRequestBodyStrict request
      putMVar uploads (LazyByteString.toStrict body)
      respond (responseLBS status200 [] "")
  | otherwise = respond (responseLBS status200 [] "")

-- | Returns the byte length of the first part's content in a multipart body
-- with a single part, or -1 if the body does not end with the closing
-- boundary.
multipartFileSize :: ByteString -> Int
multipartFileSize body =
  let (_, rest) = ByteString.breakSubstring "\r\n\r\n" body
      content = ByteString.drop 4 rest
      boundary = ByteString.takeWhile (/= 13) body
      closing = "\r\n" <> boundary <> "--\r\n"
   in if closing `ByteString.isSuffixOf` content
        then ByteString.length content - ByteString.length closing
        else -1

-- | Runs an action that must fail with a protocol error whose message contains
-- the given text.
expectProtocolError :: Text -> Playwright a -> Playwright ()
expectProtocolError needle action = do
  result <- try @Playwright @PlaywrightError action
  case result of
    Left (ProtocolRpc failure) ->
      assertBool
        ("message should contain " <> show needle <> ", got: " <> show (protocolFailureMessage failure))
        (needle `isInfixOf` protocolFailureMessage failure)
    Left err -> assertFailure ("expected ProtocolRpc, got: " <> show err)
    Right _ -> assertFailure "expected setInputFiles to fail"
