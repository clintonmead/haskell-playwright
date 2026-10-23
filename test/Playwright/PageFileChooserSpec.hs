module Playwright.PageFileChooserSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import UnliftIO.Async
  ( wait
  , withAsync
  )
import Playwright
  ( Frame
  , InputFile (InputFile)
  , InputFileName (InputFileName)
  , Page
  , Playwright
  , armFileChooser
  , childFrames
  , click
  , defaultGotoOptions
  , evaluate
  , evaluateOnLocatorNumber
  , evaluateOnLocatorText
  , evaluateText
  , fileChoosersEqual
  , fileChooserMultiple
  , fileChooserPage
  , fileChooserSetFiles
  , goto
  , locatorOnFrame
  , mainFrame
  , pagesEqual
  , selectorFromText
  , setContentOnFrame
  , urlFromText
  , milliseconds
  , setContent
  , setDefaultTimeout
  , waitForFileChooser
  , waitForFileChooserWith
  )
import Playwright.Harness
  ( assertWaitTimeout
  , attachFrame
  , locate
  , readTestAsset
  , withAssets
  , withChromiumPage
  , assertBool
  , assertEqual
  , assertFailure
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-filechooser"
    [ testCase "should emit event once" $
        withChromiumPage $ \page -> do
          setContent page "<input type=file>"
          takeChooser <- armFileChooser page
          withAsync takeChooser $ \handle -> do
            click (locate page "input")
            chooser <- wait handle
            assertEqual "single" False (fileChooserMultiple chooser)
    , testCase "should respect default timeout when there is no custom timeout" $
        withChromiumPage $ \page ->
          case milliseconds 1 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              setDefaultTimeout page timeout
              assertWaitTimeout (waitForFileChooser page)
    , testCase "should prioritize exact timeout over default timeout" $
        withChromiumPage $ \page ->
          case (milliseconds 0, milliseconds 1) of
            (Just disabled, Just timeout) -> do
              setDefaultTimeout page disabled
              assertWaitTimeout (waitForFileChooserWith page timeout)
            _ ->
              assertFailure "timeout"
    , testCase "should work with no timeout" $
        withChromiumPage $ \page ->
          case milliseconds 0 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              withAsync (waitForFileChooserWith page timeout) $ \pending -> do
                _ <-
                  evaluate
                    page
                    "() => { window.setTimeout(() => { const el = document.createElement('input'); el.type = 'file'; el.click(); }, 50); }"
                chooser <- wait pending
                assertEqual "chooser" False (fileChooserMultiple chooser)
    , testCase "should work when file input is attached to DOM" $
        withChromiumPage $ \page -> do
          setContent page "<input type=file>"
          takeChooser <- armFileChooser page
          click (locate page "input")
          _ <- takeChooser
          pure ()
    , testCase "should emit event for iframe" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          frame <- attachFrame page "frame1" (prefix <> "/empty.html")
          setContentOnFrame frame "<input type=file>"
          takeChooser <- armFileChooser page
          click (locatorOnFrame page frame (selectorFromText "input"))
          _ <- takeChooser
          pure ()
    , testCase "should accept single file" $
        withChromiumPage $ \page -> do
          setContent page "<input type=file oninput='javascript:console.timeStamp()'>"
          takeChooser <- armFileChooser page
          click (locate page "input")
          chooser <- takeChooser
          assertEqual "page" True (pagesEqual (fileChooserPage chooser) page)
          bytes <- readTestAsset "file-to-upload.txt"
          fileChooserSetFiles chooser [InputFile (InputFileName "file-to-upload.txt") bytes]
          assertEqual "count" (1 :: Double) =<< evaluateOnLocatorNumber (locate page "input") "input => input.files.length"
          assertEqual "name" "file-to-upload.txt"
            =<< evaluateOnLocatorText (locate page "input") "input => input.files[0].name"
    , testCase "should be able to read selected file" $
        withChromiumPage $ \page -> do
          setContent page "<input type=file>"
          takeChooser <- armFileChooser page
          withAsync (evaluateText page readSelectedFileScript) $ \content -> do
            chooser <- takeChooser
            bytes <- readTestAsset "file-to-upload.txt"
            fileChooserSetFiles chooser [InputFile (InputFileName "file-to-upload.txt") bytes]
            assertEqual "content" "contents of the file" =<< wait content
    , testCase "should be able to reset selected files with empty file list" $
        withChromiumPage $ \page -> do
          setContent page "<input type=file>"
          takeFirst <- armFileChooser page
          withAsync (evaluateText page countSelectedFilesScript) $ \first -> do
            chooser <- takeFirst
            bytes <- readTestAsset "file-to-upload.txt"
            fileChooserSetFiles chooser [InputFile (InputFileName "file-to-upload.txt") bytes]
            assertEqual "selected" "1" =<< wait first
          takeSecond <- armFileChooser page
          withAsync (evaluateText page countSelectedFilesScript) $ \second -> do
            chooser <- takeSecond
            fileChooserSetFiles chooser []
            assertEqual "reset" "0" =<< wait second
    , testCase "should work for single file pick" $
        withChromiumPage $ \page -> do
          setContent page "<input type=file>"
          takeChooser <- armFileChooser page
          click (locate page "input")
          chooser <- takeChooser
          assertEqual "single" False (fileChooserMultiple chooser)
    , testCase "should work for \"multiple\"" $
        withChromiumPage $ \page -> do
          setContent page "<input multiple type=file>"
          takeChooser <- armFileChooser page
          click (locate page "input")
          chooser <- takeChooser
          assertEqual "multiple" True (fileChooserMultiple chooser)
    , testCase "should emit event after navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page "<input type=file>"
          takeFirst <- armFileChooser page
          click (locate page "input")
          _ <- takeFirst
          _ <- goto page (urlFromText (Text.replace "127.0.0.1" "localhost" prefix <> "/empty.html")) defaultGotoOptions
          setContent page "<input type=file>"
          takeSecond <- armFileChooser page
          click (locate page "input")
          _ <- takeSecond
          pure ()
    , testCase "should trigger listener added before navigation" $
        withAssets $ \prefix page -> do
          takeChooser <- armFileChooser page
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- goto page (urlFromText (Text.replace "127.0.0.1" "localhost" prefix <> "/empty.html")) defaultGotoOptions
          setContent page "<input type=file>"
          click (locate page "input")
          _ <- takeChooser
          pure ()

    , testCase "should work when file input is not attached to DOM" $
        withChromiumPage $ \page -> do
          takeChooser <- armFileChooser page
          withAsync (evaluateText page detachedFileInputScript) $ \content -> do
            chooser <- takeChooser
            bytes <- readTestAsset "file-to-upload.txt"
            fileChooserSetFiles chooser [InputFile (InputFileName "file-to-upload.txt") bytes]
            assertEqual "content" "contents of the file" =<< wait content
    , testCase "should respect timeout" $
        withChromiumPage $ \page ->
          case milliseconds 1 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertWaitTimeout (waitForFileChooserWith page timeout)
    , testCase "should not throw when filechooser belongs to iframe" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          frame <- firstChildFrame page
          setContentOnFrame frame clickOpensChooserContent
          takeChooser <- armFileChooser page
          withAsync takeChooser $ \handle -> do
            click (locatorOnFrame page frame (selectorFromText "div"))
            _ <- wait handle
            pure ()
          assertEqual "done" "true" =<< evaluateText page "() => String(window.__done === true)"
    , testCase "should not throw when frame is detached immediately" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          frame <- firstChildFrame page
          setContentOnFrame frame clickOpensChooserAndDetachesContent
          -- Subscribes to file choosers, as the upstream no-op listener does.
          _ <- armFileChooser page
          click (locatorOnFrame page frame (selectorFromText "div"))
          assertEqual "done" "true" =<< evaluateText page "() => String(window.__done === true)"
    , testCase "should return the same file chooser when there are many watchdogs simultaneously" $
        withChromiumPage $ \page -> do
          setContent page "<input type=file>"
          takeFirst <- armFileChooser page
          takeSecond <- armFileChooser page
          withAsync takeFirst $ \first ->
            withAsync takeSecond $ \second -> do
              click (locate page "input")
              chooserFirst <- wait first
              chooserSecond <- wait second
              assertBool "same chooser" (fileChoosersEqual chooserFirst chooserSecond)
    ]

-- | The first child frame of the main frame, which must exist.
firstChildFrame :: Page -> Playwright Frame
firstChildFrame page = do
  children <- childFrames (mainFrame page)
  case children of
    child : _ -> pure child
    [] -> assertFailure "expected a child frame"

-- | Frame content whose click opens a file chooser and records that the handler ran.
clickOpensChooserContent :: Text
clickOpensChooserContent =
  "<div>Click me</div><script>document.querySelector('div').addEventListener('click', () => { const input = document.createElement('input'); input.type = 'file'; input.click(); window.parent.__done = true; });</script>"

-- | Like clickOpensChooserContent, but removes the iframe from the parent during the click.
clickOpensChooserAndDetachesContent :: Text
clickOpensChooserAndDetachesContent =
  "<div>Click me</div><script>document.querySelector('div').addEventListener('click', () => { const input = document.createElement('input'); input.type = 'file'; input.click(); window.parent.__done = true; const iframe = window.parent.document.querySelector('iframe'); iframe.remove(); });</script>"

-- | Clicks the file input, waits for the selection, and returns the file text.
readSelectedFileScript :: Text
readSelectedFileScript =
  "async () => { const picker = document.querySelector('input'); picker.click(); await new Promise(x => picker.oninput = x); const reader = new FileReader(); const promise = new Promise(fulfill => reader.onload = fulfill); reader.readAsText(picker.files[0]); await promise; return reader.result; }"

-- | Clicks the file input, waits for the selection, and returns how many files it holds.
countSelectedFilesScript :: Text
countSelectedFilesScript =
  "async () => { const picker = document.querySelector('input'); picker.click(); await new Promise(x => picker.oninput = x); return String(picker.files.length); }"

-- | Creates an input that is not in the page, opens it, and returns the file text.
detachedFileInputScript :: Text
detachedFileInputScript =
  "async () => { const el = document.createElement('input'); el.type = 'file'; el.click(); await new Promise(x => el.oninput = x); const reader = new FileReader(); const promise = new Promise(fulfill => reader.onload = fulfill); reader.readAsText(el.files[0]); await promise; return reader.result; }"
