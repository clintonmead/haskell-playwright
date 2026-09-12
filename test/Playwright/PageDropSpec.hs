module Playwright.PageDropSpec
  ( tests
  ) where

import UnliftIO.Exception
  ( try
  )
import Data.ByteString.Char8 qualified as BS8
import Data.Text
  ( Text
  , isInfixOf
  , toLower
  )
import Playwright
  ( DropData (DropData)
  , DropFile (DropFile)
  , DropPayload (DropPayload, dropPayloadData, dropPayloadFiles)
  , InputFileName (InputFileName)
  , MimeType (MimeType)
  , Page
  , dropOnto
  , emptyDropPayload
  , evaluateNumber
  , evaluateText
  , setContent
  , waitForFunction
  , Playwright
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
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
    "page-drop"
    [ testCase "should drop a file payload" $
        withChromiumPage $ \page -> do
          setupDropzone page
          dropOnto
            (locate page "#dropzone")
            emptyDropPayload
              { dropPayloadFiles =
                  [ DropFile
                      (InputFileName "note.txt")
                      (Just (MimeType "text/plain"))
                      (BS8.pack "hello")
                  ]
              }
          waitForDrop page
          assertEqual "name" "note.txt" =<< evaluateText page "() => window.__dropInfo.files[0].name"
          assertEqual "type" "text/plain" =<< evaluateText page "() => window.__dropInfo.files[0].type"
          assertEqual "size" (5 :: Double) =<< evaluateNumber page "() => window.__dropInfo.files[0].size"
          assertEqual "text" "hello" =<< evaluateText page "() => window.__dropInfo.files[0].text"
          assertEqual "data" "{}" =<< evaluateText page "() => JSON.stringify(window.__dropInfo.data)"
    , testCase "should drop multiple file payloads" $
        withChromiumPage $ \page -> do
          setupDropzone page
          dropOnto
            (locate page "#dropzone")
            emptyDropPayload
              { dropPayloadFiles =
                  [ DropFile (InputFileName "a.txt") (Just (MimeType "text/plain")) (BS8.pack "AAA")
                  , DropFile (InputFileName "b.txt") (Just (MimeType "text/plain")) (BS8.pack "BB")
                  ]
              }
          waitForDrop page
          assertEqual "a" "AAA" =<< evaluateText page "() => window.__dropInfo.files[0].text"
          assertEqual "b" "BB" =<< evaluateText page "() => window.__dropInfo.files[1].text"
    , testCase "should drop clipboard-like data" $
        withChromiumPage $ \page -> do
          setupDropzone page
          dropOnto
            (locate page "#dropzone")
            emptyDropPayload
              { dropPayloadData =
                  [ DropData (MimeType "text/plain") "hello world"
                  , DropData (MimeType "text/uri-list") "https://example.com"
                  ]
              }
          waitForDrop page
          assertEqual "files" (0 :: Double) =<< evaluateNumber page "() => window.__dropInfo.files.length"
          assertEqual "plain" "hello world" =<< evaluateText page "() => window.__dropInfo.data['text/plain']"
          assertEqual "uri" "https://example.com" =<< evaluateText page "() => window.__dropInfo.data['text/uri-list']"
    , testCase "should drop files and data together" $
        withChromiumPage $ \page -> do
          setupDropzone page
          dropOnto
            (locate page "#dropzone")
            DropPayload
              { dropPayloadFiles =
                  [ DropFile (InputFileName "mix.txt") (Just (MimeType "text/plain")) (BS8.pack "mix")
                  ]
              , dropPayloadData =
                  [ DropData (MimeType "text/plain") "label"
                  ]
              }
          waitForDrop page
          assertEqual "file" "mix" =<< evaluateText page "() => window.__dropInfo.files[0].text"
          assertEqual "label" "label" =<< evaluateText page "() => window.__dropInfo.data['text/plain']"
    , testCase "should throw when target does not accept drop" $
        withChromiumPage $ \page -> do
          setContent page "<div id=\"dropzone\" style=\"width: 200px; height: 100px;\"></div>"
          assertProtocolContains
            "did not accept the drop"
            ( dropOnto
                (locate page "#dropzone")
                emptyDropPayload
                  { dropPayloadData = [DropData (MimeType "text/plain") "nope"]
                  }
            )
    , testCase "should throw when neither files nor data provided" $
        withChromiumPage $ \page -> do
          setupDropzone page
          result <- try (dropOnto (locate page "#dropzone") emptyDropPayload)
          case result of
            Left (DecodeFailure message) ->
              assertBool "message" (containsFold "At least one of \"files\" or \"data\"" message)
            Left err ->
              assertFailure ("expected DecodeFailure, got: " <> show err)
            Right _ ->
              assertFailure "expected empty drop to fail"
    ]

setupDropzone :: Page -> Playwright ()
setupDropzone page =
  setContent
    page
    "<style>#dropzone { width: 300px; height: 200px; border: 2px dashed #888; }</style><div id=\"dropzone\"></div><script>window.__dropInfo = null; const zone = document.getElementById('dropzone'); zone.addEventListener('dragenter', e => e.preventDefault()); zone.addEventListener('dragover', e => e.preventDefault()); zone.addEventListener('drop', async e => { e.preventDefault(); const files = []; for (const file of e.dataTransfer.files) files.push({ name: file.name, type: file.type, size: file.size, text: await file.text() }); const data = {}; for (const t of e.dataTransfer.types) { if (t !== 'Files') data[t] = e.dataTransfer.getData(t); } window.__dropInfo = { files, data }; });</script>"

waitForDrop :: Page -> Playwright ()
waitForDrop page = do
  _ <- waitForFunction page "() => window.__dropInfo"
  pure ()

containsFold :: Text -> Text -> Bool
containsFold needle haystack =
  toLower needle `isInfixOf` toLower haystack
