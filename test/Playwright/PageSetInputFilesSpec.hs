module Playwright.PageSetInputFilesSpec
  ( tests
  ) where

import Playwright
  ( InputFile (InputFile)
  , InputFileName (InputFileName)
  , evaluate
  , evaluateOnElementText
  , evaluateOnLocatorNumber
  , evaluateOnLocatorText
  , evaluateTexts
  , setContent
  , setInputFiles
  , querySelector
  , selectorFromText
  , setInputFilesHandle
  )
import Playwright.Harness
  ( locate
  , readTestAsset
  , requireHandle
  , withAssetPage
  , withChromiumPage
  , assertEqual
  , locate
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

    ]
