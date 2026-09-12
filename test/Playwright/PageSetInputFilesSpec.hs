module Playwright.PageSetInputFilesSpec
  ( tests
  ) where

import Playwright
  ( InputFile (InputFile)
  , InputFileName (InputFileName)
  , evaluateOnElementText
  , querySelector
  , selectorFromText
  , setInputFilesHandle
  )
import Playwright.Harness
  ( readTestAsset
  , requireHandle
  , withAssetPage
  , assertEqual
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
    ]
