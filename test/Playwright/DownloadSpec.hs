module Playwright.DownloadSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text
  ( pack
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
  ( AcceptDownloads (AcceptDownloadsAccept)
  , NewContextOptions (newContextAcceptDownloads)
  , SuggestedFilename (suggestedFilenameToText)
  , defaultGotoOptions
  , defaultNewContextOptions
  , downloadPath
  , downloadSuggestedFilename
  , downloadUrl
  , goto
  , urlFromText
  , urlToText
  , waitForDownload
  )
import Playwright.Harness
  ( assertProtocolContains
  , withChromiumContextWith
  )
import System.Directory
  ( doesFileExist
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "download"
    [ testCase "should report download when navigation turns into download" $
        withApplication (pure downloadApp) $ \port ->
          withChromiumContextWith
            defaultNewContextOptions {newContextAcceptDownloads = Just AcceptDownloadsAccept}
            $ \_ page -> do
              let
                target = urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/downloadWithFilename")
              withAsync (waitForDownload page) $ \handle -> do
                assertProtocolContains
                  "Download is starting"
                  (goto page target defaultGotoOptions)
                download <- wait handle
                assertEqual "url" (urlToText target) (urlToText (downloadUrl download))
                assertEqual "name" "file.txt" (suggestedFilenameToText (downloadSuggestedFilename download))
                path <- downloadPath download
                exists <- doesFileExist path
                assertEqual "exists" True exists
                contents <- BS8.readFile path
                assertEqual "body" (BS8.pack "Hello world") contents
    ]

downloadApp :: Application
downloadApp _ respond =
  respond $
    responseLBS
      status200
      [ (hContentType, "application/octet-stream")
      , ("Content-Disposition", "attachment; filename=file.txt")
      ]
      (LBS8.pack "Hello world")
