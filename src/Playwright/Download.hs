module Playwright.Download
  ( Download
  , downloadPage
  , downloadPath
  , downloadSuggestedFilename
  , downloadUrl
  , SuggestedFilename (..)
  , armDownload
  , waitForDownload
  ) where

import Control.Concurrent.STM
  ( atomically
  , takeTMVar
  )
import Control.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  , unpack
  )
import Playwright.Channel
  ( ArtifactTag
  , Channel (Channel, channelConnection, channelGuid)
  , send
  )
import Playwright.Connection
  ( registerEventWaiter
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Protocol
  ( ArtifactPathAfterFinishedResult (artifactPathAfterFinishedResultValue)
  , PageDownloadEvent (pageDownloadEventArtifact, pageDownloadEventSuggestedFilename, pageDownloadEventUrl)
  , mkArtifactPathAfterFinishedParams
  )
import Playwright.Url
  ( Url
  )

newtype SuggestedFilename = SuggestedFilename
  { suggestedFilenameToText :: Text
  }
  deriving stock (Eq, Show)

data Download = Download
  { downloadPage :: Page
  , downloadArtifact :: Channel ArtifactTag
  , downloadUrl :: Url
  , downloadSuggestedFilename :: SuggestedFilename
  }

waitForDownload :: Page -> IO Download
waitForDownload page = do
  takeDownload <- armDownload page
  takeDownload

armDownload :: Page -> IO (IO Download)
armDownload page = do
  let
    conn = channelConnection (pageChannel page)
    guid = channelGuid (pageChannel page)
  box <- registerEventWaiter conn guid (MethodName "download")
  pure $ do
    params <- atomically (takeTMVar box)
    event <- either throwIO pure (fromJsonValue params)
    pure
      ( Download
          { downloadPage = page
          , downloadArtifact = Channel conn (channelRefGuid (pageDownloadEventArtifact event))
          , downloadUrl = pageDownloadEventUrl event
          , downloadSuggestedFilename =
              SuggestedFilename (pageDownloadEventSuggestedFilename event)
          }
      )

downloadPath :: Download -> IO FilePath
downloadPath download = do
  result <-
    send
      (downloadArtifact download)
      (MethodName "pathAfterFinished")
      mkArtifactPathAfterFinishedParams
  pure (unpack (artifactPathAfterFinishedResultValue result))
