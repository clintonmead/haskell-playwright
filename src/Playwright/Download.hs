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

import Playwright.Monad
  ( Playwright
  )
import Data.Text
  ( Text
  , unpack
  )
import Playwright.Channel
  ( ArtifactTag
  , Channel (Channel, channelGuid)
  , send
  )
import Playwright.Connection
  ( registerEventWaiter
  , waitForWaiter
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( throwDecode
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

waitForDownload :: Page -> Playwright Download
waitForDownload page = do
  takeDownload <- armDownload page
  takeDownload

armDownload :: Page -> Playwright (Playwright Download)
armDownload page = do
  let
    guid = channelGuid (pageChannel page)
  waitId <- registerEventWaiter guid (MethodName "download")
  pure $ do
    params <- waitForWaiter waitId
    event <- throwDecode params
    pure
      ( Download
          { downloadPage = page
          , downloadArtifact = Channel (channelRefGuid (pageDownloadEventArtifact event))
          , downloadUrl = pageDownloadEventUrl event
          , downloadSuggestedFilename =
              SuggestedFilename (pageDownloadEventSuggestedFilename event)
          }
      )

downloadPath :: Download -> Playwright FilePath
downloadPath download = do
  result <-
    send
      (downloadArtifact download)
      (MethodName "pathAfterFinished")
      mkArtifactPathAfterFinishedParams
  pure (unpack (artifactPathAfterFinishedResultValue result))
