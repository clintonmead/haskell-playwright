module Playwright.Download
  ( Download
  , downloadCancel
  , downloadDelete
  , downloadFailure
  , downloadPage
  , downloadPath
  , downloadSaveAs
  , downloadSuggestedFilename
  , downloadUrl
  , SuggestedFilename (..)
  , armDownload
  , waitForDownload
  ) where

import Playwright.Monad
  ( Playwright
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import System.Directory
  ( makeAbsolute
  )
import Data.Text
  ( Text
  , pack
  , unpack
  )
import Playwright.Channel
  ( ArtifactTag
  , Channel (Channel, channelGuid)
  , send
  )
import Playwright.Connection
  ( registerEventWaiter
  , waitForWaiterUnlessClosed
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
  ( ArtifactCancelResult
  , ArtifactDeleteResult
  , ArtifactFailureResult (artifactFailureResultError)
  , ArtifactPathAfterFinishedResult (artifactPathAfterFinishedResultValue)
  , ArtifactSaveAsResult
  , PageDownloadEvent (pageDownloadEventArtifact, pageDownloadEventSuggestedFilename, pageDownloadEventUrl)
  , mkArtifactCancelParams
  , mkArtifactDeleteParams
  , mkArtifactFailureParams
  , mkArtifactPathAfterFinishedParams
  , mkArtifactSaveAsParams
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
    params <- waitForWaiterUnlessClosed waitId guid
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

downloadSaveAs :: Download -> FilePath -> Playwright ()
downloadSaveAs download dest = do
  absPath <- liftIO (makeAbsolute dest)
  _ :: ArtifactSaveAsResult <-
    send
      (downloadArtifact download)
      (MethodName "saveAs")
      (mkArtifactSaveAsParams (pack absPath))
  pure ()

downloadDelete :: Download -> Playwright ()
downloadDelete download = do
  _ :: ArtifactDeleteResult <-
    send
      (downloadArtifact download)
      (MethodName "delete")
      mkArtifactDeleteParams
  pure ()

downloadCancel :: Download -> Playwright ()
downloadCancel download = do
  _ :: ArtifactCancelResult <-
    send
      (downloadArtifact download)
      (MethodName "cancel")
      mkArtifactCancelParams
  pure ()

downloadFailure :: Download -> Playwright (Maybe Text)
downloadFailure download = do
  result <-
    send
      (downloadArtifact download)
      (MethodName "failure")
      mkArtifactFailureParams
  pure (artifactFailureResultError result)
