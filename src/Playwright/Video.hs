module Playwright.Video
  ( Video
  , pageVideo
  , videoDelete
  , videoPath
  , videoSaveAs
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( pack
  , unpack
  )
import System.Directory
  ( makeAbsolute
  )
import Playwright.Channel
  ( ArtifactTag
  , Channel (Channel, channelGuid)
  , send
  )
import Playwright.Connection
  ( lookupObject
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
import Playwright.Monad
  ( Playwright
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Protocol
  ( ArtifactDeleteResult
  , ArtifactInitializer (artifactInitializerAbsolutePath)
  , ArtifactSaveAsResult
  , PageInitializer (pageInitializerVideo)
  , mkArtifactDeleteParams
  , mkArtifactSaveAsParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )

-- | A page's video recording, backed by its artifact.
newtype Video = Video
  { videoArtifact :: Channel ArtifactTag
  }

-- | The page's video, or 'Nothing' unless its context records video.
pageVideo :: Page -> Playwright (Maybe Video)
pageVideo page = do
  obj <- lookupObject (channelGuid (pageChannel page))
  initializer <- throwDecode (remoteInitializer obj)
  pure (fmap (Video . Channel . channelRefGuid) (pageInitializerVideo initializer))

-- | The absolute path of the video file. Does not wait for recording to finish.
videoPath :: Video -> Playwright FilePath
videoPath video = do
  obj <- lookupObject (channelGuid (videoArtifact video))
  initializer <- throwDecode (remoteInitializer obj)
  pure (unpack (artifactInitializerAbsolutePath initializer))

-- | Copies the video to the given path, waiting for recording to finish.
videoSaveAs :: Video -> FilePath -> Playwright ()
videoSaveAs video dest = do
  absPath <- liftIO (makeAbsolute dest)
  _ :: ArtifactSaveAsResult <-
    send
      (videoArtifact video)
      (MethodName "saveAs")
      (mkArtifactSaveAsParams (pack absPath))
  pure ()

videoDelete :: Video -> Playwright ()
videoDelete video = do
  _ :: ArtifactDeleteResult <-
    send
      (videoArtifact video)
      (MethodName "delete")
      mkArtifactDeleteParams
  pure ()
