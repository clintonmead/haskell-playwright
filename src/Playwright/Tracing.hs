module Playwright.Tracing
  ( Tracing (..)
  , TracingStartOptions (..)
  , defaultTracingStartOptions
  , tracingFor
  , tracingGroup
  , tracingGroupEnd
  , tracingStart
  , tracingStartChunk
  , tracingStartWith
  , tracingStop
  , tracingStopChunk
  , tracingStopChunkTo
  , tracingStopTo
  ) where

import Data.Text
  ( Text
  , pack
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Playwright.BrowserContext
  ( BrowserContext (browserContextChannel)
  )
import Playwright.Channel
  ( ArtifactTag
  , Channel (Channel)
  , TracingTag
  , channelGuid
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
import UnliftIO.Exception
  ( SomeException
  , throwIO
  , try
  )
import Playwright.Protocol
  ( ArtifactSaveAsResult
  , BrowserContextInitializer (browserContextInitializerTracing)
  , TracingTracingGroupEndResult
  , TracingTracingGroupResult
  , TracingTracingStartChunkResult (tracingTracingStartChunkResultTraceName)
  , TracingTracingStartChunkParams (tracingTracingStartChunkParamsName, tracingTracingStartChunkParamsTitle)
  , TracingTracingStartParams (tracingTracingStartParamsLive, tracingTracingStartParamsName, tracingTracingStartParamsScreencast, tracingTracingStartParamsSnapshotAria, tracingTracingStartParamsSnapshotDom, tracingTracingStartParamsSnapshotScreen)
  , TracingTracingStartResult
  , TracingTracingStopChunkParamsMode (TracingTracingStopChunkParamsModeArchive, TracingTracingStopChunkParamsModeDiscard)
  , TracingTracingStopChunkResult (tracingTracingStopChunkResultArtifact)
  , TracingTracingStopResult
  , mkArtifactSaveAsParams
  , mkTracingTracingGroupEndParams
  , mkTracingTracingGroupParams
  , mkTracingTracingStartChunkParams
  , mkTracingTracingStartParams
  , mkTracingTracingStopChunkParams
  , mkTracingTracingStopParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import System.Directory
  ( makeAbsolute
  )

-- | The tracing object of a browser context.
newtype Tracing = Tracing
  { tracingChannel :: Channel TracingTag
  }

data TracingStartOptions = TracingStartOptions
  { tracingStartOptionsName :: Maybe Text
  , tracingStartOptionsScreenshots :: Maybe Bool
  , tracingStartOptionsSnapshots :: Maybe Bool
  , tracingStartOptionsLive :: Maybe Bool
  }
  deriving stock (Eq, Show)

defaultTracingStartOptions :: TracingStartOptions
defaultTracingStartOptions =
  TracingStartOptions
    { tracingStartOptionsName = Nothing
    , tracingStartOptionsScreenshots = Nothing
    , tracingStartOptionsSnapshots = Nothing
    , tracingStartOptionsLive = Nothing
    }

-- | Looks up the tracing object that belongs to a browser context.
tracingFor :: BrowserContext -> Playwright Tracing
tracingFor ctx = do
  obj <- lookupObject (channelGuid (browserContextChannel ctx))
  initializer <- throwDecode (remoteInitializer obj)
  pure
    Tracing
      { tracingChannel =
          Channel (channelRefGuid (browserContextInitializerTracing initializer))
      }

tracingStart :: BrowserContext -> Playwright ()
tracingStart ctx = tracingStartWith ctx defaultTracingStartOptions

tracingStartWith :: BrowserContext -> TracingStartOptions -> Playwright ()
tracingStartWith ctx options = do
  tracing <- tracingFor ctx
  let params =
        (mkTracingTracingStartParams)
          { tracingTracingStartParamsName = tracingStartOptionsName options
          , tracingTracingStartParamsScreencast = tracingStartOptionsScreenshots options
          , tracingTracingStartParamsSnapshotDom = tracingStartOptionsSnapshots options
          , tracingTracingStartParamsSnapshotScreen = tracingStartOptionsScreenshots options
          , tracingTracingStartParamsSnapshotAria = tracingStartOptionsSnapshots options
          , tracingTracingStartParamsLive = tracingStartOptionsLive options
          }
  _ :: TracingTracingStartResult <-
    send (tracingChannel tracing) (MethodName "tracingStart") params
  _ :: TracingTracingStartChunkResult <-
    send
      (tracingChannel tracing)
      (MethodName "tracingStartChunk")
      mkTracingTracingStartChunkParams
  pure ()

-- | Starts a new trace chunk, optionally with a name and a title.
-- Returns the name of the chunk's trace.
tracingStartChunk :: BrowserContext -> Maybe Text -> Maybe Text -> Playwright Text
tracingStartChunk ctx name title = do
  tracing <- tracingFor ctx
  let params =
        (mkTracingTracingStartChunkParams)
          { tracingTracingStartChunkParamsName = name
          , tracingTracingStartChunkParamsTitle = title
          }
  result :: TracingTracingStartChunkResult <-
    send (tracingChannel tracing) (MethodName "tracingStartChunk") params
  pure (tracingTracingStartChunkResultTraceName result)

-- | Stops the trace, discarding its data.
tracingStop :: BrowserContext -> Playwright ()
tracingStop ctx = do
  tracing <- tracingFor ctx
  _ :: TracingTracingStopResult <-
    send (tracingChannel tracing) (MethodName "tracingStop") mkTracingTracingStopParams
  pure ()

-- | Stops the trace and writes it to the given path as a zip archive.
tracingStopTo :: BrowserContext -> FilePath -> Playwright ()
tracingStopTo ctx dest = do
  -- Mirrors the upstream client: always stop the tracing session, even when
  -- saving the chunk fails, then rethrow the save error.
  saved <- try (tracingStopChunkTo ctx dest)
  stopped <- try (tracingStop ctx)
  case saved of
    Left err -> throwIO (err :: SomeException)
    Right () -> case stopped of
      Left err -> throwIO (err :: SomeException)
      Right () -> pure ()

-- | Stops the current chunk, discarding its data.
tracingStopChunk :: BrowserContext -> Playwright ()
tracingStopChunk ctx = do
  tracing <- tracingFor ctx
  _ :: TracingTracingStopChunkResult <-
    send
      (tracingChannel tracing)
      (MethodName "tracingStopChunk")
      (mkTracingTracingStopChunkParams TracingTracingStopChunkParamsModeDiscard)
  pure ()

-- | Stops the current chunk and writes it to the given path as a zip archive.
tracingStopChunkTo :: BrowserContext -> FilePath -> Playwright ()
tracingStopChunkTo ctx dest = do
  tracing <- tracingFor ctx
  result :: TracingTracingStopChunkResult <-
    send
      (tracingChannel tracing)
      (MethodName "tracingStopChunk")
      (mkTracingTracingStopChunkParams TracingTracingStopChunkParamsModeArchive)
  case tracingTracingStopChunkResultArtifact result of
    Nothing -> pure ()
    Just artifactRef -> do
      absPath <- liftIO (makeAbsolute dest)
      _ :: ArtifactSaveAsResult <-
        send
          (Channel (channelRefGuid artifactRef) :: Channel ArtifactTag)
          (MethodName "saveAs")
          (mkArtifactSaveAsParams (pack absPath))
      pure ()

-- | Opens a named group in the trace.
tracingGroup :: BrowserContext -> Text -> Playwright ()
tracingGroup ctx name = do
  tracing <- tracingFor ctx
  _ :: TracingTracingGroupResult <-
    send
      (tracingChannel tracing)
      (MethodName "tracingGroup")
      (mkTracingTracingGroupParams name)
  pure ()

-- | Closes the innermost group opened by 'tracingGroup'.
tracingGroupEnd :: BrowserContext -> Playwright ()
tracingGroupEnd ctx = do
  tracing <- tracingFor ctx
  _ :: TracingTracingGroupEndResult <-
    send
      (tracingChannel tracing)
      (MethodName "tracingGroupEnd")
      mkTracingTracingGroupEndParams
  pure ()
