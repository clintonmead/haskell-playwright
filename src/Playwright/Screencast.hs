module Playwright.Screencast
  ( ScreencastFrame (..)
  , ScreencastHandle
  , ScreencastOptions (..)
  , ScreencastSize (..)
  , defaultScreencastOptions
  , screencastDispose
  , screencastStart
  , screencastStop
  ) where

import Control.Monad
  ( forM_
  , when
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString
  ( ByteString
  )
import Data.Maybe
  ( isJust
  )
import Data.Scientific (toRealFloat)
import Data.Text
  ( pack
  )
import Data.Unique
  ( hashUnique
  , newUnique
  )
import Playwright.Channel
  ( ArtifactTag
  , Channel (Channel, channelGuid)
  , send
  )
import Playwright.Connection
  ( deleteScreencast
  , lookupScreencast
  , putScreencast
  , registerEventHandler
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (ProtocolFailure, protocolFailureMessage, protocolFailureName, protocolFailureStack)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.Json.ProtocolBinary
  ( protocolBinaryToByteString
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
  ( ArtifactSaveAsResult
  , PageScreencastFrameAckResult
  , PageScreencastFrameEvent (pageScreencastFrameEventData, pageScreencastFrameEventFrameId, pageScreencastFrameEventTimestamp, pageScreencastFrameEventViewportHeight, pageScreencastFrameEventViewportWidth)
  , PageScreencastStartParams (pageScreencastStartParamsFps, pageScreencastStartParamsQuality, pageScreencastStartParamsRecord, pageScreencastStartParamsSendFrames, pageScreencastStartParamsSize)
  , PageScreencastStartResult (pageScreencastStartResultArtifact)
  , PageScreencastStopResult
  , mkArtifactSaveAsParams
  , mkPageScreencastFrameAckParams
  , mkPageScreencastStartParams
  , mkPageScreencastStartParamsSize
  , mkPageScreencastStopParams
  )
import Playwright.Session
  ( ScreencastSession (ScreencastSession, screencastSessionArtifact, screencastSessionGeneration, screencastSessionPath)
  )
import System.Directory
  ( makeAbsolute
  )
import UnliftIO.Exception
  ( finally
  , throwIO
  )

-- | Requested size of the screencast frames, in CSS pixels.
data ScreencastSize = ScreencastSize
  { screencastSizeWidth :: Int
  , screencastSizeHeight :: Int
  }
  deriving stock (Eq, Show)

-- | One JPEG frame of a running screencast.
data ScreencastFrame = ScreencastFrame
  { screencastFrameData :: ByteString
  , screencastFrameTimestamp :: Double
  , screencastFrameViewportWidth :: Int
  , screencastFrameViewportHeight :: Int
  }
  deriving stock (Eq, Show)

data ScreencastOptions = ScreencastOptions
  { screencastOptionsPath :: Maybe FilePath
  -- ^ Records the page to this file when the screencast stops.
  , screencastOptionsSize :: Maybe ScreencastSize
  , screencastOptionsQuality :: Maybe Int
  , screencastOptionsFps :: Maybe Int
  , screencastOptionsOnFrame :: Maybe (ScreencastFrame -> Playwright ())
  -- ^ Receives each frame. The next frame waits until this action returns.
  }

defaultScreencastOptions :: ScreencastOptions
defaultScreencastOptions =
  ScreencastOptions
    { screencastOptionsPath = Nothing
    , screencastOptionsSize = Nothing
    , screencastOptionsQuality = Nothing
    , screencastOptionsFps = Nothing
    , screencastOptionsOnFrame = Nothing
    }

-- | Identifies one started screencast, so disposing it does not stop a later
-- screencast that replaced it.
data ScreencastHandle = ScreencastHandle
  { screencastHandlePage :: Page
  , screencastHandleGeneration :: Int
  }

-- | Starts a screencast on the page. Fails with "Screencast is already
-- started" while another screencast on the same page is running.
screencastStart :: Page -> ScreencastOptions -> Playwright ScreencastHandle
screencastStart page options = do
  let
    pageGuid = channelGuid (pageChannel page)
  existing <- lookupScreencast pageGuid
  when (isJust existing) $
    throwIO
      ( ProtocolRpc
          ProtocolFailure
            { protocolFailureName = "Error"
            , protocolFailureMessage = "Screencast is already started"
            , protocolFailureStack = Nothing
            }
      )
  generation <- liftIO (hashUnique <$> newUnique)
  absPath <- traverse (liftIO . makeAbsolute) (screencastOptionsPath options)
  let
    params =
      mkPageScreencastStartParams
        { pageScreencastStartParamsSize = fmap sizeParams (screencastOptionsSize options)
        , pageScreencastStartParamsQuality = screencastOptionsQuality options
        , pageScreencastStartParamsFps = screencastOptionsFps options
        , pageScreencastStartParamsSendFrames = Just (isJust (screencastOptionsOnFrame options))
        , pageScreencastStartParamsRecord = Just (isJust absPath)
        }
  result <- send (pageChannel page) (MethodName "screencastStart") params
  registerEventHandler pageGuid (MethodName "screencastFrame") $ \value -> do
    current <- lookupScreencast pageGuid
    when (fmap screencastSessionGeneration current == Just generation) $ do
      event <- throwDecode value
      let
        frame =
          ScreencastFrame
            { screencastFrameData = protocolBinaryToByteString (pageScreencastFrameEventData event)
            , screencastFrameTimestamp = toRealFloat (pageScreencastFrameEventTimestamp event)
            , screencastFrameViewportWidth = pageScreencastFrameEventViewportWidth event
            , screencastFrameViewportHeight = pageScreencastFrameEventViewportHeight event
            }
      forM_ (screencastOptionsOnFrame options) (\onFrame -> onFrame frame)
        `finally` ackFrame page (pageScreencastFrameEventFrameId event)
  putScreencast
    pageGuid
    ScreencastSession
      { screencastSessionGeneration = generation
      , screencastSessionArtifact = channelRefGuid <$> pageScreencastStartResultArtifact result
      , screencastSessionPath = absPath
      }
  pure (ScreencastHandle page generation)
  where
    sizeParams size =
      mkPageScreencastStartParamsSize
        (screencastSizeWidth size)
        (screencastSizeHeight size)

-- | Stops the screencast on the page. Does nothing when none is running. When
-- the screencast recorded to a path, the recording is saved there.
screencastStop :: Page -> Playwright ()
screencastStop page = do
  let
    pageGuid = channelGuid (pageChannel page)
  current <- lookupScreencast pageGuid
  forM_ current $ \screencast -> do
    deleteScreencast pageGuid
    _ :: PageScreencastStopResult <-
      send (pageChannel page) (MethodName "screencastStop") mkPageScreencastStopParams
    forM_ ((,) <$> screencastSessionArtifact screencast <*> screencastSessionPath screencast) $
      \(artifactGuid, path) -> do
        _ :: ArtifactSaveAsResult <-
          send (Channel artifactGuid :: Channel ArtifactTag) (MethodName "saveAs") (mkArtifactSaveAsParams (pack path))
        pure ()

-- | Stops the screencast this handle started, if it is still the running one.
screencastDispose :: ScreencastHandle -> Playwright ()
screencastDispose handle = do
  let
    page = screencastHandlePage handle
    pageGuid = channelGuid (pageChannel page)
  current <- lookupScreencast pageGuid
  when (fmap screencastSessionGeneration current == Just (screencastHandleGeneration handle)) $
    screencastStop page

ackFrame :: Page -> Int -> Playwright ()
ackFrame page frameId = do
  _ :: PageScreencastFrameAckResult <-
    send (pageChannel page) (MethodName "screencastFrameAck") (mkPageScreencastFrameAckParams frameId)
  pure ()
