module Playwright.RecordHar
  ( RecordHar (..)
  , RecordHarContent (..)
  , RecordHarMode (..)
  , exportRecordHar
  , startRecordHar
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , object
  , optionalField'
  , (.=)
  )
import Control.Monad
  ( forM_
  , when
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Aeson
  ( FromJSON
  )
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson (parseMaybe)
import Data.Aeson.Key qualified as Key
import Data.List
  ( isSuffixOf
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  )
import Playwright.Connection
  ( deleteHarRecorder
  , lookupHarRecorder
  , lookupObject
  , lookupPlaywrightGuid
  , putHarRecorder
  , sendCommand
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure
      ( ProtocolFailure
      , protocolFailureMessage
      , protocolFailureName
      , protocolFailureStack
      )
  )
import Playwright.Guid
  ( Guid
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
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.Session
  ( HarRecorder (HarRecorder, harRecorderId, harRecorderPath, harRecorderTracing)
  )
import System.Directory
  ( makeAbsolute
  )
import UnliftIO.Exception
  ( throwIO
  )
import Data.Text qualified as Text

-- | What the HAR records for response bodies.
data RecordHarContent
  = RecordHarContentEmbed
  | RecordHarContentAttach
  | RecordHarContentOmit
  deriving stock (Eq, Show)

-- | How much the HAR records: everything, or only what replaying needs.
data RecordHarMode
  = RecordHarModeFull
  | RecordHarModeMinimal
  deriving stock (Eq, Show)

-- | HAR recording options for a browser context. A @.zip@ path records the
-- resources attached to the archive; any other path writes a plain HAR file.
data RecordHar = RecordHar
  { recordHarPath :: FilePath
  , recordHarContent :: Maybe RecordHarContent
  , recordHarMode :: Maybe RecordHarMode
  , recordHarUrlGlob :: Maybe Text
  }
  deriving stock (Eq, Show)

isZipPath :: FilePath -> Bool
isZipPath = (".zip" `isSuffixOf`)

contentText :: RecordHarContent -> Text
contentText content =
  case content of
    RecordHarContentEmbed -> "embed"
    RecordHarContentAttach -> "attach"
    RecordHarContentOmit -> "omit"

modeText :: RecordHarMode -> Text
modeText mode =
  case mode of
    RecordHarModeFull -> "full"
    RecordHarModeMinimal -> "minimal"

-- | Starts recording a context's traffic. The tracing guid is the context's
-- tracing object, which the driver asks to start and export the HAR.
startRecordHar :: Guid -> Guid -> RecordHar -> Playwright ()
startRecordHar contextGuid tracingGuid opts = do
  path <- liftIO (makeAbsolute (recordHarPath opts))
  let
    zipped = isZipPath path
    content =
      fromMaybe
        (if zipped then RecordHarContentAttach else RecordHarContentEmbed)
        (recordHarContent opts)
    options =
      Aeson.object $
        [ "content" Aeson..= contentText content
        , "mode" Aeson..= modeText (fromMaybe RecordHarModeFull (recordHarMode opts))
        ]
          <> [ "urlGlob" Aeson..= glob | Just glob <- [recordHarUrlGlob opts] ]
          -- A zipped HAR is written by this client at export time, so the driver gets no path.
          <> [ "harPath" Aeson..= path | not zipped ]
  result <-
    sendCommand
      tracingGuid
      (MethodName "harStart")
      (Aeson.object ["options" Aeson..= options])
  harId <- resultText "harId" result
  putHarRecorder
    contextGuid
    HarRecorder
      { harRecorderId = harId
      , harRecorderPath = path
      , harRecorderTracing = tracingGuid
      }

-- | Stops the context's HAR recording, if one is running, and writes the HAR
-- to its path. Called before the context closes.
exportRecordHar :: Guid -> Playwright ()
exportRecordHar contextGuid = do
  recorder <- lookupHarRecorder contextGuid
  forM_ recorder $ \active -> do
    deleteHarRecorder contextGuid
    let path = harRecorderPath active
    result :: Aeson.Value <-
      sendCommand
        (harRecorderTracing active)
        (MethodName "harExport")
        ( Aeson.object
            [ "harId" Aeson..= harRecorderId active
            , "mode" Aeson..= ("entries" :: Text)
            ]
        )
    -- A plain HAR is written by the driver to its path; a zipped one is zipped here from the entries.
    when (isZipPath path) $ do
      entries <- resultValue "entries" result
      utilsGuid <- lookupUtilsGuid
      _ :: Aeson.Value <-
        sendCommand
          utilsGuid
          (MethodName "zip")
          ( Aeson.object
              [ "zipFile" Aeson..= path
              , "entries" Aeson..= entries
              , "mode" Aeson..= ("write" :: Text)
              , "includeSources" Aeson..= False
              , "additionalSources" Aeson..= ([] :: [Text])
              ]
          )
      pure ()

resultValue :: Text -> Aeson.Value -> Playwright Aeson.Value
resultValue key result =
  case Aeson.parseMaybe (Aeson.withObject "result" (\o -> o Aeson..: Key.fromText key)) result of
    Just value ->
      pure value
    Nothing ->
      throwIO (harFailure ("HAR result is missing " <> key))

resultText :: Text -> Aeson.Value -> Playwright Text
resultText key result = do
  value <- resultValue key result
  case Aeson.parseMaybe Aeson.parseJSON value of
    Just text ->
      pure text
    Nothing ->
      throwIO (harFailure ("HAR result field " <> key <> " is not text"))

harFailure :: Text -> PlaywrightError
harFailure message =
  ProtocolRpc
    ProtocolFailure
      { protocolFailureName = Text.pack "Error"
      , protocolFailureMessage = message
      , protocolFailureStack = Nothing
      }

-- | The playwright object's initializer, which names the @utils@ channel.
newtype PlaywrightInitializer = PlaywrightInitializer
  { playwrightUtils :: Maybe ChannelRef
  }
  deriving stock (Eq, Show)
  deriving (FromJSON) via (Autodocodec PlaywrightInitializer)

instance HasCodec PlaywrightInitializer where
  codec =
    object "PlaywrightInitializer" $
      PlaywrightInitializer <$> optionalField' "utils" .= playwrightUtils

lookupUtilsGuid :: Playwright Guid
lookupUtilsGuid = do
  playwrightGuid <- lookupPlaywrightGuid
  obj <- lookupObject playwrightGuid
  initializer <- throwDecode (remoteInitializer obj)
  case playwrightUtils initializer of
    Just ref ->
      pure (channelRefGuid ref)
    Nothing ->
      throwIO (harFailure "HAR support is unavailable on this connection")
