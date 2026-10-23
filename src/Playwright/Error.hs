module Playwright.Error
  ( PlaywrightError (..)
  , ProtocolFailure (..)
  , ignoreTargetClosed
  , targetClosedError
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , object
  , optionalField'
  , requiredField'
  , (.=)
  )
import Control.Exception
  ( Exception
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Text
  ( Text
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.RequestId
  ( RequestId
  )
import Playwright.SerializedValue
  ( SerializedValue
  )
import Playwright.Timeout
  ( Timeout
  )
import Playwright.Transport.Frame
  ( FrameError
  )
import UnliftIO
  ( MonadUnliftIO
  )
import UnliftIO.Exception
  ( throwIO
  , try
  )

data ProtocolFailure = ProtocolFailure
  { protocolFailureName :: Text
  , protocolFailureMessage :: Text
  , protocolFailureStack :: Maybe Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec ProtocolFailure)

instance HasCodec ProtocolFailure where
  codec =
    object "ProtocolFailure" $
      ProtocolFailure
        <$> requiredField' "name" .= protocolFailureName
        <*> requiredField' "message" .= protocolFailureMessage
        <*> optionalField' "stack" .= protocolFailureStack

data PlaywrightError
  = DriverEnvMissing Text
  | DriverNotFound FilePath
  | ConnectionClosed
  | ProtocolRpc ProtocolFailure
  | DecodeFailure Text
  | MissingObject Guid
  | UnexpectedResponse RequestId
  | FrameCodec FrameError
  | BrowserPathMissing
  | EvaluateUnexpected SerializedValue
  | WaitTimeout Timeout
  | -- | A wait timed out. The text is the call log line naming what was waited for.
    WaitTimeoutLog Timeout Text
  | RouteAlreadyHandled
  | SelectorsRegister Text
  | LocatorNotMainFrame
  | SocketClosed
  | SocketError
  deriving stock (Eq, Show)

instance Exception PlaywrightError

targetClosedError :: PlaywrightError
targetClosedError =
  ProtocolRpc
    ProtocolFailure
      { protocolFailureName = "TargetClosedError"
      , protocolFailureMessage = "Target page, context or browser has been closed"
      , protocolFailureStack = Nothing
      }

ignoreTargetClosed :: (MonadUnliftIO m) => m a -> m ()
ignoreTargetClosed action = do
  result <- try action
  case result of
    Left (ProtocolRpc failure)
      | protocolFailureName failure == "TargetClosedError" ->
          pure ()
    Left err ->
      throwIO err
    Right _ ->
      pure ()
