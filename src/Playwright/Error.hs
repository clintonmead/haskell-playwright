module Playwright.Error
  ( PlaywrightError (..)
  , ProtocolFailure (..)
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
  | RouteAlreadyHandled
  | SelectorsRegister Text
  | LocatorNotMainFrame
  deriving stock (Eq, Show)

instance Exception PlaywrightError
