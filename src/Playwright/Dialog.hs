module Playwright.Dialog
  ( Dialog (..)
  , acceptDialog
  , acceptDialogWith
  , dialogFromRef
  , dismissDialog
  ) where

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( Channel (Channel)
  , DialogTag
  , send
  )
import Playwright.Connection
  ( lookupObject
  )
import Playwright.DialogType
  ( DialogType
  , dialogTypeFromText
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
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
import Playwright.Protocol
  ( DialogAcceptParams (dialogAcceptParamsPromptText)
  , DialogAcceptResult
  , DialogDismissResult
  , DialogInitializer (dialogInitializerDefaultValue, dialogInitializerMessage, dialogInitializerType)
  , mkDialogAcceptParams
  , mkDialogDismissParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )

data Dialog = Dialog
  { dialogChannel :: Channel DialogTag
  , dialogType :: DialogType
  , dialogMessage :: Text
  , dialogDefaultValue :: Text
  }

dialogFromRef :: ChannelRef -> Playwright Dialog
dialogFromRef ref = do
  obj <- lookupObject (channelRefGuid ref)
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  kind <-
    case dialogTypeFromText (dialogInitializerType initializer) of
      Just parsed ->
        pure parsed
      Nothing ->
        throwIO (DecodeFailure ("unknown dialog type: " <> dialogInitializerType initializer))
  pure
    Dialog
      { dialogChannel = Channel (channelRefGuid ref)
      , dialogType = kind
      , dialogMessage = dialogInitializerMessage initializer
      , dialogDefaultValue = dialogInitializerDefaultValue initializer
      }

acceptDialog :: Dialog -> Playwright ()
acceptDialog dialog =
  acceptDialogWith dialog Nothing

acceptDialogWith :: Dialog -> Maybe Text -> Playwright ()
acceptDialogWith dialog promptText = do
  _ :: DialogAcceptResult <-
    send
      (dialogChannel dialog)
      (MethodName "accept")
      mkDialogAcceptParams {dialogAcceptParamsPromptText = promptText}
  pure ()

dismissDialog :: Dialog -> Playwright ()
dismissDialog dialog = do
  _ :: DialogDismissResult <-
    send (dialogChannel dialog) (MethodName "dismiss") mkDialogDismissParams
  pure ()
