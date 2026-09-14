module Playwright.Dialog
  ( Dialog (..)
  , acceptDialog
  , acceptDialogWith
  , dialogFromRef
  , dialogsEqual
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
  ( BrowserContextTag
  , Channel (Channel, channelGuid)
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
import Playwright.Page
  ( Page
  , pageFromRef
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( DialogAcceptParams (dialogAcceptParamsPromptText)
  , DialogAcceptResult
  , DialogDismissResult
  , DialogInitializer (dialogInitializerDefaultValue, dialogInitializerMessage, dialogInitializerPage, dialogInitializerType)
  , mkDialogAcceptParams
  , mkDialogDismissParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer, remoteParent)
  )

data Dialog = Dialog
  { dialogChannel :: Channel DialogTag
  , dialogType :: DialogType
  , dialogMessage :: Text
  , dialogDefaultValue :: Text
  , dialogPage :: Maybe Page
  }

dialogFromRef :: ChannelRef -> Playwright Dialog
dialogFromRef ref = do
  obj <- lookupObject (channelRefGuid ref)
  initializer <- throwDecode (remoteInitializer obj)
  kind <-
    case dialogTypeFromText (dialogInitializerType initializer) of
      Just parsed ->
        pure parsed
      Nothing ->
        throwIO (DecodeFailure ("unknown dialog type: " <> dialogInitializerType initializer))
  matchedPage <-
    case dialogInitializerPage initializer of
      Nothing ->
        pure Nothing
      Just pageRef ->
        Just <$> pageFromRef (contextChannel obj) pageRef
  pure
    Dialog
      { dialogChannel = Channel (channelRefGuid ref)
      , dialogType = kind
      , dialogMessage = dialogInitializerMessage initializer
      , dialogDefaultValue = dialogInitializerDefaultValue initializer
      , dialogPage = matchedPage
      }

contextChannel :: RemoteObject -> Channel BrowserContextTag
contextChannel obj =
  Channel (remoteParent obj)

dialogsEqual :: Dialog -> Dialog -> Bool
dialogsEqual left right =
  channelGuid (dialogChannel left) == channelGuid (dialogChannel right)

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
