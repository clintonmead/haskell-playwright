module Playwright.PageError
  ( PageError (..)
  , clearPageErrors
  , pageErrorFromSerialized
  , pageErrors
  , waitForPageError
  ) where

import Playwright.Monad
  ( Playwright
  )
import Data.Text
  ( Text
  , pack
  )
import Playwright.Channel
  ( Channel (channelGuid)
  , send
  )
import Playwright.Connection
  ( waitForChannelEvent
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
  ( Page (pageChannel, pageContextChannel)
  )
import Playwright.Protocol
  ( BrowserContextPageErrorEvent (browserContextPageErrorEventError, browserContextPageErrorEventPage)
  , PageClearPageErrorsResult
  , PagePageErrorsResult (pagePageErrorsResultErrors)
  , mkPageClearPageErrorsParams
  , mkPagePageErrorsParams
  )
import Playwright.SerializedError
  ( SerializedError (serializedErrorError, serializedErrorValue)
  )
import Playwright.SerializedErrorInfo
  ( SerializedErrorInfo (serializedErrorInfoMessage, serializedErrorInfoName, serializedErrorInfoStack)
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadBool, PayloadNumber, PayloadSpecial, PayloadString)
  , SerializedValue (SerializedNode)
  )
import Playwright.SpecialJsValue
  ( SpecialJsValue (SpecialNull, SpecialUndefined)
  )

data PageError = PageError
  { pageErrorName :: Text
  , pageErrorMessage :: Text
  , pageErrorStack :: Maybe Text
  }
  deriving stock (Eq, Show)

waitForPageError :: Page -> Playwright PageError
waitForPageError page = do
  let
    contextGuid = channelGuid (pageContextChannel page)
  params <- waitForChannelEvent contextGuid (MethodName "pageError")
  event <- throwDecode params
  if channelRefGuid (browserContextPageErrorEventPage event) == channelGuid (pageChannel page)
    then pure (pageErrorFromSerialized (browserContextPageErrorEventError event))
    else waitForPageError page

pageErrors :: Page -> Playwright [PageError]
pageErrors page = do
  result <-
    send
      (pageChannel page)
      (MethodName "pageErrors")
      mkPagePageErrorsParams
  pure (fmap pageErrorFromSerialized (pagePageErrorsResultErrors result))

clearPageErrors :: Page -> Playwright ()
clearPageErrors page = do
  _ :: PageClearPageErrorsResult <-
    send (pageChannel page) (MethodName "clearPageErrors") mkPageClearPageErrorsParams
  pure ()

pageErrorFromSerialized :: SerializedError -> PageError
pageErrorFromSerialized err =
  case serializedErrorError err of
    Just info ->
      PageError
        { pageErrorName = serializedErrorInfoName info
        , pageErrorMessage = serializedErrorInfoMessage info
        , pageErrorStack = serializedErrorInfoStack info
        }
    Nothing ->
      PageError
        { pageErrorName = "Error"
        , pageErrorMessage = maybe "Error" serializedMessage (serializedErrorValue err)
        , pageErrorStack = Nothing
        }

serializedMessage :: SerializedValue -> Text
serializedMessage value =
  case value of
    SerializedNode (PayloadString text) _ ->
      text
    SerializedNode (PayloadNumber number) _ ->
      pack (show number)
    SerializedNode (PayloadBool True) _ ->
      "true"
    SerializedNode (PayloadBool False) _ ->
      "false"
    SerializedNode (PayloadSpecial SpecialNull) _ ->
      "null"
    SerializedNode (PayloadSpecial SpecialUndefined) _ ->
      "undefined"
    _ ->
      "Error"
