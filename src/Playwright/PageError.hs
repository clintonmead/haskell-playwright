module Playwright.PageError
  ( PageError (..)
  , clearPageErrors
  , pageErrorFromSerialized
  , pageErrors
  , waitForPageError
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  , pack
  )
import Playwright.Channel
  ( Channel (channelConnection, channelGuid)
  , send
  )
import Playwright.Connection
  ( waitForChannelEvent
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

waitForPageError :: Page -> IO PageError
waitForPageError page = do
  let
    conn = channelConnection (pageContextChannel page)
    contextGuid = channelGuid (pageContextChannel page)
  params <- waitForChannelEvent conn contextGuid (MethodName "pageError")
  event <- either throwIO pure (fromJsonValue params)
  if channelRefGuid (browserContextPageErrorEventPage event) == channelGuid (pageChannel page)
    then pure (pageErrorFromSerialized (browserContextPageErrorEventError event))
    else waitForPageError page

pageErrors :: Page -> IO [PageError]
pageErrors page = do
  result <-
    send
      (pageChannel page)
      (MethodName "pageErrors")
      mkPagePageErrorsParams
  pure (fmap pageErrorFromSerialized (pagePageErrorsResultErrors result))

clearPageErrors :: Page -> IO ()
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
