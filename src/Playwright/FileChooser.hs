module Playwright.FileChooser
  ( FileChooser (..)
  , armFileChooser
  , fileChoosersEqual
  , fileChooserSetFiles
  , waitForFileChooser
  , waitForFileChooserWith
  ) where

import Playwright.Monad
  ( Playwright
  )
import Playwright.Channel
  ( Channel (channelGuid)
  , send
  )
import Playwright.Connection
  ( registerEventWaiter
  , waitForWaiter
  )
import Playwright.ElementHandle
  ( ElementHandle (elementHandleChannel)
  , elementHandleFromRef
  , setInputFilesHandle
  )
import Playwright.InputFile
  ( InputFile
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageChannel)
  , pageActionTimeout
  )
import Playwright.RaceTimeout
  ( waitWithTimeout
  )
import Playwright.Timeout
  ( Timeout
  )
import Playwright.Protocol
  ( PageFileChooserEvent (pageFileChooserEventElement, pageFileChooserEventIsMultiple)
  , PageUpdateSubscriptionParamsEvent (PageUpdateSubscriptionParamsEventFileChooser)
  , PageUpdateSubscriptionResult
  , mkPageUpdateSubscriptionParams
  )

data FileChooser = FileChooser
  { fileChooserElement :: ElementHandle
  , fileChooserMultiple :: Bool
  , fileChooserPage :: Page
  }

waitForFileChooser :: Page -> Playwright FileChooser
waitForFileChooser page = do
  timeout <- pageActionTimeout page
  waitForFileChooserWith page timeout

waitForFileChooserWith :: Page -> Timeout -> Playwright FileChooser
waitForFileChooserWith page timeout = do
  takeChooser <- armFileChooser page
  waitWithTimeout timeout takeChooser

armFileChooser :: Page -> Playwright (Playwright FileChooser)
armFileChooser page = do
  _ :: PageUpdateSubscriptionResult <-
    send
      (pageChannel page)
      (MethodName "updateSubscription")
      (mkPageUpdateSubscriptionParams True PageUpdateSubscriptionParamsEventFileChooser)
  let
    guid = channelGuid (pageChannel page)
  waitId <- registerEventWaiter guid (MethodName "fileChooser")
  pure $ do
    params <- waitForWaiter waitId
    event <- throwDecode params
    pure
      FileChooser
        { fileChooserElement = elementHandleFromRef (pageFileChooserEventElement event)
        , fileChooserMultiple = pageFileChooserEventIsMultiple event
        , fileChooserPage = page
        }

-- | True when both choosers wrap the same input element, as two arms of one event do.
fileChoosersEqual :: FileChooser -> FileChooser -> Bool
fileChoosersEqual left right =
  channelGuid (elementHandleChannel (fileChooserElement left))
    == channelGuid (elementHandleChannel (fileChooserElement right))

-- | Sets the files the chooser picked. This is setInputFiles on the input.
fileChooserSetFiles :: FileChooser -> [InputFile] -> Playwright ()
fileChooserSetFiles chooser files =
  setInputFilesHandle (fileChooserElement chooser) files
