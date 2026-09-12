module Playwright.FileChooser
  ( FileChooser (..)
  , armFileChooser
  , waitForFileChooser
  , waitForFileChooserWith
  ) where

import Control.Exception
  ( throwIO
  )
import Playwright.Channel
  ( Channel (channelConnection, channelGuid)
  , send
  )
import Playwright.Connection
  ( registerEventWaiter
  , waitForWaiter
  )
import Playwright.ElementHandle
  ( ElementHandle
  , elementHandleFromRef
  )
import Playwright.Json.Decode
  ( fromJsonValue
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
  }

waitForFileChooser :: Page -> IO FileChooser
waitForFileChooser page = do
  timeout <- pageActionTimeout page
  waitForFileChooserWith page timeout

waitForFileChooserWith :: Page -> Timeout -> IO FileChooser
waitForFileChooserWith page timeout = do
  takeChooser <- armFileChooser page
  waitWithTimeout timeout takeChooser

armFileChooser :: Page -> IO (IO FileChooser)
armFileChooser page = do
  _ :: PageUpdateSubscriptionResult <-
    send
      (pageChannel page)
      (MethodName "updateSubscription")
      (mkPageUpdateSubscriptionParams True PageUpdateSubscriptionParamsEventFileChooser)
  let
    conn = channelConnection (pageChannel page)
    guid = channelGuid (pageChannel page)
  waitId <- registerEventWaiter conn guid (MethodName "fileChooser")
  pure $ do
    params <- waitForWaiter conn waitId
    event <- either throwIO pure (fromJsonValue params)
    pure
      FileChooser
        { fileChooserElement = elementHandleFromRef conn (pageFileChooserEventElement event)
        , fileChooserMultiple = pageFileChooserEventIsMultiple event
        }
