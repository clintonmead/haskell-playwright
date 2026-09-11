module Playwright.FileChooser
  ( FileChooser (..)
  , armFileChooser
  , waitForFileChooser
  ) where

import Control.Concurrent.STM
  ( atomically
  , takeTMVar
  )
import Control.Exception
  ( throwIO
  )
import Playwright.Channel
  ( Channel (channelConnection, channelGuid)
  , send
  )
import Playwright.Connection
  ( registerEventWaiter
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
  takeChooser <- armFileChooser page
  takeChooser

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
  box <- registerEventWaiter conn guid (MethodName "fileChooser")
  pure $ do
    params <- atomically (takeTMVar box)
    event <- either throwIO pure (fromJsonValue params)
    pure
      FileChooser
        { fileChooserElement = elementHandleFromRef conn (pageFileChooserEventElement event)
        , fileChooserMultiple = pageFileChooserEventIsMultiple event
        }
