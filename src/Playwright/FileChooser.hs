module Playwright.FileChooser
  ( FileChooser (..)
  , armFileChooser
  , waitForFileChooser
  , waitForFileChooserWith
  ) where

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
  ( throwIO
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
    event <- either throwIO pure (fromJsonValue params)
    pure
      FileChooser
        { fileChooserElement = elementHandleFromRef (pageFileChooserEventElement event)
        , fileChooserMultiple = pageFileChooserEventIsMultiple event
        }
