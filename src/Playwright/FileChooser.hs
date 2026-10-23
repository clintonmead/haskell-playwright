module Playwright.FileChooser
  ( FileChooser (..)
  , FileChooserListener
  , armFileChooser
  , fileChoosersEqual
  , fileChooserSetFiles
  , fileChooserSetFilesPaths
  , offFileChooser
  , onFileChooser
  , waitForFileChooser
  , waitForFileChooserWith
  ) where

import Control.Monad
  ( when
  )
import Data.Aeson
  ( Value
  )
import Data.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Channel
  ( Channel (channelGuid)
  , send
  )
import Playwright.Connection
  ( registerEventHandler
  , registerEventWaiter
  , waitForWaiter
  )
import Playwright.ElementHandle
  ( ElementHandle (elementHandleChannel)
  , elementHandleFromRef
  , setInputFilesHandle
  , setInputFilesHandlePaths
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

-- | A listener registered with 'onFileChooser'. Pass it to 'offFileChooser' to stop it.
newtype FileChooserListener = FileChooserListener (IORef Bool)

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
  subscribeFileChooser page
  let
    guid = channelGuid (pageChannel page)
  waitId <- registerEventWaiter guid (MethodName "fileChooser")
  pure $ do
    params <- waitForWaiter waitId
    fileChooserFromParams page params

-- | Runs the callback for every file chooser the page opens, until 'offFileChooser'
-- is called with the returned listener. The callback runs on the event handler thread.
onFileChooser :: Page -> (FileChooser -> Playwright ()) -> Playwright FileChooserListener
onFileChooser page run = do
  subscribeFileChooser page
  active <- liftIO (newIORef True)
  registerEventHandler
    (channelGuid (pageChannel page))
    (MethodName "fileChooser")
    $ \params -> do
      isActive <- liftIO (readIORef active)
      when isActive $
        fileChooserFromParams page params >>= run
  pure (FileChooserListener active)

-- | Stops a listener registered with 'onFileChooser'. Choosers opened after this
-- call are not delivered to its callback.
offFileChooser :: FileChooserListener -> Playwright ()
offFileChooser (FileChooserListener active) =
  liftIO (writeIORef active False)

subscribeFileChooser :: Page -> Playwright ()
subscribeFileChooser page = do
  _ :: PageUpdateSubscriptionResult <-
    send
      (pageChannel page)
      (MethodName "updateSubscription")
      (mkPageUpdateSubscriptionParams True PageUpdateSubscriptionParamsEventFileChooser)
  pure ()

fileChooserFromParams :: Page -> Value -> Playwright FileChooser
fileChooserFromParams page params = do
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

-- | Sets the files the chooser picked from host paths. A single directory uploads its tree.
fileChooserSetFilesPaths :: FileChooser -> [FilePath] -> Playwright ()
fileChooserSetFilesPaths chooser paths =
  setInputFilesHandlePaths (fileChooserElement chooser) paths
