module Playwright.LocatorHandler
  ( addLocatorHandler
  , addLocatorHandlerWith
  , removeLocatorHandler
  ) where

import Control.Concurrent
  ( forkIO
  )
import Control.Monad
  ( unless
  , void
  , when
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Aeson
  ( Value
  )
import Playwright.AddLocatorHandlerOptions
  ( AddLocatorHandlerOptions (addLocatorHandlerNoWaitAfter, addLocatorHandlerTimes)
  , LocatorHandlerTimes (locatorHandlerTimesToInt)
  , defaultAddLocatorHandlerOptions
  )
import Playwright.Channel
  ( Channel (channelGuid)
  , send
  )
import Playwright.Connection
  ( StoredLocatorHandler (StoredLocatorHandler, storedLocatorHandlerFrame, storedLocatorHandlerRun, storedLocatorHandlerSelector, storedLocatorHandlerTimes)
  , beginLocatorHandler
  , deleteLocatorHandler
  , locatorHandlerUidsMatching
  , markLocatorHandlersListening
  , putLocatorHandler
  , registerEventHandler
  )
import Playwright.Error
  ( PlaywrightError (LocatorNotMainFrame)
  )
import Playwright.Frame
  ( Frame (frameChannel)
  , framesEqual
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.Locator
  ( Locator (locatorFrame, locatorSelector)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
  , askConnection
  , runPlaywrightWith
  )
import Playwright.Page
  ( Page (pageChannel, pageMainFrame)
  )
import Playwright.Protocol
  ( PageLocatorHandlerTriggeredEvent (pageLocatorHandlerTriggeredEventUid)
  , PageRegisterLocatorHandlerParams (pageRegisterLocatorHandlerParamsNoWaitAfter)
  , PageRegisterLocatorHandlerResult (pageRegisterLocatorHandlerResultUid)
  , PageResolveLocatorHandlerNoReplyParams (pageResolveLocatorHandlerNoReplyParamsRemove)
  , PageResolveLocatorHandlerNoReplyResult
  , PageUnregisterLocatorHandlerResult
  , mkPageRegisterLocatorHandlerParams
  , mkPageResolveLocatorHandlerNoReplyParams
  , mkPageUnregisterLocatorHandlerParams
  )
import Playwright.Selector
  ( selectorToText
  )
import UnliftIO.Exception
  ( SomeException
  , catch
  , throwIO
  )

addLocatorHandler :: Page -> Locator -> (Locator -> Playwright ()) -> Playwright ()
addLocatorHandler page loc handler =
  addLocatorHandlerWith page loc handler defaultAddLocatorHandlerOptions

addLocatorHandlerWith :: Page -> Locator -> (Locator -> Playwright ()) -> AddLocatorHandlerOptions -> Playwright ()
addLocatorHandlerWith page loc handler opts = do
  unless (framesEqual (locatorFrame loc) (pageMainFrame page)) $
    throwIO LocatorNotMainFrame
  case fmap locatorHandlerTimesToInt (addLocatorHandlerTimes opts) of
    Just 0 ->
      pure ()
    times -> do
      conn <- askConnection
      let
        pageGuid = channelGuid (pageChannel page)
      first <- markLocatorHandlersListening pageGuid
      when first $
        registerEventHandler
          pageGuid
          (MethodName "locatorHandlerTriggered")
          (dispatchLocatorHandler page)
      result <-
        send
          (pageChannel page)
          (MethodName "registerLocatorHandler")
          (mkPageRegisterLocatorHandlerParams (locatorSelector loc))
            { pageRegisterLocatorHandlerParamsNoWaitAfter = addLocatorHandlerNoWaitAfter opts
            }
      putLocatorHandler
        pageGuid
        (pageRegisterLocatorHandlerResultUid result)
        StoredLocatorHandler
          { storedLocatorHandlerTimes = times
          , storedLocatorHandlerSelector = selectorToText (locatorSelector loc)
          , storedLocatorHandlerFrame = channelGuid (frameChannel (locatorFrame loc))
          , storedLocatorHandlerRun = runPlaywrightWith conn (handler loc)
          }

removeLocatorHandler :: Page -> Locator -> Playwright ()
removeLocatorHandler page loc = do
  let
    pageGuid = channelGuid (pageChannel page)
  uids <-
    locatorHandlerUidsMatching
      pageGuid
      (selectorToText (locatorSelector loc))
      (channelGuid (frameChannel (locatorFrame loc)))
  mapM_ (unregisterOne page) uids

unregisterOne :: Page -> Int -> Playwright ()
unregisterOne page uid = do
  deleteLocatorHandler (channelGuid (pageChannel page)) uid
  _ :: PageUnregisterLocatorHandlerResult <-
    send
      (pageChannel page)
      (MethodName "unregisterLocatorHandler")
      (mkPageUnregisterLocatorHandlerParams uid)
  pure ()

dispatchLocatorHandler :: Page -> Value -> Playwright ()
dispatchLocatorHandler page params = do
  conn <- askConnection
  liftIO $
    void $
      forkIO $
        runPlaywrightWith conn $ do
          event <- either throwIO pure (fromJsonValue params :: Either PlaywrightError PageLocatorHandlerTriggeredEvent)
          let
            uid = pageLocatorHandlerTriggeredEventUid event
            pageGuid = channelGuid (pageChannel page)
          (mhandler, remove) <- beginLocatorHandler pageGuid uid
          case mhandler of
            Nothing ->
              pure ()
            Just stored ->
              liftIO (storedLocatorHandlerRun stored) `catch` (\(_ :: SomeException) -> pure ())
          when remove (deleteLocatorHandler pageGuid uid)
          resolveLocatorHandler page uid remove

resolveLocatorHandler :: Page -> Int -> Bool -> Playwright ()
resolveLocatorHandler page uid remove =
  ( do
      _ :: PageResolveLocatorHandlerNoReplyResult <-
        send
          (pageChannel page)
          (MethodName "resolveLocatorHandlerNoReply")
          (mkPageResolveLocatorHandlerNoReplyParams uid)
            { pageResolveLocatorHandlerNoReplyParamsRemove = Just remove
            }
      pure ()
  )
    `catch` (\(_ :: PlaywrightError) -> pure ())
