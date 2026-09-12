module Playwright.LocatorHandler
  ( addLocatorHandler
  , addLocatorHandlerWith
  , removeLocatorHandler
  ) where

import Control.Concurrent
  ( forkIO
  )
import Control.Exception
  ( SomeException
  , catch
  , throwIO
  )
import Control.Monad
  ( unless
  , void
  , when
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
  ( Channel (channelConnection, channelGuid)
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

addLocatorHandler :: Page -> Locator -> (Locator -> IO ()) -> IO ()
addLocatorHandler page loc handler =
  addLocatorHandlerWith page loc handler defaultAddLocatorHandlerOptions

addLocatorHandlerWith :: Page -> Locator -> (Locator -> IO ()) -> AddLocatorHandlerOptions -> IO ()
addLocatorHandlerWith page loc handler opts = do
  unless (framesEqual (locatorFrame loc) (pageMainFrame page)) $
    throwIO LocatorNotMainFrame
  case fmap locatorHandlerTimesToInt (addLocatorHandlerTimes opts) of
    Just 0 ->
      pure ()
    times -> do
      let
        conn = channelConnection (pageChannel page)
        pageGuid = channelGuid (pageChannel page)
      first <- markLocatorHandlersListening conn pageGuid
      when first $
        registerEventHandler
          conn
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
        conn
        pageGuid
        (pageRegisterLocatorHandlerResultUid result)
        StoredLocatorHandler
          { storedLocatorHandlerTimes = times
          , storedLocatorHandlerSelector = selectorToText (locatorSelector loc)
          , storedLocatorHandlerFrame = channelGuid (frameChannel (locatorFrame loc))
          , storedLocatorHandlerRun = handler loc
          }

removeLocatorHandler :: Page -> Locator -> IO ()
removeLocatorHandler page loc = do
  let
    conn = channelConnection (pageChannel page)
    pageGuid = channelGuid (pageChannel page)
  uids <-
    locatorHandlerUidsMatching
      conn
      pageGuid
      (selectorToText (locatorSelector loc))
      (channelGuid (frameChannel (locatorFrame loc)))
  mapM_ (unregisterOne page) uids

unregisterOne :: Page -> Int -> IO ()
unregisterOne page uid = do
  deleteLocatorHandler
    (channelConnection (pageChannel page))
    (channelGuid (pageChannel page))
    uid
  _ :: PageUnregisterLocatorHandlerResult <-
    send
      (pageChannel page)
      (MethodName "unregisterLocatorHandler")
      (mkPageUnregisterLocatorHandlerParams uid)
  pure ()

dispatchLocatorHandler :: Page -> Value -> IO ()
dispatchLocatorHandler page params =
  void $
    forkIO $ do
      event <- either throwIO pure (fromJsonValue params :: Either PlaywrightError PageLocatorHandlerTriggeredEvent)
      let
        uid = pageLocatorHandlerTriggeredEventUid event
        conn = channelConnection (pageChannel page)
        pageGuid = channelGuid (pageChannel page)
      (mhandler, remove) <- beginLocatorHandler conn pageGuid uid
      case mhandler of
        Nothing ->
          pure ()
        Just stored ->
          storedLocatorHandlerRun stored `catch` (\(_ :: SomeException) -> pure ())
      when remove (deleteLocatorHandler conn pageGuid uid)
      resolveLocatorHandler page uid remove

resolveLocatorHandler :: Page -> Int -> Bool -> IO ()
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
