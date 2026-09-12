module Playwright.Connection.Store
  ( addSelectorEngine
  , beginLocatorHandler
  , clearRouteHandlers
  , closedGuids
  , deleteLocatorHandler
  , guidsOfType
  , locatorHandlerUidsMatching
  , lookupContextBaseUrl
  , lookupContextBrowserGuid
  , lookupMainFramePage
  , lookupObject
  , lookupOwnedContextGuids
  , lookupRequestFailure
  , lookupRequestOverrides
  , lookupSelectorEngines
  , lookupTimeoutDefaults
  , markLocatorHandlersListening
  , markPageOwnsContext
  , pageOwnsContext
  , pushRouteHandler
  , putLocatorHandler
  , registerMainFramePage
  , rememberBrowserContext
  , removeRouteHandlers
  , routeHandlersFor
  , setTimeoutDefault
  , setTimeoutNavigation
  , writeContextBaseUrl
  , writeRequestOverrides
  ) where

import Control.Concurrent.STM
  ( atomically
  , modifyTVar'
  , readTVar
  , writeTVar
  )
import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( fromMaybe
  )
import Data.Set
  ( Set
  )
import Data.Set qualified as Set
import Data.Text
  ( Text
  )
import Playwright.Connection.Types
  ( Connection (connectionBrowserContexts, connectionClosedGuids, connectionContextBaseUrls, connectionContextBrowsers, connectionLocatorHandlers, connectionLocatorListening, connectionMainFramePages, connectionObjects, connectionPagesOwningContext, connectionRequestFailures, connectionRequestOverrides, connectionRouteHandlers, connectionRouteListening, connectionSelectorEngines, connectionTimeoutDefaults)
  , StoredLocatorHandler (storedLocatorHandlerFrame, storedLocatorHandlerSelector, storedLocatorHandlerTimes)
  )
import Playwright.Error
  ( PlaywrightError (MissingObject)
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Json.ChannelRef
  ( ChannelRef
  )
import Playwright.Protocol
  ( ChannelType
  , SelectorEngine
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteGuid, remoteType)
  )
import Playwright.RequestOverrides
  ( RequestOverrides
  , emptyRequestOverrides
  )
import Playwright.Timeout
  ( Timeout
  )
import Playwright.TimeoutDefaults
  ( TimeoutDefaults (timeoutDefaultsNavigation, timeoutDefaultsTimeout)
  , emptyTimeoutDefaults
  )
import Playwright.Url
  ( Url
  )
import Playwright.UrlGlob
  ( UrlGlob
  )

lookupObject :: Connection -> Guid -> IO RemoteObject
lookupObject conn guid = do
  objects <- atomically (readTVar (connectionObjects conn))
  case Map.lookup guid objects of
    Nothing ->
      throwIO (MissingObject guid)
    Just obj ->
      pure obj

guidsOfType :: Connection -> ChannelType -> IO [Guid]
guidsOfType conn ty = do
  objects <- atomically (readTVar (connectionObjects conn))
  pure
    [ remoteGuid obj
    | obj <- Map.elems objects
    , remoteType obj == ty
    ]

closedGuids :: Connection -> IO (Set Guid)
closedGuids conn =
  atomically (readTVar (connectionClosedGuids conn))

pushRouteHandler :: Connection -> Guid -> UrlGlob -> (ChannelRef -> IO Bool) -> IO Bool
pushRouteHandler conn guid glob run =
  atomically $ do
    listening <- readTVar (connectionRouteListening conn)
    let
      first = Set.notMember guid listening
    when first (modifyTVar' (connectionRouteListening conn) (Set.insert guid))
    modifyTVar'
      (connectionRouteHandlers conn)
      (Map.insertWith (++) guid [(glob, run)])
    pure first

removeRouteHandlers :: Connection -> Guid -> UrlGlob -> IO ()
removeRouteHandlers conn guid glob =
  atomically $
    modifyTVar'
      (connectionRouteHandlers conn)
      (Map.adjust (filter (\(existing, _) -> existing /= glob)) guid)

clearRouteHandlers :: Connection -> Guid -> IO ()
clearRouteHandlers conn guid =
  atomically $
    modifyTVar' (connectionRouteHandlers conn) (Map.delete guid)

routeHandlersFor :: Connection -> Guid -> IO [(UrlGlob, ChannelRef -> IO Bool)]
routeHandlersFor conn guid = do
  handlers <- atomically (readTVar (connectionRouteHandlers conn))
  pure (Map.findWithDefault [] guid handlers)

lookupRequestOverrides :: Connection -> Guid -> IO RequestOverrides
lookupRequestOverrides conn guid = do
  overrides <- atomically (readTVar (connectionRequestOverrides conn))
  pure (Map.findWithDefault emptyRequestOverrides guid overrides)

writeRequestOverrides :: Connection -> Guid -> RequestOverrides -> IO ()
writeRequestOverrides conn guid overrides =
  atomically $
    modifyTVar' (connectionRequestOverrides conn) (Map.insert guid overrides)

lookupContextBaseUrl :: Connection -> Guid -> IO (Maybe Url)
lookupContextBaseUrl conn guid =
  atomically $
    Map.lookup guid <$> readTVar (connectionContextBaseUrls conn)

writeContextBaseUrl :: Connection -> Guid -> Url -> IO ()
writeContextBaseUrl conn guid url =
  atomically $
    modifyTVar' (connectionContextBaseUrls conn) (Map.insert guid url)

rememberBrowserContext :: Connection -> Guid -> Guid -> IO ()
rememberBrowserContext conn browserGuid contextGuid =
  atomically $ do
    modifyTVar' (connectionContextBrowsers conn) (Map.insert contextGuid browserGuid)
    modifyTVar' (connectionBrowserContexts conn) $
      Map.insertWith (\new old -> old <> new) browserGuid [contextGuid]

lookupOwnedContextGuids :: Connection -> Guid -> IO [Guid]
lookupOwnedContextGuids conn browserGuid =
  atomically $
    Map.findWithDefault [] browserGuid <$> readTVar (connectionBrowserContexts conn)

lookupContextBrowserGuid :: Connection -> Guid -> IO (Maybe Guid)
lookupContextBrowserGuid conn contextGuid =
  atomically $
    Map.lookup contextGuid <$> readTVar (connectionContextBrowsers conn)

markPageOwnsContext :: Connection -> Guid -> IO ()
markPageOwnsContext conn pageGuid =
  atomically $
    modifyTVar' (connectionPagesOwningContext conn) (Set.insert pageGuid)

pageOwnsContext :: Connection -> Guid -> IO Bool
pageOwnsContext conn pageGuid =
  atomically $
    Set.member pageGuid <$> readTVar (connectionPagesOwningContext conn)

lookupSelectorEngines :: Connection -> IO [SelectorEngine]
lookupSelectorEngines conn =
  atomically (readTVar (connectionSelectorEngines conn))

addSelectorEngine :: Connection -> SelectorEngine -> IO ()
addSelectorEngine conn engine =
  atomically $
    modifyTVar' (connectionSelectorEngines conn) (++ [engine])

markLocatorHandlersListening :: Connection -> Guid -> IO Bool
markLocatorHandlersListening conn guid =
  atomically $ do
    listening <- readTVar (connectionLocatorListening conn)
    let
      first = Set.notMember guid listening
    when first (modifyTVar' (connectionLocatorListening conn) (Set.insert guid))
    pure first

putLocatorHandler :: Connection -> Guid -> Int -> StoredLocatorHandler -> IO ()
putLocatorHandler conn pageGuid uid stored =
  atomically $
    modifyTVar' (connectionLocatorHandlers conn) (Map.insertWith Map.union pageGuid (Map.singleton uid stored))

beginLocatorHandler :: Connection -> Guid -> Int -> IO (Maybe StoredLocatorHandler, Bool)
beginLocatorHandler conn pageGuid uid =
  atomically $ do
    pages <- readTVar (connectionLocatorHandlers conn)
    let
      handlers = Map.findWithDefault Map.empty pageGuid pages
    case Map.lookup uid handlers of
      Nothing ->
        pure (Nothing, False)
      Just stored ->
        case storedLocatorHandlerTimes stored of
          Just 0 ->
            pure (Nothing, True)
          Just remaining -> do
            let
              next = remaining - 1
              updated = stored {storedLocatorHandlerTimes = Just next}
            writeTVar
              (connectionLocatorHandlers conn)
              (Map.insert pageGuid (Map.insert uid updated handlers) pages)
            pure (Just stored, next == 0)
          Nothing ->
            pure (Just stored, False)

deleteLocatorHandler :: Connection -> Guid -> Int -> IO ()
deleteLocatorHandler conn pageGuid uid =
  atomically $
    modifyTVar' (connectionLocatorHandlers conn) (Map.adjust (Map.delete uid) pageGuid)

locatorHandlerUidsMatching :: Connection -> Guid -> Text -> Guid -> IO [Int]
locatorHandlerUidsMatching conn pageGuid selector frameGuid =
  atomically $ do
    pages <- readTVar (connectionLocatorHandlers conn)
    let
      handlers = Map.findWithDefault Map.empty pageGuid pages
    pure
      [ uid
      | (uid, stored) <- Map.toList handlers
      , storedLocatorHandlerSelector stored == selector
      , storedLocatorHandlerFrame stored == frameGuid
      ]

lookupTimeoutDefaults :: Connection -> Guid -> IO TimeoutDefaults
lookupTimeoutDefaults conn guid =
  atomically $
    Map.findWithDefault emptyTimeoutDefaults guid <$> readTVar (connectionTimeoutDefaults conn)

setTimeoutDefault :: Connection -> Guid -> Maybe Timeout -> IO ()
setTimeoutDefault conn guid value =
  atomically $
    modifyTVar' (connectionTimeoutDefaults conn) (Map.alter (Just . update . fromMaybe emptyTimeoutDefaults) guid)
  where
    update defaults =
      defaults {timeoutDefaultsTimeout = value}

setTimeoutNavigation :: Connection -> Guid -> Maybe Timeout -> IO ()
setTimeoutNavigation conn guid value =
  atomically $
    modifyTVar' (connectionTimeoutDefaults conn) (Map.alter (Just . update . fromMaybe emptyTimeoutDefaults) guid)
  where
    update defaults =
      defaults {timeoutDefaultsNavigation = value}

registerMainFramePage :: Connection -> Guid -> Guid -> IO ()
registerMainFramePage conn frameGuid pageGuid =
  atomically $
    modifyTVar' (connectionMainFramePages conn) (Map.insert frameGuid pageGuid)

lookupMainFramePage :: Connection -> Guid -> IO (Maybe Guid)
lookupMainFramePage conn frameGuid =
  atomically $
    Map.lookup frameGuid <$> readTVar (connectionMainFramePages conn)

lookupRequestFailure :: Connection -> Guid -> IO (Maybe Text)
lookupRequestFailure conn guid =
  atomically $
    Map.lookup guid <$> readTVar (connectionRequestFailures conn)
