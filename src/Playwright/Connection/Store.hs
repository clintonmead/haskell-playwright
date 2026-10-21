module Playwright.Connection.Store
  ( addSelectorEngine
  , beginLocatorHandler
  , clearRouteHandlers
  , closedGuids
  , contextHasOwnerPage
  , deleteLocatorHandler
  , guidsOfType
  , locatorHandlerUidsMatching
  , lookupBrowserTypeGuid
  , lookupContextBaseUrl
  , lookupContextBrowserGuid
  , lookupMainFramePage
  , lookupObject
  , lookupOwnedContextGuids
  , lookupRequestEndTiming
  , lookupRequestFailure
  , lookupRequestOverrides
  , lookupSelectorEngines
  , lookupTimeoutDefaults
  , markContextHasOwnerPage
  , markLocatorHandlersListening
  , markPageOwnsContext
  , pageOwnsContext
  , pushRouteHandler
  , putLocatorHandler
  , registerMainFramePage
  , rememberBrowserContext
  , rememberBrowserType
  , removeRouteHandlers
  , routeHandlersFor
  , setTimeoutDefault
  , setTimeoutNavigation
  , writeContextBaseUrl
  , writeRequestOverrides
  ) where

import Control.Concurrent.STM
  ( atomically
  )
import Control.Exception
  ( throwIO
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( fromMaybe
  )
import Data.Set
  ( Set
  )
import Data.Set qualified as Set
import Data.Scientific
  ( Scientific
  )
import Data.Text
  ( Text
  )
import Playwright.Connection.Types
  ( Connection
  , modifySession
  , readSession
  , stateSession
  , withSession
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
import Playwright.Session
  ( Session (sessionBrowserContexts, sessionBrowserTypes, sessionClosedGuids, sessionContextBaseUrls, sessionContextBrowsers, sessionContextsWithOwnerPage, sessionLocatorHandlers, sessionLocatorListening, sessionMainFramePages, sessionObjects, sessionPagesOwningContext, sessionRequestEndTiming, sessionRequestFailures, sessionRequestOverrides, sessionRouteHandlers, sessionRouteListening, sessionSelectorEngines, sessionTimeoutDefaults)
  )
import Playwright.StoredLocatorHandler
  ( StoredLocatorHandler (storedLocatorHandlerFrame, storedLocatorHandlerSelector, storedLocatorHandlerTimes)
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
import Playwright.UrlMatch
  ( UrlMatch
  )

lookupObject :: Connection -> Guid -> IO RemoteObject
lookupObject conn guid = do
  session <- atomically (readSession conn)
  case Map.lookup guid (sessionObjects session) of
    Nothing ->
      throwIO (MissingObject guid)
    Just obj ->
      pure obj

guidsOfType :: Connection -> ChannelType -> IO [Guid]
guidsOfType conn ty = do
  session <- atomically (readSession conn)
  pure
    [ remoteGuid obj
    | obj <- Map.elems (sessionObjects session)
    , remoteType obj == ty
    ]

closedGuids :: Connection -> IO (Set Guid)
closedGuids conn =
  atomically (sessionClosedGuids <$> readSession conn)

pushRouteHandler :: Connection -> Guid -> UrlMatch -> (ChannelRef -> IO Bool) -> IO Bool
pushRouteHandler conn guid glob run =
  atomically $
    stateSession conn $ \session ->
      let
        first = Set.notMember guid (sessionRouteListening session)
        listening =
          if first
            then Set.insert guid (sessionRouteListening session)
            else sessionRouteListening session
      in
        ( first
        , session
            { sessionRouteListening = listening
            , sessionRouteHandlers =
                Map.insertWith (++) guid [(glob, run)] (sessionRouteHandlers session)
            }
        )

removeRouteHandlers :: Connection -> Guid -> UrlMatch -> IO ()
removeRouteHandlers conn guid glob =
  atomically $
    modifySession conn $ \session ->
      session
        { sessionRouteHandlers =
            Map.adjust (filter (\(existing, _) -> existing /= glob)) guid (sessionRouteHandlers session)
        }

clearRouteHandlers :: Connection -> Guid -> IO ()
clearRouteHandlers conn guid =
  atomically $
    modifySession conn $ \session ->
      session {sessionRouteHandlers = Map.delete guid (sessionRouteHandlers session)}

routeHandlersFor :: Connection -> Guid -> IO [(UrlMatch, ChannelRef -> IO Bool)]
routeHandlersFor conn guid = do
  session <- atomically (readSession conn)
  pure (Map.findWithDefault [] guid (sessionRouteHandlers session))

lookupRequestOverrides :: Connection -> Guid -> IO RequestOverrides
lookupRequestOverrides conn guid = do
  session <- atomically (readSession conn)
  pure (Map.findWithDefault emptyRequestOverrides guid (sessionRequestOverrides session))

writeRequestOverrides :: Connection -> Guid -> RequestOverrides -> IO ()
writeRequestOverrides conn guid overrides =
  atomically $
    modifySession conn $ \session ->
      session {sessionRequestOverrides = Map.insert guid overrides (sessionRequestOverrides session)}

lookupContextBaseUrl :: Connection -> Guid -> IO (Maybe Url)
lookupContextBaseUrl conn guid =
  atomically $
    Map.lookup guid . sessionContextBaseUrls <$> readSession conn

writeContextBaseUrl :: Connection -> Guid -> Url -> IO ()
writeContextBaseUrl conn guid url =
  atomically $
    modifySession conn $ \session ->
      session {sessionContextBaseUrls = Map.insert guid url (sessionContextBaseUrls session)}

rememberBrowserContext :: Connection -> Guid -> Guid -> IO ()
rememberBrowserContext conn browserGuid contextGuid =
  atomically $
    modifySession conn $ \session ->
      session
        { sessionContextBrowsers = Map.insert contextGuid browserGuid (sessionContextBrowsers session)
        , sessionBrowserContexts =
            Map.insertWith (\new old -> old <> new) browserGuid [contextGuid] (sessionBrowserContexts session)
        }

lookupOwnedContextGuids :: Connection -> Guid -> IO [Guid]
lookupOwnedContextGuids conn browserGuid =
  atomically $
    Map.findWithDefault [] browserGuid . sessionBrowserContexts <$> readSession conn

lookupContextBrowserGuid :: Connection -> Guid -> IO (Maybe Guid)
lookupContextBrowserGuid conn contextGuid =
  atomically $
    Map.lookup contextGuid . sessionContextBrowsers <$> readSession conn

markPageOwnsContext :: Connection -> Guid -> IO ()
markPageOwnsContext conn pageGuid =
  atomically $
    modifySession conn $ \session ->
      session {sessionPagesOwningContext = Set.insert pageGuid (sessionPagesOwningContext session)}

pageOwnsContext :: Connection -> Guid -> IO Bool
pageOwnsContext conn pageGuid =
  atomically $
    Set.member pageGuid . sessionPagesOwningContext <$> readSession conn

markContextHasOwnerPage :: Connection -> Guid -> IO ()
markContextHasOwnerPage conn contextGuid =
  atomically $
    modifySession conn $ \session ->
      session {sessionContextsWithOwnerPage = Set.insert contextGuid (sessionContextsWithOwnerPage session)}

contextHasOwnerPage :: Connection -> Guid -> IO Bool
contextHasOwnerPage conn contextGuid =
  atomically $
    Set.member contextGuid . sessionContextsWithOwnerPage <$> readSession conn

rememberBrowserType :: Connection -> Guid -> Guid -> IO ()
rememberBrowserType conn browserGuid browserTypeGuid =
  atomically $
    modifySession conn $ \session ->
      session {sessionBrowserTypes = Map.insert browserGuid browserTypeGuid (sessionBrowserTypes session)}

lookupBrowserTypeGuid :: Connection -> Guid -> IO (Maybe Guid)
lookupBrowserTypeGuid conn browserGuid =
  atomically $
    Map.lookup browserGuid . sessionBrowserTypes <$> readSession conn

lookupSelectorEngines :: Connection -> IO [SelectorEngine]
lookupSelectorEngines conn =
  atomically (sessionSelectorEngines <$> readSession conn)

addSelectorEngine :: Connection -> SelectorEngine -> IO ()
addSelectorEngine conn engine =
  atomically $
    modifySession conn $ \session ->
      session {sessionSelectorEngines = sessionSelectorEngines session ++ [engine]}

markLocatorHandlersListening :: Connection -> Guid -> IO Bool
markLocatorHandlersListening conn guid =
  atomically $
    stateSession conn $ \session ->
      let
        first = Set.notMember guid (sessionLocatorListening session)
      in
        ( first
        , if first
            then session {sessionLocatorListening = Set.insert guid (sessionLocatorListening session)}
            else session
        )

putLocatorHandler :: Connection -> Guid -> Int -> StoredLocatorHandler -> IO ()
putLocatorHandler conn pageGuid uid stored =
  atomically $
    modifySession conn $ \session ->
      session
        { sessionLocatorHandlers =
            Map.insertWith Map.union pageGuid (Map.singleton uid stored) (sessionLocatorHandlers session)
        }

beginLocatorHandler :: Connection -> Guid -> Int -> IO (Maybe StoredLocatorHandler, Bool)
beginLocatorHandler conn pageGuid uid =
  atomically $
    withSession conn $ \session ->
      let
        handlers = Map.findWithDefault Map.empty pageGuid (sessionLocatorHandlers session)
      in
        case Map.lookup uid handlers of
          Nothing ->
            pure ((Nothing, False), session)
          Just stored ->
            case storedLocatorHandlerTimes stored of
              Just 0 ->
                pure ((Nothing, True), session)
              Just remaining ->
                let
                  next = remaining - 1
                  updated = stored {storedLocatorHandlerTimes = Just next}
                in
                  pure
                    ( (Just stored, next == 0)
                    , session
                        { sessionLocatorHandlers =
                            Map.insert pageGuid (Map.insert uid updated handlers) (sessionLocatorHandlers session)
                        }
                    )
              Nothing ->
                pure ((Just stored, False), session)

deleteLocatorHandler :: Connection -> Guid -> Int -> IO ()
deleteLocatorHandler conn pageGuid uid =
  atomically $
    modifySession conn $ \session ->
      session {sessionLocatorHandlers = Map.adjust (Map.delete uid) pageGuid (sessionLocatorHandlers session)}

locatorHandlerUidsMatching :: Connection -> Guid -> Text -> Guid -> IO [Int]
locatorHandlerUidsMatching conn pageGuid selector frameGuid =
  atomically $ do
    session <- readSession conn
    let
      handlers = Map.findWithDefault Map.empty pageGuid (sessionLocatorHandlers session)
    pure
      [ uid
      | (uid, stored) <- Map.toList handlers
      , storedLocatorHandlerSelector stored == selector
      , storedLocatorHandlerFrame stored == frameGuid
      ]

lookupTimeoutDefaults :: Connection -> Guid -> IO TimeoutDefaults
lookupTimeoutDefaults conn guid =
  atomically $
    Map.findWithDefault emptyTimeoutDefaults guid . sessionTimeoutDefaults <$> readSession conn

setTimeoutDefault :: Connection -> Guid -> Maybe Timeout -> IO ()
setTimeoutDefault conn guid value =
  atomically $
    modifySession conn $ \session ->
      session
        { sessionTimeoutDefaults =
            Map.alter (Just . update . fromMaybe emptyTimeoutDefaults) guid (sessionTimeoutDefaults session)
        }
  where
    update defaults =
      defaults {timeoutDefaultsTimeout = value}

setTimeoutNavigation :: Connection -> Guid -> Maybe Timeout -> IO ()
setTimeoutNavigation conn guid value =
  atomically $
    modifySession conn $ \session ->
      session
        { sessionTimeoutDefaults =
            Map.alter (Just . update . fromMaybe emptyTimeoutDefaults) guid (sessionTimeoutDefaults session)
        }
  where
    update defaults =
      defaults {timeoutDefaultsNavigation = value}

registerMainFramePage :: Connection -> Guid -> Guid -> IO ()
registerMainFramePage conn frameGuid pageGuid =
  atomically $
    modifySession conn $ \session ->
      session {sessionMainFramePages = Map.insert frameGuid pageGuid (sessionMainFramePages session)}

lookupMainFramePage :: Connection -> Guid -> IO (Maybe Guid)
lookupMainFramePage conn frameGuid =
  atomically $
    Map.lookup frameGuid . sessionMainFramePages <$> readSession conn

lookupRequestFailure :: Connection -> Guid -> IO (Maybe Text)
lookupRequestFailure conn guid =
  atomically $
    Map.lookup guid . sessionRequestFailures <$> readSession conn

lookupRequestEndTiming :: Connection -> Guid -> IO (Maybe Scientific)
lookupRequestEndTiming conn guid =
  atomically $
    Map.lookup guid . sessionRequestEndTiming <$> readSession conn
