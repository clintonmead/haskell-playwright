module Playwright.Worker
  ( Worker (..)
  , evaluateOnWorker
  , evaluateOnWorkerNumber
  , evaluateOnWorkerText
  , onServiceWorker
  , onWorkerClose
  , serviceWorkers
  , waitForServiceWorker
  , waitForWorker
  , armWorkerConsole
  , waitForWorkerConsole
  , workerFromRef
  , workersEqual
  , workerUrl
  , workers
  ) where

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
  ( throwIO
  )
import Data.Aeson
  ( Result (Error, Success)
  , Value
  , fromJSON
  , object
  , (.=)
  )
import Data.Maybe
  ( mapMaybe
  )
import Data.Set qualified as Set
import Data.Text
  ( Text
  )
import Playwright.BrowserContext
  ( BrowserContext (BrowserContext, browserContextChannel)
  )
import Playwright.BrowserContext.Network
  ( subscribeBrowserContext
  )
import Playwright.Channel
  ( Channel (Channel, channelGuid)
  , WorkerTag
  , send
  )
import Playwright.Connection
  ( closedGuids
  , guidsOfType
  , lookupObject
  , registerEventHandler
  , registerFutureEventWaiterWhere
  , waitForChannelEvent
  , waitForFutureChannelEventUnlessClosed
  , waitForWaiterUnlessClosed
  )
import Playwright.ConsoleMessage
  ( ConsoleMessage
  , consoleMessageFromEvent
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Protocol
  ( BrowserContextConsoleEvent (browserContextConsoleEventWorker)
  , BrowserContextServiceWorkerEvent (browserContextServiceWorkerEventWorker)
  , BrowserContextUpdateSubscriptionParamsEvent (BrowserContextUpdateSubscriptionParamsEventConsole)
  , ChannelType (ChannelTypePage, ChannelTypeWorker)
  , PageWorkerEvent (pageWorkerEventWorker)
  , WorkerEvaluateExpressionParams (workerEvaluateExpressionParamsIsFunction)
  , WorkerEvaluateExpressionResult (workerEvaluateExpressionResultValue)
  , WorkerInitializer (workerInitializerUrl)
  , mkWorkerEvaluateExpressionParams
  )
import Playwright.RaceTimeout
  ( waitWithTimeout
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteGuid, remoteInitializer, remoteParent, remoteType)
  )
import Playwright.SerializedArgument
  ( undefinedArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadNumber, PayloadString)
  , SerializedValue (SerializedNode)
  )
import Playwright.Timeout
  ( defaultTimeout
  )
import Playwright.Url
  ( Url
  )

newtype Worker = Worker
  { workerChannel :: Channel WorkerTag
  }

workerFromRef :: ChannelRef -> Worker
workerFromRef ref =
  Worker (Channel (channelRefGuid ref))

workerUrl :: Worker -> Playwright Url
workerUrl worker = do
  obj <- lookupObject (channelGuid (workerChannel worker))
  initializer <- throwDecode (remoteInitializer obj)
  pure (workerInitializerUrl initializer)

waitForWorker :: Page -> Playwright Worker
waitForWorker page = do
  params <-
    waitForChannelEvent
      (channelGuid (pageChannel page))
      (MethodName "worker")
  event <- throwDecode params
  pure (workerFromRef (pageWorkerEventWorker event))

workers :: Page -> Playwright [Worker]
workers page = do
  closed <- closedGuids
  objects <- traverse (lookupObject) =<< guidsOfType ChannelTypeWorker
  pure
    ( mapMaybe
        ( \obj ->
            if remoteType obj == ChannelTypeWorker
              && remoteParent obj == channelGuid (pageChannel page)
              && Set.notMember (remoteGuid obj) closed
              then Just (Worker (Channel (remoteGuid obj)))
              else Nothing
        )
        objects
    )

waitForServiceWorker :: BrowserContext -> Playwright Worker
waitForServiceWorker ctx =
  waitWithTimeout defaultTimeout $ do
    params <-
      waitForFutureChannelEventUnlessClosed
        (channelGuid (browserContextChannel ctx))
        (MethodName "serviceWorker")
    event <- throwDecode params
    pure (workerFromRef (browserContextServiceWorkerEventWorker event))

contextFromWorker :: Worker -> Playwright BrowserContext
contextFromWorker worker = do
  obj <- lookupObject (channelGuid (workerChannel worker))
  parentObj <- lookupObject (remoteParent obj)
  -- A page worker's parent is its page, and the page's parent is the context.
  let
    contextGuid =
      if remoteType parentObj == ChannelTypePage
        then remoteParent parentObj
        else remoteParent obj
  pure (BrowserContext (Channel contextGuid))

waitForWorkerConsole :: Worker -> Playwright ConsoleMessage
waitForWorkerConsole worker = do
  takeMessage <- armWorkerConsole worker
  takeMessage

-- | Subscribes and registers for the next console message from this worker
-- before the action that logs runs. Console events from other sources on the
-- same context are skipped, not consumed, so they stay available to their own
-- waiters.
armWorkerConsole :: Worker -> Playwright (Playwright ConsoleMessage)
armWorkerConsole worker = do
  ctx <- contextFromWorker worker
  subscribeWorkerConsole worker
  subscribeBrowserContext ctx BrowserContextUpdateSubscriptionParamsEventConsole
  let
    workerGuid = channelGuid (workerChannel worker)
  waitId <-
    registerFutureEventWaiterWhere
      (channelGuid (browserContextChannel ctx))
      (MethodName "console")
      (isWorkerConsoleEvent workerGuid)
  pure $ waitWithTimeout defaultTimeout $ do
    -- Fails when the worker closes, instead of waiting for a log that never comes.
    params <- waitForWaiterUnlessClosed waitId workerGuid
    event <- throwDecode params
    consoleMessageFromEvent (browserContextChannel ctx) event

-- | Asks the worker itself to dispatch its console events. Upstream's context
-- dispatcher only forwards a dedicated worker's console event when the worker
-- channel holds a 'console' subscription, which the context subscription alone
-- does not create. The protocol type is not generated, so the params are raw JSON.
subscribeWorkerConsole :: Worker -> Playwright ()
subscribeWorkerConsole worker = do
  _ :: Value <-
    send
      (workerChannel worker)
      (MethodName "updateSubscription")
      (object ["event" .= ("console" :: Text), "enabled" .= True])
  pure ()

-- | Whether a console event was logged by the worker with the given guid.
isWorkerConsoleEvent :: Guid -> Value -> Bool
isWorkerConsoleEvent workerGuid params =
  case fromJSON params of
    Success event ->
      fmap channelRefGuid (browserContextConsoleEventWorker (event :: BrowserContextConsoleEvent)) == Just workerGuid
    Error _ ->
      False

serviceWorkers :: BrowserContext -> Playwright [Worker]
serviceWorkers ctx = do
  let
    parent = channelGuid (browserContextChannel ctx)
  closed <- closedGuids
  objects <- traverse lookupObject =<< guidsOfType ChannelTypeWorker
  pure
    ( mapMaybe
        ( \obj ->
            if remoteType obj == ChannelTypeWorker
              && remoteParent obj == parent
              && Set.notMember (remoteGuid obj) closed
              then Just (Worker (Channel (remoteGuid obj)))
              else Nothing
        )
        objects
    )

onServiceWorker :: BrowserContext -> (Worker -> Playwright ()) -> Playwright ()
onServiceWorker ctx run =
  registerEventHandler
    (channelGuid (browserContextChannel ctx))
    (MethodName "serviceWorker")
    ( \params -> do
        event <- throwDecode params
        run (workerFromRef (browserContextServiceWorkerEventWorker event))
    )

onWorkerClose :: Worker -> Playwright () -> Playwright ()
onWorkerClose worker run =
  registerEventHandler
    (channelGuid (workerChannel worker))
    (MethodName "close")
    (\_ -> run)

workersEqual :: Worker -> Worker -> Bool
workersEqual left right =
  channelGuid (workerChannel left) == channelGuid (workerChannel right)

evaluateOnWorker :: Worker -> Text -> Playwright SerializedValue
evaluateOnWorker worker expression = do
  result <-
    send
      (workerChannel worker)
      (MethodName "evaluateExpression")
      (mkWorkerEvaluateExpressionParams undefinedArgument expression)
        { workerEvaluateExpressionParamsIsFunction = Just True
        }
  pure (workerEvaluateExpressionResultValue result)

evaluateOnWorkerText :: Worker -> Text -> Playwright Text
evaluateOnWorkerText worker expression = do
  value <- evaluateOnWorker worker expression
  case value of
    SerializedNode (PayloadString text) _ ->
      pure text
    _ ->
      throwIO (EvaluateUnexpected value)

evaluateOnWorkerNumber :: Worker -> Text -> Playwright Double
evaluateOnWorkerNumber worker expression = do
  value <- evaluateOnWorker worker expression
  case value of
    SerializedNode (PayloadNumber number) _ ->
      pure number
    _ ->
      throwIO (EvaluateUnexpected value)
