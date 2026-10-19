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
  , waitForChannelEvent
  , waitForChannelEventUnlessClosed
  , waitForFutureChannelEventUnlessClosed
  )
import Playwright.ConsoleMessage
  ( ConsoleMessage (consoleMessageWorkerRef)
  , consoleMessageFromEvent
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
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
  ( BrowserContextServiceWorkerEvent (browserContextServiceWorkerEventWorker)
  , BrowserContextUpdateSubscriptionParamsEvent (BrowserContextUpdateSubscriptionParamsEventConsole)
  , ChannelType (ChannelTypeWorker)
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
  pure (BrowserContext (Channel (remoteParent obj)))

waitForWorkerConsole :: Worker -> Playwright ConsoleMessage
waitForWorkerConsole worker = do
  takeMessage <- armWorkerConsole worker
  takeMessage

-- | Subscribes now and returns the wait for the next console message from this
-- worker. Use it before the action that logs: messages logged before the
-- subscription are never sent, and messages sent while no waiter is registered
-- are buffered for the returned wait.
armWorkerConsole :: Worker -> Playwright (Playwright ConsoleMessage)
armWorkerConsole worker = do
  ctx <- contextFromWorker worker
  subscribeBrowserContext ctx BrowserContextUpdateSubscriptionParamsEventConsole
  pure (waitWithTimeout defaultTimeout collect)
  where
    collect = do
      ctx' <- contextFromWorker worker
      params <-
        waitForChannelEventUnlessClosed
          (channelGuid (browserContextChannel ctx'))
          (MethodName "console")
          (channelGuid (workerChannel worker))
      event <- throwDecode params
      message <- consoleMessageFromEvent (browserContextChannel ctx') event
      case consoleMessageWorkerRef message of
        Just ref
          | channelRefGuid ref == channelGuid (workerChannel worker) ->
              pure message
        _ ->
          collect

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
