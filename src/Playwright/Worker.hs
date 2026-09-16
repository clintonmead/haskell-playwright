module Playwright.Worker
  ( Worker (..)
  , evaluateOnWorker
  , evaluateOnWorkerText
  , onServiceWorker
  , serviceWorkers
  , waitForServiceWorker
  , waitForWorker
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
  ( BrowserContext (browserContextChannel)
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
  , waitForFutureChannelEventUnlessClosed
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
  ( SerializedPayload (PayloadString)
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
