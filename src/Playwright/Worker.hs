module Playwright.Worker
  ( Worker (..)
  , evaluateOnWorker
  , evaluateOnWorkerText
  , waitForWorker
  , workerFromRef
  , workerUrl
  , workers
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Maybe
  ( mapMaybe
  )
import Data.Set qualified as Set
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( Channel (Channel, channelConnection, channelGuid)
  , WorkerTag
  , send
  )
import Playwright.Connection
  ( Connection
  , closedGuids
  , guidsOfType
  , lookupObject
  , waitForChannelEvent
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
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
  ( ChannelType (ChannelTypeWorker)
  , PageWorkerEvent (pageWorkerEventWorker)
  , WorkerEvaluateExpressionParams (workerEvaluateExpressionParamsIsFunction)
  , WorkerEvaluateExpressionResult (workerEvaluateExpressionResultValue)
  , WorkerInitializer (workerInitializerUrl)
  , mkWorkerEvaluateExpressionParams
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
import Playwright.Url
  ( Url
  )

newtype Worker = Worker
  { workerChannel :: Channel WorkerTag
  }

workerFromRef :: Connection -> ChannelRef -> Worker
workerFromRef conn ref =
  Worker (Channel conn (channelRefGuid ref))

workerUrl :: Worker -> IO Url
workerUrl worker = do
  obj <- lookupObject (channelConnection (workerChannel worker)) (channelGuid (workerChannel worker))
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  pure (workerInitializerUrl initializer)

waitForWorker :: Page -> IO Worker
waitForWorker page = do
  params <-
    waitForChannelEvent
      (channelConnection (pageChannel page))
      (channelGuid (pageChannel page))
      (MethodName "worker")
  event <- either throwIO pure (fromJsonValue params)
  pure (workerFromRef (channelConnection (pageChannel page)) (pageWorkerEventWorker event))

workers :: Page -> IO [Worker]
workers page = do
  let conn = channelConnection (pageChannel page)
  closed <- closedGuids conn
  objects <- traverse (lookupObject conn) =<< guidsOfType conn ChannelTypeWorker
  pure
    ( mapMaybe
        ( \obj ->
            if remoteType obj == ChannelTypeWorker
              && remoteParent obj == channelGuid (pageChannel page)
              && Set.notMember (remoteGuid obj) closed
              then Just (Worker (Channel conn (remoteGuid obj)))
              else Nothing
        )
        objects
    )

evaluateOnWorker :: Worker -> Text -> IO SerializedValue
evaluateOnWorker worker expression = do
  result <-
    send
      (workerChannel worker)
      (MethodName "evaluateExpression")
      (mkWorkerEvaluateExpressionParams undefinedArgument expression)
        { workerEvaluateExpressionParamsIsFunction = Just True
        }
  pure (workerEvaluateExpressionResultValue result)

evaluateOnWorkerText :: Worker -> Text -> IO Text
evaluateOnWorkerText worker expression = do
  value <- evaluateOnWorker worker expression
  case value of
    SerializedNode (PayloadString text) _ ->
      pure text
    _ ->
      throwIO (EvaluateUnexpected value)
