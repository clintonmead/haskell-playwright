module Playwright.Connection.Types
  ( Connection (..)
  , OutgoingCall (..)
  , modifySession
  , readSession
  , stateSession
  , withSession
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , object
  , requiredField'
  , (.=)
  )
import Control.Concurrent.STM
  ( STM
  , TVar
  , modifyTVar'
  , readTVar
  , writeTVar
  )
import Control.Concurrent.STM.TQueue
  ( TQueue
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.MethodName
  ( MethodName
  )
import Playwright.RequestId
  ( RequestId
  )
import Playwright.Session
  ( Session
  )
import Playwright.Transport
  ( Transport
  )

data Connection = Connection
  { connectionTransport :: Transport
  , connectionSession :: TVar Session
  , connectionHandlerQueue :: TQueue (IO ())
  }

readSession :: Connection -> STM Session
readSession conn =
  readTVar (connectionSession conn)

modifySession :: Connection -> (Session -> Session) -> STM ()
modifySession conn =
  modifyTVar' (connectionSession conn)

withSession :: Connection -> (Session -> STM (a, Session)) -> STM a
withSession conn use = do
  session <- readTVar (connectionSession conn)
  (value, session') <- use session
  writeTVar (connectionSession conn) $! session'
  pure value

stateSession :: Connection -> (Session -> (a, Session)) -> STM a
stateSession conn use =
  withSession conn (pure . use)

data OutgoingCall = OutgoingCall
  { outgoingCallId :: RequestId
  , outgoingCallGuid :: Guid
  , outgoingCallMethod :: MethodName
  , outgoingCallParams :: Value
  , outgoingCallMetadata :: Value
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec OutgoingCall)

instance HasCodec OutgoingCall where
  codec =
    object "OutgoingCall" $
      OutgoingCall
        <$> requiredField' "id" .= outgoingCallId
        <*> requiredField' "guid" .= outgoingCallGuid
        <*> requiredField' "method" .= outgoingCallMethod
        <*> requiredField' "params" .= outgoingCallParams
        <*> requiredField' "metadata" .= outgoingCallMetadata
