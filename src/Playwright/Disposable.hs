module Playwright.Disposable
  ( Disposable (..)
  , disposableFromRef
  , dispose
  ) where

import Playwright.Channel
  ( Channel (Channel)
  , DisposableTag
  , send
  )
import Playwright.Connection
  ( Connection
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( DisposableDisposeResult
  , mkDisposableDisposeParams
  )

newtype Disposable = Disposable
  { disposableChannel :: Channel DisposableTag
  }

dispose :: Disposable -> IO ()
dispose handle = do
  _ :: DisposableDisposeResult <-
    send (disposableChannel handle) (MethodName "dispose") mkDisposableDisposeParams
  pure ()

disposableFromRef :: Connection -> ChannelRef -> Disposable
disposableFromRef conn ref =
  Disposable (Channel conn (channelRefGuid ref))
