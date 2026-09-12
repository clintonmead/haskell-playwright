module Playwright.Disposable
  ( Disposable (..)
  , disposableFromRef
  , dispose
  ) where

import Playwright.Monad
  ( Playwright
  )
import Playwright.Channel
  ( Channel (Channel)
  , DisposableTag
  , send
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

dispose :: Disposable -> Playwright ()
dispose handle = do
  _ :: DisposableDisposeResult <-
    send (disposableChannel handle) (MethodName "dispose") mkDisposableDisposeParams
  pure ()

disposableFromRef :: ChannelRef -> Disposable
disposableFromRef ref =
  Disposable (Channel (channelRefGuid ref))
