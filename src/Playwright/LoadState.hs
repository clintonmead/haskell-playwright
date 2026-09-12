module Playwright.LoadState
  ( LoadState (..)
  , defaultNavigationLoadState
  , toLifecycle
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , JSONCodec
  , bimapCodec
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Playwright.Protocol
  ( LifecycleEvent (LifecycleEventCommit, LifecycleEventDomcontentloaded, LifecycleEventLoad, LifecycleEventNetworkidle)
  )

data LoadState
  = LoadStateLoad
  | LoadStateDomContentLoaded
  | LoadStateNetworkIdle
  | LoadStateCommit
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec LoadState)

defaultNavigationLoadState :: LoadState
defaultNavigationLoadState =
  LoadStateLoad

toLifecycle :: LoadState -> LifecycleEvent
toLifecycle state =
  case state of
    LoadStateLoad ->
      LifecycleEventLoad
    LoadStateDomContentLoaded ->
      LifecycleEventDomcontentloaded
    LoadStateNetworkIdle ->
      LifecycleEventNetworkidle
    LoadStateCommit ->
      LifecycleEventCommit

fromLifecycle :: LifecycleEvent -> Either String LoadState
fromLifecycle event =
  case event of
    LifecycleEventLoad ->
      Right LoadStateLoad
    LifecycleEventDomcontentloaded ->
      Right LoadStateDomContentLoaded
    LifecycleEventNetworkidle ->
      Right LoadStateNetworkIdle
    LifecycleEventCommit ->
      Right LoadStateCommit

instance HasCodec LoadState where
  codec =
    bimapCodec fromLifecycle toLifecycle (codec :: JSONCodec LifecycleEvent)
