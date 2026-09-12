module Playwright.TimeoutDefaults
  ( TimeoutDefaults (..)
  , emptyTimeoutDefaults
  , resolveActionTimeout
  , resolveNavigationTimeout
  ) where

import Data.Maybe
  ( fromMaybe
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  )

data TimeoutDefaults = TimeoutDefaults
  { timeoutDefaultsTimeout :: Maybe Timeout
  , timeoutDefaultsNavigation :: Maybe Timeout
  }
  deriving stock (Eq, Show)

emptyTimeoutDefaults :: TimeoutDefaults
emptyTimeoutDefaults =
  TimeoutDefaults
    { timeoutDefaultsTimeout = Nothing
    , timeoutDefaultsNavigation = Nothing
    }

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust preferred fallback =
  case preferred of
    Just value ->
      Just value
    Nothing ->
      fallback

resolveActionTimeout :: TimeoutDefaults -> TimeoutDefaults -> Timeout
resolveActionTimeout page context =
  fromMaybe
    defaultTimeout
    (firstJust (timeoutDefaultsTimeout page) (timeoutDefaultsTimeout context))

resolveNavigationTimeout :: TimeoutDefaults -> TimeoutDefaults -> Timeout
resolveNavigationTimeout page context =
  fromMaybe
    defaultTimeout
    ( firstJust
        (timeoutDefaultsNavigation page)
        ( firstJust
            (timeoutDefaultsTimeout page)
            ( firstJust
                (timeoutDefaultsNavigation context)
                (timeoutDefaultsTimeout context)
            )
        )
    )
