module Playwright.HttpCredentialsSend
  ( HttpCredentialsSend (..)
  ) where

data HttpCredentialsSend
  = HttpCredentialsSendAlways
  | HttpCredentialsSendUnauthorized
  deriving stock (Eq, Show)
