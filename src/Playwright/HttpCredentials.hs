module Playwright.HttpCredentials
  ( HttpCredentials (..)
  , HttpPassword (..)
  , HttpUsername (..)
  ) where

import Data.Text
  ( Text
  )
import Playwright.Url
  ( Url
  )

newtype HttpUsername = HttpUsername
  { httpUsernameToText :: Text
  }
  deriving stock (Eq, Show)

newtype HttpPassword = HttpPassword
  { httpPasswordToText :: Text
  }
  deriving stock (Eq, Show)

data HttpCredentials = HttpCredentials
  { httpCredentialsUsername :: HttpUsername
  , httpCredentialsPassword :: HttpPassword
  , httpCredentialsOrigin :: Maybe Url
  }
  deriving stock (Eq, Show)
