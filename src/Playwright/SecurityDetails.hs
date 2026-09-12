module Playwright.SecurityDetails
  ( SecurityDetails (..)
  , fromProtocolSecurityDetails
  ) where

import Data.Scientific
  ( Scientific
  )
import Data.Text
  ( Text
  )
import Playwright.Protocol qualified as Protocol
  ( SecurityDetails (securityDetailsIssuer, securityDetailsProtocol, securityDetailsSubjectName, securityDetailsValidFrom, securityDetailsValidTo)
  )

data SecurityDetails = SecurityDetails
  { securityDetailsIssuer :: Maybe Text
  , securityDetailsProtocol :: Maybe Text
  , securityDetailsSubjectName :: Maybe Text
  , securityDetailsValidFrom :: Maybe Scientific
  , securityDetailsValidTo :: Maybe Scientific
  }
  deriving stock (Eq, Show)

fromProtocolSecurityDetails :: Protocol.SecurityDetails -> SecurityDetails
fromProtocolSecurityDetails details =
  SecurityDetails
    { securityDetailsIssuer = Protocol.securityDetailsIssuer details
    , securityDetailsProtocol = Protocol.securityDetailsProtocol details
    , securityDetailsSubjectName = Protocol.securityDetailsSubjectName details
    , securityDetailsValidFrom = Protocol.securityDetailsValidFrom details
    , securityDetailsValidTo = Protocol.securityDetailsValidTo details
    }
