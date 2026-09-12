module Playwright.DropPayload
  ( DropPayload (..)
  , DropFile (..)
  , DropData (..)
  , emptyDropPayload
  ) where

import Data.ByteString
  ( ByteString
  )
import Data.Text
  ( Text
  )
import Playwright.InputFileName
  ( InputFileName
  )
import Playwright.MimeType
  ( MimeType
  )

data DropFile = DropFile
  { dropFileName :: InputFileName
  , dropFileMimeType :: Maybe MimeType
  , dropFileBytes :: ByteString
  }
  deriving stock (Eq, Show)

data DropData = DropData
  { dropDataMimeType :: MimeType
  , dropDataValue :: Text
  }
  deriving stock (Eq, Show)

data DropPayload = DropPayload
  { dropPayloadFiles :: [DropFile]
  , dropPayloadData :: [DropData]
  }
  deriving stock (Eq, Show)

emptyDropPayload :: DropPayload
emptyDropPayload =
  DropPayload
    { dropPayloadFiles = []
    , dropPayloadData = []
    }
