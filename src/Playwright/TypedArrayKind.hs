module Playwright.TypedArrayKind
  ( TypedArrayKind (..)
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , stringConstCodec
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.List.NonEmpty
  ( NonEmpty ((:|))
  )

data TypedArrayKind
  = TypedArrayI8
  | TypedArrayUi8
  | TypedArrayUi8c
  | TypedArrayI16
  | TypedArrayUi16
  | TypedArrayI32
  | TypedArrayUi32
  | TypedArrayF32
  | TypedArrayF64
  | TypedArrayBi64
  | TypedArrayBui64
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec TypedArrayKind)

instance HasCodec TypedArrayKind where
  codec =
    stringConstCodec $
      (TypedArrayI8, "i8")
        :| [ (TypedArrayUi8, "ui8")
           , (TypedArrayUi8c, "ui8c")
           , (TypedArrayI16, "i16")
           , (TypedArrayUi16, "ui16")
           , (TypedArrayI32, "i32")
           , (TypedArrayUi32, "ui32")
           , (TypedArrayF32, "f32")
           , (TypedArrayF64, "f64")
           , (TypedArrayBi64, "bi64")
           , (TypedArrayBui64, "bui64")
           ]
