module Playwright.SerializedValue
  ( SerializedValue (..)
  , SerializedPayload (..)
  , arrayValue
  , boolValue
  , numberValue
  , objectValue
  , undefinedValue
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , JSONCodec
  , bimapCodec
  , disjointMatchChoiceCodec
  , listCodec
  , named
  , object
  , optionalField'
  , requiredField'
  , requiredFieldWith'
  , rmapCodec
  , valueCodec
  , (.=)
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value (Null)
  )
import Data.Scientific
  ( Scientific
  , fromFloatDigits
  , toRealFloat
  )
import Data.Text
  ( Text
  )
import Playwright.JsErrorValue
  ( JsErrorValue
  )
import Playwright.Json.ProtocolBinary
  ( ProtocolBinary
  )
import Playwright.SpecialJsValue
  ( SpecialJsValue (SpecialUndefined)
  )
import Playwright.TypedArrayKind
  ( TypedArrayKind
  )
import Playwright.Url
  ( Url
  )

data SerializedPayload
  = PayloadNumber Double
  | PayloadBool Bool
  | PayloadString Text
  | PayloadSpecial SpecialJsValue
  | PayloadDate Text
  | PayloadUrl Url
  | PayloadBigInt Text
  | PayloadTypedArray ProtocolBinary TypedArrayKind
  | PayloadError JsErrorValue
  | PayloadRegex Text Text
  | PayloadArray [SerializedValue]
  | PayloadObject [(Text, SerializedValue)]
  | PayloadHandle Int
  deriving stock (Eq, Show)

data SerializedValue
  = SerializedRef Int
  | SerializedNode SerializedPayload (Maybe Int)
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON) via (Autodocodec SerializedValue)

undefinedValue :: SerializedValue
undefinedValue =
  SerializedNode (PayloadSpecial SpecialUndefined) Nothing

numberValue :: Double -> SerializedValue
numberValue n =
  SerializedNode (PayloadNumber n) Nothing

boolValue :: Bool -> SerializedValue
boolValue flag =
  SerializedNode (PayloadBool flag) Nothing

arrayValue :: [SerializedValue] -> SerializedValue
arrayValue items =
  SerializedNode (PayloadArray items) Nothing

objectValue :: [(Text, SerializedValue)] -> SerializedValue
objectValue fields =
  SerializedNode (PayloadObject fields) Nothing

instance HasCodec SerializedValue where
  codec =
    named "SerializedValue" serializedValueCodec

serializedValueCodec :: JSONCodec SerializedValue
serializedValueCodec =
  chooseVariant previewRef SerializedRef refObject $
    chooseVariant previewNumber mkNumber numberObject $
      chooseVariant previewBool mkBool boolObject $
        chooseVariant previewString mkString stringObject $
          chooseVariant previewSpecial mkSpecial specialObject $
            chooseVariant previewDate mkDate dateObject $
              chooseVariant previewUrl mkUrl urlObject $
                chooseVariant previewBigInt mkBigInt bigIntObject $
                  chooseVariant previewTypedArray mkTypedArray typedArrayObject $
                    chooseVariant previewError mkError errorObject $
                      chooseVariant previewRegex mkRegex regexObject $
                        chooseVariant previewArray mkArray arrayObject $
                          chooseVariant previewObject mkObject objectObject $
                            chooseVariant previewHandle mkHandle handleObject unrecognized

chooseVariant ::
  (a -> Maybe b) ->
  (b -> a) ->
  JSONCodec b ->
  JSONCodec a ->
  JSONCodec a
chooseVariant preview construct inner rest =
  disjointMatchChoiceCodec
    (rmapCodec construct inner)
    rest
    $ \value ->
      case preview value of
        Just innerValue ->
          Left innerValue
        Nothing ->
          Right value

unrecognized :: JSONCodec SerializedValue
unrecognized =
  bimapCodec
    (\value -> Left ("unrecognized SerializedValue: " <> show value))
    (const Null)
    valueCodec

refObject :: JSONCodec Int
refObject =
  object "SerializedRef" (requiredField' "ref")

numberObject :: JSONCodec (Scientific, Maybe Int)
numberObject =
  object "SerializedNumber" $
    (,)
      <$> requiredField' "n" .= fst
      <*> optionalField' "id" .= snd

boolObject :: JSONCodec (Bool, Maybe Int)
boolObject =
  object "SerializedBool" $
    (,)
      <$> requiredField' "b" .= fst
      <*> optionalField' "id" .= snd

stringObject :: JSONCodec (Text, Maybe Int)
stringObject =
  object "SerializedString" $
    (,)
      <$> requiredField' "s" .= fst
      <*> optionalField' "id" .= snd

specialObject :: JSONCodec (SpecialJsValue, Maybe Int)
specialObject =
  object "SerializedSpecial" $
    (,)
      <$> requiredField' "v" .= fst
      <*> optionalField' "id" .= snd

dateObject :: JSONCodec (Text, Maybe Int)
dateObject =
  object "SerializedDate" $
    (,)
      <$> requiredField' "d" .= fst
      <*> optionalField' "id" .= snd

urlObject :: JSONCodec (Url, Maybe Int)
urlObject =
  object "SerializedUrl" $
    (,)
      <$> requiredField' "u" .= fst
      <*> optionalField' "id" .= snd

bigIntObject :: JSONCodec (Text, Maybe Int)
bigIntObject =
  object "SerializedBigInt" $
    (,)
      <$> requiredField' "bi" .= fst
      <*> optionalField' "id" .= snd

typedArrayBody :: JSONCodec (ProtocolBinary, TypedArrayKind)
typedArrayBody =
  object "TypedArray" $
    (,)
      <$> requiredField' "b" .= fst
      <*> requiredField' "k" .= snd

typedArrayObject :: JSONCodec (ProtocolBinary, TypedArrayKind, Maybe Int)
typedArrayObject =
  object "SerializedTypedArray" $
    (\(bytes, kind) ident -> (bytes, kind, ident))
      <$> requiredFieldWith' "ta" typedArrayBody .= (\(bytes, kind, _) -> (bytes, kind))
      <*> optionalField' "id" .= (\(_, _, ident) -> ident)

errorObject :: JSONCodec (JsErrorValue, Maybe Int)
errorObject =
  object "SerializedErrorValue" $
    (,)
      <$> requiredField' "e" .= fst
      <*> optionalField' "id" .= snd

regexBody :: JSONCodec (Text, Text)
regexBody =
  object "Regex" $
    (,)
      <$> requiredField' "p" .= fst
      <*> requiredField' "f" .= snd

regexObject :: JSONCodec (Text, Text, Maybe Int)
regexObject =
  object "SerializedRegex" $
    (\(pattern, flags) ident -> (pattern, flags, ident))
      <$> requiredFieldWith' "r" regexBody .= (\(pattern, flags, _) -> (pattern, flags))
      <*> optionalField' "id" .= (\(_, _, ident) -> ident)

arrayObject :: JSONCodec ([SerializedValue], Maybe Int)
arrayObject =
  object "SerializedArray" $
    (,)
      <$> requiredField' "a" .= fst
      <*> optionalField' "id" .= snd

objectEntry :: JSONCodec (Text, SerializedValue)
objectEntry =
  object "SerializedObjectEntry" $
    (,)
      <$> requiredField' "k" .= fst
      <*> requiredField' "v" .= snd

objectObject :: JSONCodec ([(Text, SerializedValue)], Maybe Int)
objectObject =
  object "SerializedObject" $
    (,)
      <$> requiredFieldWith' "o" (listCodec objectEntry) .= fst
      <*> optionalField' "id" .= snd

handleObject :: JSONCodec (Int, Maybe Int)
handleObject =
  object "SerializedHandle" $
    (,)
      <$> requiredField' "h" .= fst
      <*> optionalField' "id" .= snd

previewRef :: SerializedValue -> Maybe Int
previewRef value =
  case value of
    SerializedRef ref ->
      Just ref
    SerializedNode _ _ ->
      Nothing

previewNumber :: SerializedValue -> Maybe (Scientific, Maybe Int)
previewNumber value =
  case value of
    SerializedNode (PayloadNumber n) ident ->
      Just (fromFloatDigits n, ident)
    _ ->
      Nothing

previewBool :: SerializedValue -> Maybe (Bool, Maybe Int)
previewBool value =
  case value of
    SerializedNode (PayloadBool b) ident ->
      Just (b, ident)
    _ ->
      Nothing

previewString :: SerializedValue -> Maybe (Text, Maybe Int)
previewString value =
  case value of
    SerializedNode (PayloadString s) ident ->
      Just (s, ident)
    _ ->
      Nothing

previewSpecial :: SerializedValue -> Maybe (SpecialJsValue, Maybe Int)
previewSpecial value =
  case value of
    SerializedNode (PayloadSpecial special) ident ->
      Just (special, ident)
    _ ->
      Nothing

previewDate :: SerializedValue -> Maybe (Text, Maybe Int)
previewDate value =
  case value of
    SerializedNode (PayloadDate d) ident ->
      Just (d, ident)
    _ ->
      Nothing

previewUrl :: SerializedValue -> Maybe (Url, Maybe Int)
previewUrl value =
  case value of
    SerializedNode (PayloadUrl url) ident ->
      Just (url, ident)
    _ ->
      Nothing

previewBigInt :: SerializedValue -> Maybe (Text, Maybe Int)
previewBigInt value =
  case value of
    SerializedNode (PayloadBigInt bi) ident ->
      Just (bi, ident)
    _ ->
      Nothing

previewTypedArray :: SerializedValue -> Maybe (ProtocolBinary, TypedArrayKind, Maybe Int)
previewTypedArray value =
  case value of
    SerializedNode (PayloadTypedArray bytes kind) ident ->
      Just (bytes, kind, ident)
    _ ->
      Nothing

previewError :: SerializedValue -> Maybe (JsErrorValue, Maybe Int)
previewError value =
  case value of
    SerializedNode (PayloadError err) ident ->
      Just (err, ident)
    _ ->
      Nothing

previewRegex :: SerializedValue -> Maybe (Text, Text, Maybe Int)
previewRegex value =
  case value of
    SerializedNode (PayloadRegex pattern flags) ident ->
      Just (pattern, flags, ident)
    _ ->
      Nothing

previewArray :: SerializedValue -> Maybe ([SerializedValue], Maybe Int)
previewArray value =
  case value of
    SerializedNode (PayloadArray values) ident ->
      Just (values, ident)
    _ ->
      Nothing

previewObject :: SerializedValue -> Maybe ([(Text, SerializedValue)], Maybe Int)
previewObject value =
  case value of
    SerializedNode (PayloadObject fields) ident ->
      Just (fields, ident)
    _ ->
      Nothing

previewHandle :: SerializedValue -> Maybe (Int, Maybe Int)
previewHandle value =
  case value of
    SerializedNode (PayloadHandle handle) ident ->
      Just (handle, ident)
    _ ->
      Nothing

mkNumber :: (Scientific, Maybe Int) -> SerializedValue
mkNumber (scientific, ident) =
  SerializedNode (PayloadNumber (toRealFloat scientific)) ident

mkBool :: (Bool, Maybe Int) -> SerializedValue
mkBool (b, ident) =
  SerializedNode (PayloadBool b) ident

mkString :: (Text, Maybe Int) -> SerializedValue
mkString (s, ident) =
  SerializedNode (PayloadString s) ident

mkSpecial :: (SpecialJsValue, Maybe Int) -> SerializedValue
mkSpecial (special, ident) =
  SerializedNode (PayloadSpecial special) ident

mkDate :: (Text, Maybe Int) -> SerializedValue
mkDate (d, ident) =
  SerializedNode (PayloadDate d) ident

mkUrl :: (Url, Maybe Int) -> SerializedValue
mkUrl (url, ident) =
  SerializedNode (PayloadUrl url) ident

mkBigInt :: (Text, Maybe Int) -> SerializedValue
mkBigInt (bi, ident) =
  SerializedNode (PayloadBigInt bi) ident

mkTypedArray :: (ProtocolBinary, TypedArrayKind, Maybe Int) -> SerializedValue
mkTypedArray (bytes, kind, ident) =
  SerializedNode (PayloadTypedArray bytes kind) ident

mkError :: (JsErrorValue, Maybe Int) -> SerializedValue
mkError (err, ident) =
  SerializedNode (PayloadError err) ident

mkRegex :: (Text, Text, Maybe Int) -> SerializedValue
mkRegex (pattern, flags, ident) =
  SerializedNode (PayloadRegex pattern flags) ident

mkArray :: ([SerializedValue], Maybe Int) -> SerializedValue
mkArray (values, ident) =
  SerializedNode (PayloadArray values) ident

mkObject :: ([(Text, SerializedValue)], Maybe Int) -> SerializedValue
mkObject (fields, ident) =
  SerializedNode (PayloadObject fields) ident

mkHandle :: (Int, Maybe Int) -> SerializedValue
mkHandle (handle, ident) =
  SerializedNode (PayloadHandle handle) ident
