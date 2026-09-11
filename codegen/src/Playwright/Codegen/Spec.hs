module Playwright.Codegen.Spec
  ( loadSpecDir
  , loadSpec
  ) where

import Data.Aeson
  ( Value (Array, Bool, Null, Number, Object, String)
  )
import Data.Aeson.Key
  ( Key
  )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap
  ( KeyMap
  )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List
  ( sort
  )
import Data.Map.Strict
  ( Map
  )
import Data.Map.Strict qualified as Map
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Data.Yaml qualified as Yaml
import Playwright.Codegen.Types
  ( CommandDef (CommandDef, commandName, commandParams, commandReturns)
  , EnumDef (EnumDef, enumLiterals, enumName)
  , EventDef (EventDef, eventName, eventParams)
  , Field (Field, fieldOptional, fieldType)
  , InterfaceDef (InterfaceDef, interfaceCommands, interfaceEvents, interfaceExtends, interfaceInitializer, interfaceName)
  , JsonName (JsonName)
  , MixinDef (MixinDef, mixinFields, mixinName)
  , ObjectDef (ObjectDef, objectFields, objectName)
  , ProtoType (TArray, TBinary, TBool, TEnum, TFloat, TInt, TJson, TNamed, TObject, TString, TUndefined)
  , Spec (Spec, specEnums, specInterfaces, specMixins, specObjects)
  , TypeName (TypeName)
  )
import System.Directory
  ( listDirectory
  )
import System.FilePath
  ( takeExtension
  , (</>)
  )

loadSpecDir :: FilePath -> IO Spec
loadSpecDir dir = do
  names <- listDirectory dir
  let
    yamlFiles :: [FilePath]
    yamlFiles = sort [name | name <- names, takeExtension name == ".yml"]
  values <- traverse (decodeFile . (dir </>)) yamlFiles
  merged <- either fail pure (mergeObjects values)
  either fail pure (loadSpec merged)

decodeFile :: FilePath -> IO Value
decodeFile path = do
  result <- Yaml.decodeFileEither path
  case result of
    Left err ->
      fail (path <> ": " <> Yaml.prettyPrintParseException err)
    Right value ->
      pure value

mergeObjects :: [Value] -> Either String (KeyMap Value)
mergeObjects = go KeyMap.empty
  where
    go :: KeyMap Value -> [Value] -> Either String (KeyMap Value)
    go acc [] =
      Right acc
    go acc (Object obj : rest) = do
      acc' <- foldlMerge acc (KeyMap.toList obj)
      go acc' rest
    go _ (other : _) =
      Left ("Expected top-level YAML mapping, got: " <> show other)

    foldlMerge :: KeyMap Value -> [(Key, Value)] -> Either String (KeyMap Value)
    foldlMerge acc [] =
      Right acc
    foldlMerge acc ((key, value) : rest) =
      if KeyMap.member key acc
        then Left ("Duplicate top-level protocol name: " <> Key.toString key)
        else foldlMerge (KeyMap.insert key value acc) rest

loadSpec :: KeyMap Value -> Either String Spec
loadSpec raw = do
  mixins0 <- Map.fromList <$> mapMaybeM (parseMixin Map.empty) (KeyMap.toList raw)
  mixins <- Map.fromList <$> mapMaybeM (parseMixin mixins0) (KeyMap.toList raw)
  enums <- Map.fromList <$> mapMaybeM parseEnum (KeyMap.toList raw)
  objects <- Map.fromList <$> mapMaybeM (parseObject mixins) (KeyMap.toList raw)
  interfaces <- Map.fromList <$> mapMaybeM (parseInterface mixins) (KeyMap.toList raw)
  pure
    Spec
      { specInterfaces = interfaces
      , specObjects = objects
      , specEnums = enums
      , specMixins = mixins
      }

mapMaybeM :: (a -> Either String (Maybe b)) -> [a] -> Either String [b]
mapMaybeM _ [] =
  Right []
mapMaybeM f (x : xs) = do
  mx <- f x
  rest <- mapMaybeM f xs
  pure (maybe rest (: rest) mx)

parseKind :: KeyMap Value -> Either String Text
parseKind obj =
  case KeyMap.lookup "type" obj of
    Just (String t) ->
      Right t
    Just other ->
      Left ("Invalid type field: " <> show other)
    Nothing ->
      Left "Missing type field"

parseMixin :: Map TypeName MixinDef -> (Key, Value) -> Either String (Maybe (TypeName, MixinDef))
parseMixin mixins (key, value) =
  withObject value $ \obj -> do
    kind <- parseKind obj
    if kind /= "mixin"
      then Right Nothing
      else do
        fields <- parseProperties mixins obj
        let
          name :: TypeName
          name = TypeName (Key.toText key)
        Right (Just (name, MixinDef {mixinName = name, mixinFields = fields}))

parseEnum :: (Key, Value) -> Either String (Maybe (TypeName, EnumDef))
parseEnum (key, value) =
  withObject value $ \obj -> do
    kind <- parseKind obj
    if kind /= "enum"
      then Right Nothing
      else do
        lits <- parseStringList obj "literals"
        let
          name :: TypeName
          name = TypeName (Key.toText key)
        Right (Just (name, EnumDef {enumName = name, enumLiterals = lits}))

parseObject :: Map TypeName MixinDef -> (Key, Value) -> Either String (Maybe (TypeName, ObjectDef))
parseObject mixins (key, value) =
  withObject value $ \obj -> do
    kind <- parseKind obj
    if kind /= "object"
      then Right Nothing
      else do
        fields <- parseProperties mixins obj
        let
          name :: TypeName
          name = TypeName (Key.toText key)
        Right (Just (name, ObjectDef {objectName = name, objectFields = fields}))

parseInterface :: Map TypeName MixinDef -> (Key, Value) -> Either String (Maybe (TypeName, InterfaceDef))
parseInterface mixins (key, value) =
  withObject value $ \obj -> do
    kind <- parseKind obj
    if kind /= "interface"
      then Right Nothing
      else do
        extends <- parseOptionalName obj "extends"
        initializer <- parseOptionalProperties mixins obj "initializer"
        commands <- parseCommands mixins obj
        events <- parseEvents mixins obj
        let
          name :: TypeName
          name = TypeName (Key.toText key)
        Right
          ( Just
              ( name
              , InterfaceDef
                  { interfaceName = name
                  , interfaceExtends = extends
                  , interfaceInitializer = initializer
                  , interfaceCommands = commands
                  , interfaceEvents = events
                  }
              )
          )

parseCommands :: Map TypeName MixinDef -> KeyMap Value -> Either String [CommandDef]
parseCommands mixins obj =
  case KeyMap.lookup "commands" obj of
    Nothing ->
      Right []
    Just Null ->
      Right []
    Just (Object cmds) ->
      traverse (parseCommand mixins) (KeyMap.toList cmds)
    Just other ->
      Left ("commands must be a mapping, got: " <> show other)

parseCommand :: Map TypeName MixinDef -> (Key, Value) -> Either String CommandDef
parseCommand mixins (key, value) =
  withObjectOrNull value $ \obj -> do
    params <- parseOptionalProperties mixins obj "parameters"
    returns <- parseOptionalProperties mixins obj "returns"
    Right
      CommandDef
        { commandName = JsonName (Key.toText key)
        , commandParams = params
        , commandReturns = returns
        }

parseEvents :: Map TypeName MixinDef -> KeyMap Value -> Either String [EventDef]
parseEvents mixins obj =
  case KeyMap.lookup "events" obj of
    Nothing ->
      Right []
    Just Null ->
      Right []
    Just (Object evs) ->
      traverse (parseEvent mixins) (KeyMap.toList evs)
    Just other ->
      Left ("events must be a mapping, got: " <> show other)

parseEvent :: Map TypeName MixinDef -> (Key, Value) -> Either String EventDef
parseEvent mixins (key, value) =
  withObjectOrNull value $ \obj -> do
    params <- parseOptionalProperties mixins obj "parameters"
    Right
      EventDef
        { eventName = JsonName (Key.toText key)
        , eventParams = params
        }

parseOptionalName :: KeyMap Value -> Key -> Either String (Maybe TypeName)
parseOptionalName obj key =
  case KeyMap.lookup key obj of
    Nothing ->
      Right Nothing
    Just (String t) ->
      Right (Just (TypeName t))
    Just other ->
      Left ("Expected string for " <> Key.toString key <> ", got: " <> show other)

parseOptionalProperties :: Map TypeName MixinDef -> KeyMap Value -> Key -> Either String [(JsonName, Field)]
parseOptionalProperties mixins obj key =
  case KeyMap.lookup key obj of
    Nothing ->
      Right []
    Just Null ->
      Right []
    Just (Object props) ->
      flattenProperties mixins (KeyMap.toList props)
    Just other ->
      Left ("Expected mapping for " <> Key.toString key <> ", got: " <> show other)

parseProperties :: Map TypeName MixinDef -> KeyMap Value -> Either String [(JsonName, Field)]
parseProperties mixins obj =
  case KeyMap.lookup "properties" obj of
    Nothing ->
      Right []
    Just Null ->
      Right []
    Just (Object props) ->
      flattenProperties mixins (KeyMap.toList props)
    Just other ->
      Left ("properties must be a mapping, got: " <> show other)

flattenProperties :: Map TypeName MixinDef -> [(Key, Value)] -> Either String [(JsonName, Field)]
flattenProperties mixins = go
  where
    go :: [(Key, Value)] -> Either String [(JsonName, Field)]
    go [] =
      Right []
    go ((key, value) : rest) =
      let
        keyText :: Text
        keyText = Key.toText key
      in
        if Text.isPrefixOf "$mixin" keyText
          then case value of
            Null ->
              go rest
            _ -> do
              mixinName <- stringValue value
              restFields <- go rest
              case Map.lookup (TypeName mixinName) mixins of
                Nothing ->
                  Right restFields
                Just mixin ->
                  Right (mixinFields mixin <> restFields)
          else do
            field <-
              case parseField mixins value of
                Left err ->
                  Left (Key.toString key <> ": " <> err)
                Right parsed ->
                  Right parsed
            restFields <- go rest
            Right ((JsonName keyText, field) : restFields)

parseField :: Map TypeName MixinDef -> Value -> Either String Field
parseField mixins value = do
  (optionalFlag, ty) <- parseProtoType mixins value
  Right Field {fieldOptional = optionalFlag, fieldType = ty}

parseProtoType :: Map TypeName MixinDef -> Value -> Either String (Bool, ProtoType)
parseProtoType mixins value =
  case value of
    String t ->
      Right (splitOptionalNamed t)
    Object obj ->
      parseStructuredType mixins obj
    other ->
      Left ("Invalid type value: " <> show other)

splitOptionalNamed :: Text -> (Bool, ProtoType)
splitOptionalNamed t =
  if Text.isSuffixOf "?" t
    then (True, namedOrPrim (Text.dropEnd 1 t))
    else (False, namedOrPrim t)

namedOrPrim :: Text -> ProtoType
namedOrPrim t =
  case t of
    "string" -> TString
    "int" -> TInt
    "float" -> TFloat
    "number" -> TFloat
    "boolean" -> TBool
    "binary" -> TBinary
    "json" -> TJson
    "undefined" -> TUndefined
    "Channel" -> TNamed (TypeName "Channel")
    other -> TNamed (TypeName other)

parseStructuredType :: Map TypeName MixinDef -> KeyMap Value -> Either String (Bool, ProtoType)
parseStructuredType mixins obj = do
  raw <-
    case KeyMap.lookup "type" obj of
      Just (String t) ->
        Right t
      Just other ->
        Left ("Structured type.type must be a string, got: " <> show other)
      Nothing ->
        Left "Structured type missing type field"
  let
    (base, optionalFlag) = stripOptional raw
  case base of
    "array" -> do
      items <-
        case KeyMap.lookup "items" obj of
          Just v ->
            Right v
          Nothing ->
            Left "array type missing items"
      (_, itemTy) <- parseProtoType mixins items
      Right (optionalFlag, TArray itemTy)
    "enum" -> do
      lits <- parseStringList obj "literals"
      Right (optionalFlag, TEnum lits)
    "object" -> do
      fields <- parseProperties mixins obj
      Right (optionalFlag, TObject fields)
    other ->
      Left ("Unknown structured type: " <> Text.unpack other)

stripOptional :: Text -> (Text, Bool)
stripOptional t =
  if Text.isSuffixOf "?" t
    then (Text.dropEnd 1 t, True)
    else (t, False)

parseStringList :: KeyMap Value -> Key -> Either String [Text]
parseStringList obj key =
  case KeyMap.lookup key obj of
    Just (Array arr) ->
      traverse stringValue (Vector.toList arr)
    Just other ->
      Left ("Expected array for " <> Key.toString key <> ", got: " <> show other)
    Nothing ->
      Left ("Missing " <> Key.toString key)

stringValue :: Value -> Either String Text
stringValue value =
  case value of
    String t ->
      Right t
    Null ->
      Right "null"
    Number n ->
      Right (Text.pack (show n))
    Bool True ->
      Right "true"
    Bool False ->
      Right "false"
    other ->
      Left ("Expected string, got: " <> show other)

withObject :: Value -> (KeyMap Value -> Either String a) -> Either String a
withObject value f =
  case value of
    Object obj ->
      f obj
    other ->
      Left ("Expected mapping, got: " <> show other)

withObjectOrNull :: Value -> (KeyMap Value -> Either String a) -> Either String a
withObjectOrNull value f =
  case value of
    Null ->
      f KeyMap.empty
    Object obj ->
      f obj
    other ->
      Left ("Expected mapping or null, got: " <> show other)
