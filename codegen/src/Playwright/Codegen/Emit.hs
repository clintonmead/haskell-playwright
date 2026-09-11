module Playwright.Codegen.Emit
  ( emitModule
  ) where

import Control.Monad.Trans.State.Strict
  ( State
  , execState
  , modify
  )
import Data.Map.Strict
  ( Map
  )
import Data.Map.Strict qualified as Map
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Playwright.Codegen.Names
  ( fieldName
  , literalConstructor
  , pascalCase
  )
import Playwright.Codegen.Types
  ( CommandDef (commandName, commandParams, commandReturns)
  , EnumDef (enumLiterals, enumName)
  , EventDef (eventName, eventParams)
  , Field (fieldOptional, fieldType)
  , GeneratedEnum (GeneratedEnum, generatedEnumConstructors, generatedEnumName)
  , InterfaceDef (interfaceCommands, interfaceEvents, interfaceInitializer, interfaceName)
  , JsonName (jsonNameText)
  , ObjectDef (objectFields, objectName)
  , ProtoType (TArray, TBinary, TBool, TEnum, TFloat, TInt, TJson, TNamed, TObject, TString, TUndefined)
  , RecordDef (RecordDef, recordFields, recordName)
  , RecordField (RecordField, recordFieldHs, recordFieldHsType, recordFieldJson, recordFieldOptional)
  , Spec (specEnums, specInterfaces, specObjects)
  , TypeName (typeNameText)
  )

data EmitState = EmitState
  { emitRecords :: Map Text RecordDef
  , emitEnums :: Map Text GeneratedEnum
  }

emptyState :: EmitState
emptyState =
  EmitState
    { emitRecords = Map.empty
    , emitEnums = Map.empty
    }

handWrittenObjects :: [Text]
handWrittenObjects =
  ["SerializedValue", "SerializedArgument", "SerializedError"]

emitModule :: Spec -> Text
emitModule spec =
  let
    st :: EmitState
    st = execState (collectSpec spec) emptyState
  in
    Text.unlines
      ( moduleHeader spec st
          <> recordDecls (Map.elems (emitRecords st))
          <> enumDecls (Map.elems (emitEnums st))
          <> recordInstances (Map.elems (emitRecords st))
          <> enumInstances (Map.elems (emitEnums st))
          <> recordMks (Map.elems (emitRecords st))
      )

collectSpec :: Spec -> State EmitState ()
collectSpec spec = do
  mapM_ collectNamedEnum (Map.elems (specEnums spec))
  mapM_ (collectObject spec) (Map.elems (specObjects spec))
  mapM_ (collectInterface spec) (Map.elems (specInterfaces spec))

collectNamedEnum :: EnumDef -> State EmitState ()
collectNamedEnum def = do
  _ <- addEnum (typeNameText (enumName def)) (enumLiterals def)
  pure ()

collectObject :: Spec -> ObjectDef -> State EmitState ()
collectObject spec obj =
  if typeNameText (objectName obj) `elem` handWrittenObjects
    then pure ()
    else do
      _ <- recordFromFields spec (typeNameText (objectName obj)) (objectFields obj)
      pure ()

collectInterface :: Spec -> InterfaceDef -> State EmitState ()
collectInterface spec iface = do
  let
    base :: Text
    base = typeNameText (interfaceName iface)
  _ <- recordFromFields spec (base <> "Initializer") (interfaceInitializer iface)
  mapM_ (collectCommand spec base) (interfaceCommands iface)
  mapM_ (collectEvent spec base) (interfaceEvents iface)

collectCommand :: Spec -> Text -> CommandDef -> State EmitState ()
collectCommand spec base cmd = do
  let
    cmdPascal :: Text
    cmdPascal = pascalCase (jsonNameText (commandName cmd))
  _ <- recordFromFields spec (base <> cmdPascal <> "Params") (commandParams cmd)
  _ <- recordFromFields spec (base <> cmdPascal <> "Result") (commandReturns cmd)
  pure ()

collectEvent :: Spec -> Text -> EventDef -> State EmitState ()
collectEvent spec base ev = do
  let
    evPascal :: Text
    evPascal = pascalCase (jsonNameText (eventName ev))
  _ <- recordFromFields spec (base <> evPascal <> "Event") (eventParams ev)
  pure ()

recordFromFields :: Spec -> Text -> [(JsonName, Field)] -> State EmitState Text
recordFromFields spec recName fields = do
  recFields <- traverse (mkRecordField spec recName) fields
  addRecord
    RecordDef
      { recordName = recName
      , recordFields = recFields
      }

mkRecordField :: Spec -> Text -> (JsonName, Field) -> State EmitState RecordField
mkRecordField spec recName (jsonName, field) = do
  hsType <- typeToHs spec recName jsonName (fieldType field)
  let
    wrapped :: Text
    wrapped =
      if fieldOptional field
        then "Maybe " <> parenthesize hsType
        else hsType
  pure
    RecordField
      { recordFieldJson = jsonName
      , recordFieldHs = fieldName recName (jsonNameText jsonName)
      , recordFieldOptional = fieldOptional field
      , recordFieldHsType = wrapped
      }

parenthesize :: Text -> Text
parenthesize ty =
  if Text.any (== ' ') ty
    then "(" <> ty <> ")"
    else ty

typeToHs :: Spec -> Text -> JsonName -> ProtoType -> State EmitState Text
typeToHs spec parent jsonName ty =
  case ty of
    TString ->
      pure (stringType parent (jsonNameText jsonName))
    TInt ->
      pure "Int"
    TFloat ->
      pure (floatType (jsonNameText jsonName))
    TBool ->
      pure "Bool"
    TBinary ->
      pure "ProtocolBinary"
    TJson ->
      pure "OpenJson"
    TUndefined ->
      pure "()"
    TNamed name ->
      namedType spec name
    TArray inner -> do
      innerHs <- typeToHs spec parent jsonName inner
      pure ("[" <> innerHs <> "]")
    TEnum lits -> do
      let
        enumName :: Text
        enumName = parent <> pascalCase (jsonNameText jsonName)
      addEnum enumName lits
    TObject props -> do
      let
        recName :: Text
        recName = parent <> pascalCase (jsonNameText jsonName)
      recordFromFields spec recName props

stringType :: Text -> Text -> Text
stringType parent jsonName =
  if jsonName `elem` urlFields
    then "Url"
    else
      if isSelectorField parent jsonName
        then "Selector"
        else "Text"

urlFields :: [Text]
urlFields =
  ["url", "baseURL", "referer", "endpointURL"]

isSelectorField :: Text -> Text -> Bool
isSelectorField parent jsonName =
  jsonName `elem` ["selector", "resolvedSelector", "target"]
    || (jsonName == "source" && "DragAndDrop" `Text.isInfixOf` parent)

floatType :: Text -> Text
floatType jsonName =
  if jsonName == "timeout" || jsonName == "waitTimeout" || Text.isSuffixOf "Timeout" jsonName
    then "Timeout"
    else "Scientific"

namedType :: Spec -> TypeName -> State EmitState Text
namedType spec name =
  let
    textName :: Text
    textName = typeNameText name
  in
    if textName == "Channel"
      then pure "ChannelRef"
      else
        if Map.member name (specInterfaces spec)
          then pure "ChannelRef"
          else
            if textName `elem` handWrittenObjects
              then pure textName
              else pure textName

addRecord :: RecordDef -> State EmitState Text
addRecord rec = do
  modify
    ( \st ->
        st {emitRecords = Map.insert (recordName rec) rec (emitRecords st)}
    )
  pure (recordName rec)

addEnum :: Text -> [Text] -> State EmitState Text
addEnum name lits = do
  let
    generated :: GeneratedEnum
    generated =
      GeneratedEnum
        { generatedEnumName = name
        , generatedEnumConstructors =
            fmap (\lit -> (literalConstructor name lit, lit)) lits
        }
  modify
    ( \st ->
        st {emitEnums = Map.insert name generated (emitEnums st)}
    )
  pure name

moduleHeader :: Spec -> EmitState -> [Text]
moduleHeader spec st =
  [ "{-# LANGUAGE DerivingVia #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# LANGUAGE StrictData #-}"
  , ""
  , "module Playwright.Protocol"
  , "  ( ChannelType (..)"
  , "  , channelTypeToText"
  ]
    <> fmap (\n -> "  , " <> n <> " (..)") (Map.keys (emitRecords st))
    <> fmap (\n -> "  , mk" <> n) (Map.keys (emitRecords st))
    <> fmap (\n -> "  , " <> n <> " (..)") (Map.keys (emitEnums st))
    <> [ "  ) where"
       , ""
       , "import Autodocodec"
       , "  ( Autodocodec (Autodocodec)"
       , "  , HasCodec (codec)"
       , "  , object"
       , "  , optionalField'"
       , "  , requiredField'"
       , "  , stringConstCodec"
       , "  , (.=)"
       , "  )"
       , "import Data.Aeson"
       , "  ( FromJSON"
       , "  , ToJSON"
       , "  )"
       , "import Data.List.NonEmpty"
       , "  ( NonEmpty ((:|))"
       , "  )"
       , "import Data.Scientific"
       , "  ( Scientific"
       , "  )"
       , "import Data.Text"
       , "  ( Text"
       , "  )"
       , "import Playwright.Json.ChannelRef"
       , "  ( ChannelRef"
       , "  )"
       , "import Playwright.Json.OpenJson"
       , "  ( OpenJson"
       , "  )"
       , "import Playwright.Json.ProtocolBinary"
       , "  ( ProtocolBinary"
       , "  )"
       , "import Playwright.SerializedArgument"
       , "  ( SerializedArgument"
       , "  )"
       , "import Playwright.SerializedError"
       , "  ( SerializedError"
       , "  )"
       , "import Playwright.SerializedValue"
       , "  ( SerializedValue"
       , "  )"
       , "import Playwright.Selector"
       , "  ( Selector"
       , "  )"
       , "import Playwright.Timeout"
       , "  ( Timeout"
       , "  )"
       , "import Playwright.Url"
       , "  ( Url"
       , "  )"
       , ""
       ]
    <> channelTypeDecl spec

channelTypeDecl :: Spec -> [Text]
channelTypeDecl spec =
  let
    names :: [Text]
    names = fmap typeNameText (Map.keys (specInterfaces spec))
    pairs :: [(Text, Text)]
    pairs =
      case names of
        [] ->
          [("ChannelTypeNone", "None")]
        _ ->
          fmap (\n -> ("ChannelType" <> n, n)) names
    ctorLines :: [Text]
    ctorLines =
      case fmap fst pairs of
        [] ->
          ["  = ChannelTypeNone"]
        (firstCtor : rest) ->
          ("  = " <> firstCtor) : fmap ("  | " <>) rest
  in
    [ "data ChannelType"
    ]
      <> ctorLines
      <> [ "  deriving stock (Eq, Ord, Show)"
         , "  deriving (FromJSON, ToJSON) via (Autodocodec ChannelType)"
         , ""
         , "channelTypeToText :: ChannelType -> Text"
         , "channelTypeToText ty ="
         , "  case ty of"
         ]
      <> fmap (\(ctor, lit) -> "    " <> ctor <> " -> " <> Text.pack (show lit)) pairs
      <> [ ""
         , "instance HasCodec ChannelType where"
         , "  codec ="
         , "    stringConstCodec $"
         ]
      <> stringConstLines pairs
      <> [ ""
         ]

stringConstLines :: [(Text, Text)] -> [Text]
stringConstLines pairs =
  case pairs of
    [] ->
      []
    [(ctor, lit)] ->
      ["      (" <> ctor <> ", " <> Text.pack (show lit) <> ") :| []"]
    ((ctor, lit) : rest) ->
      [ "      (" <> ctor <> ", " <> Text.pack (show lit) <> ") :|"
      , "        ["
      ]
        <> stringConstRest rest
        <> ["        ]"]

stringConstRest :: [(Text, Text)] -> [Text]
stringConstRest pairs =
  case pairs of
    [] ->
      []
    ((ctor, lit) : rest) ->
      ("          (" <> ctor <> ", " <> Text.pack (show lit) <> ")")
        : fmap
          ( \(c, l) ->
              "        , (" <> c <> ", " <> Text.pack (show l) <> ")"
          )
          rest

recordDecls :: [RecordDef] -> [Text]
recordDecls = concatMap recordDecl

recordDecl :: RecordDef -> [Text]
recordDecl rec =
  case recordFields rec of
    [] ->
      [ "data " <> recordName rec <> " = " <> recordName rec <> " {}"
      , "  deriving stock (Eq, Show)"
      , "  deriving (FromJSON, ToJSON) via (Autodocodec " <> recordName rec <> ")"
      , ""
      ]
    fields ->
      [ "data " <> recordName rec <> " = " <> recordName rec
      , "  {"
      ]
        <> fieldLines fields
        <> [ "  }"
           , "  deriving stock (Eq, Show)"
           , "  deriving (FromJSON, ToJSON) via (Autodocodec " <> recordName rec <> ")"
           , ""
           ]

fieldLines :: [RecordField] -> [Text]
fieldLines fields =
  case fields of
    [] ->
      []
    (firstField : rest) ->
      ("    " <> recordFieldHs firstField <> " :: " <> recordFieldHsType firstField)
        : fmap (\f -> "  , " <> recordFieldHs f <> " :: " <> recordFieldHsType f) rest

enumDecls :: [GeneratedEnum] -> [Text]
enumDecls = concatMap enumDecl

enumDecl :: GeneratedEnum -> [Text]
enumDecl en =
  let
    ctors :: [Text]
    ctors = fmap fst (generatedEnumConstructors en)
    ctorLines :: [Text]
    ctorLines =
      case ctors of
        [] ->
          ["  = " <> generatedEnumName en <> "None"]
        (firstCtor : rest) ->
          ("  = " <> firstCtor) : fmap ("  | " <>) rest
  in
    ("data " <> generatedEnumName en)
      : ctorLines
      <> [ "  deriving stock (Eq, Ord, Show)"
         , "  deriving (FromJSON, ToJSON) via (Autodocodec " <> generatedEnumName en <> ")"
         , ""
         ]

recordInstances :: [RecordDef] -> [Text]
recordInstances = concatMap recordInstance

recordInstance :: RecordDef -> [Text]
recordInstance rec =
  [ "instance HasCodec " <> recordName rec <> " where"
  , "  codec ="
  ]
    <> recordCodecLines rec
    <> [""]

recordCodecLines :: RecordDef -> [Text]
recordCodecLines rec =
  case recordFields rec of
    [] ->
      ["    object " <> Text.pack (show (recordName rec)) <> " (pure " <> recordName rec <> ")"]
    fields ->
      ("    object " <> Text.pack (show (recordName rec)) <> " $")
        : ("      " <> recordName rec)
        : fieldCodecLines fields

fieldCodecLines :: [RecordField] -> [Text]
fieldCodecLines fields =
  case fields of
    [] ->
      []
    (firstField : rest) ->
      ("        <$> " <> fieldCodec firstField)
        : fmap (\f -> "        <*> " <> fieldCodec f) rest

fieldCodec :: RecordField -> Text
fieldCodec field =
  let
    combinator :: Text
    combinator =
      if recordFieldOptional field
        then "optionalField' "
        else "requiredField' "
  in
    combinator
      <> Text.pack (show (jsonNameText (recordFieldJson field)))
      <> " .= "
      <> recordFieldHs field

enumInstances :: [GeneratedEnum] -> [Text]
enumInstances = concatMap enumInstance

enumInstance :: GeneratedEnum -> [Text]
enumInstance en =
  let
    pairs :: [(Text, Text)]
    pairs =
      case generatedEnumConstructors en of
        [] ->
          [(generatedEnumName en <> "None", "none")]
        cs ->
          cs
  in
    [ "instance HasCodec " <> generatedEnumName en <> " where"
    , "  codec ="
    , "    stringConstCodec $"
    ]
      <> stringConstLines pairs
      <> [""]

recordMks :: [RecordDef] -> [Text]
recordMks = concatMap recordMk

recordMk :: RecordDef -> [Text]
recordMk rec =
  let
    fn :: Text
    fn = "mk" <> recordName rec
    required :: [RecordField]
    required = filter (not . recordFieldOptional) (recordFields rec)
    args :: [Text]
    args = fmap (\i -> "arg" <> Text.pack (show i)) [1 .. length required]
    sigTypes :: [Text]
    sigTypes = fmap recordFieldHsType required <> [recordName rec]
    signature :: Text
    signature = fn <> " :: " <> Text.intercalate " -> " sigTypes
    requiredAssign :: [(Text, RecordField)]
    requiredAssign = zip args required
    assign :: RecordField -> Text
    assign field =
      case lookup field [(f, a) | (a, f) <- requiredAssign] of
        Just arg ->
          recordFieldHs field <> " = " <> arg
        Nothing ->
          recordFieldHs field <> " = Nothing"
    body :: [Text]
    body =
      case recordFields rec of
        [] ->
          [fn <> " = " <> recordName rec <> " {}"]
        fields ->
          (fn <> (if null args then "" else " " <> Text.unwords args) <> " =")
            : ("  " <> recordName rec)
            : fieldAssignLines fields assign
  in
    [signature]
      <> body
      <> [""]

fieldAssignLines :: [RecordField] -> (RecordField -> Text) -> [Text]
fieldAssignLines fields assign =
  case fields of
    [] ->
      []
    (firstField : rest) ->
      ("    { " <> assign firstField)
        : fmap (\f -> "    , " <> assign f) rest
        ++ ["    }"]
