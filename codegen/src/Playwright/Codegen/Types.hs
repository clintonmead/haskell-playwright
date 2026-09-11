module Playwright.Codegen.Types
  ( TypeName (..)
  , JsonName (..)
  , ProtoType (..)
  , Field (..)
  , InterfaceDef (..)
  , CommandDef (..)
  , EventDef (..)
  , ObjectDef (..)
  , EnumDef (..)
  , MixinDef (..)
  , Spec (..)
  , RecordDef (..)
  , RecordField (..)
  , GeneratedEnum (..)
  , interfaceNames
  ) where

import Data.Map.Strict
  ( Map
  )
import Data.Map.Strict qualified as Map
import Data.Text
  ( Text
  )

newtype TypeName = TypeName
  { typeNameText :: Text
  }
  deriving stock (Eq, Ord, Show)

newtype JsonName = JsonName
  { jsonNameText :: Text
  }
  deriving stock (Eq, Ord, Show)

data ProtoType
  = TString
  | TInt
  | TFloat
  | TBool
  | TBinary
  | TJson
  | TUndefined
  | TNamed TypeName
  | TArray ProtoType
  | TEnum [Text]
  | TObject [(JsonName, Field)]
  deriving stock (Eq, Show)

data Field = Field
  { fieldOptional :: Bool
  , fieldType :: ProtoType
  }
  deriving stock (Eq, Show)

data InterfaceDef = InterfaceDef
  { interfaceName :: TypeName
  , interfaceExtends :: Maybe TypeName
  , interfaceInitializer :: [(JsonName, Field)]
  , interfaceCommands :: [CommandDef]
  , interfaceEvents :: [EventDef]
  }
  deriving stock (Eq, Show)

data CommandDef = CommandDef
  { commandName :: JsonName
  , commandParams :: [(JsonName, Field)]
  , commandReturns :: [(JsonName, Field)]
  }
  deriving stock (Eq, Show)

data EventDef = EventDef
  { eventName :: JsonName
  , eventParams :: [(JsonName, Field)]
  }
  deriving stock (Eq, Show)

data ObjectDef = ObjectDef
  { objectName :: TypeName
  , objectFields :: [(JsonName, Field)]
  }
  deriving stock (Eq, Show)

data EnumDef = EnumDef
  { enumName :: TypeName
  , enumLiterals :: [Text]
  }
  deriving stock (Eq, Show)

data MixinDef = MixinDef
  { mixinName :: TypeName
  , mixinFields :: [(JsonName, Field)]
  }
  deriving stock (Eq, Show)

data Spec = Spec
  { specInterfaces :: Map TypeName InterfaceDef
  , specObjects :: Map TypeName ObjectDef
  , specEnums :: Map TypeName EnumDef
  , specMixins :: Map TypeName MixinDef
  }
  deriving stock (Eq, Show)

data RecordField = RecordField
  { recordFieldJson :: JsonName
  , recordFieldHs :: Text
  , recordFieldOptional :: Bool
  , recordFieldHsType :: Text
  }
  deriving stock (Eq, Show)

data RecordDef = RecordDef
  { recordName :: Text
  , recordFields :: [RecordField]
  }
  deriving stock (Eq, Show)

data GeneratedEnum = GeneratedEnum
  { generatedEnumName :: Text
  , generatedEnumConstructors :: [(Text, Text)]
  }
  deriving stock (Eq, Show)

interfaceNames :: Spec -> [TypeName]
interfaceNames spec =
  Map.keys (specInterfaces spec)
