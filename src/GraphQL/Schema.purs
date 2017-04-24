module GraphQL.Schema where

import GraphQL.Language.AST as A
import Data.Either (Either)
import Data.List.Types (List)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)



type NEList a = NonEmpty List a
type DefaultValue = A.Value
type Interfaces = List InterfaceTypeDefinition
newtype ListType t = ListType (AnnotatedType t)


data Schema = Schema (Map A.Name TypeDefinition)

data TypeDefinition
  = TypeDefinitionObject        ObjectTypeDefinition
  | TypeDefinitionInterface     InterfaceTypeDefinition
  | TypeDefinitionUnion         UnionTypeDefinition
  | TypeDefinitionScalar        ScalarTypeDefinition
  | TypeDefinitionEnum          EnumTypeDefinition
  | TypeDefinitionInputObject   InputObjectTypeDefinition
  | TypeDefinitionTypeExtension TypeExtensionDefinition

data AnnotatedType t
  = TypeNamed t
  | TypeList (ListType t)
  | TypeNonNull (NonNullType t)

data NonNullType t
    = NonNullTypeNamed t
    | NonNullTypeList  (ListType t)

data Type = DefinedType TypeDefinition | BuiltinType Builtin

data InputType = DefinedInputType InputTypeDefinition | BuiltinInputType Builtin

data InputTypeDefinition
  = InputTypeDefinitionObject        InputObjectTypeDefinition
  | InputTypeDefinitionScalar        ScalarTypeDefinition
  | InputTypeDefinitionEnum          EnumTypeDefinition

data Builtin = GInt | GBool | GString | GFloat | GID

data ObjectTypeDefinition = ObjectTypeDefinition A.Name Interfaces (NEList A.Field)
data InterfaceTypeDefinition = InterfaceTypeDefinition A.Name (NEList A.Field)
data UnionTypeDefinition = UnionTypeDefinition A.Name (NEList ObjectTypeDefinition)
data ScalarTypeDefinition = ScalarTypeDefinition A.Name
data EnumTypeDefinition = EnumTypeDefinition A.Name (NEList EnumValueDefinition)
data EnumValueDefinition = EnumValueDefinition A.Name
data InputObjectTypeDefinition = InputObjectTypeDefinition A.Name (NEList InputObjectFieldDefinition)
data InputObjectFieldDefinition = InputObjectFieldDefinition A.Name (AnnotatedType InputType) (Maybe DefaultValue)

data TypeExtensionDefinition = TypeExtensionDefinition



