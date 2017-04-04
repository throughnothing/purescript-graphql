{-
Parser based off the GraphQL Spec:
https://facebook.github.io/graphql/#EnumValue
Heavily influenced by graphql-haskell:
https://github.com/jdnavarro/graphql-haskell/blob/master/Data/GraphQL/AST.hs
-}
module GraphQL.Language.AST where

import Data.List.Types (List)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Show (class Show)
import Prelude (show, (<>))

type Name = String

type Document = NonEmpty List Definition

data Definition
  = DefinitionOperation OperationDefinition
  | DefinitionFragment FragmentDefinition

data OperationDefinition
  = OperationSelectionSet SelectionSet
  | OperationDefinition OperationType (Maybe Name) VariableDefinitions Directives SelectionSet

data OperationType = Query | Mutation

-- | SelectionSet

type SelectionSet = NonEmpty List Selection

type SelectionSetOpt = List Selection

data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment

-- | Field

data Field = Field (Maybe Alias) Name Arguments Directives SelectionSetOpt

type Alias = Name

-- | Arguments

type Arguments = List Argument

data Argument = Argument Name Value

-- | Fragments

data FragmentSpread = FragmentSpread Name Directives

data InlineFragment = InlineFragment (Maybe TypeCondition) Directives SelectionSet

data FragmentDefinition = FragmentDefinition FragmentName TypeCondition Directives SelectionSet

type FragmentName = Name

type TypeCondition = Name

-- | Input Values

data Value
  = ValueVariable Variable
  | ValueInt IntValue
  | ValueFloat FloatValue
  | ValueString StringValue
  | ValueBoolean BooleanValue
  | ValueNull
  | ValueEnum EnumValue
  | ValueList ListValue
  | ValueObject ObjectValue

type IntValue = Int

type FloatValue = Number

type StringValue = String

type BooleanValue = Boolean

type EnumValue = Name

type ListValue = List Value

type ObjectValue = List ObjectField

data ObjectField = ObjectField Name Value

-- | Variables

type VariableDefinitions = List VariableDefinition

data VariableDefinition = VariableDefinition Variable InputType (Maybe DefaultValue)

type Variable = Name

type DefaultValue = Value

-- | Input Types

data InputType
  = TypeNamed   Name
  | TypeList    InputType
  | TypeNonNull NonNullType

data NonNullType
  = NonNullTypeNamed Name
  | NonNullTypeList  InputType

-- | Directives

type Directives = List Directive
data Directive = Directive Name (List Argument)


-- | Instances

instance showOperationDefinition :: Show OperationDefinition where
  show (OperationSelectionSet s) = show s
  show (OperationDefinition o mn v d s) = "OperationDefinition"
                                       <> (show o) <> " "
                                       <> (show mn) <> " "
                                       <> (show d) <> (show s)

instance showDefinition :: Show Definition where
  show (DefinitionOperation o) = show o
  show (DefinitionFragment f) = show f


instance showInlineFragment :: Show InlineFragment where
  show (InlineFragment mt d s) = "InlineFragment("
                           <> (show mt) <> " "
                           <> (show d) <> " "
                           <> (show s) <> ")"


instance showFragmentSpread :: Show FragmentSpread where
  show (FragmentSpread n d) = "FragmentSPread("
                           <> (show n) <> " "
                           <> (show d) <> ")"

instance showSelection :: Show Selection where
  show (SelectionField f) = show f
  show (SelectionFragmentSpread f) = show f
  show (SelectionInlineFragment i) = show i

instance showFragmentDefinition :: Show FragmentDefinition where
  show (FragmentDefinition f t d s) = "FragmentDefinition("
                                   <> (show f) <> " "
                                   <> (show t) <> " "
                                   <> (show d) <> " "
                                   <> (show s) <> ")"

instance showField :: Show Field where
  show (Field ma n a d s) = "Field "
                         <> (show ma) <> " "
                         <> (show n) <> " "
                         <> (show a) <> " "
                         <> (show d) <> " "
                         <> (show s)


instance showArgument :: Show Argument where
  show (Argument n v) = "(" <> (show n) <> " => " <> (show v) <> ")"

instance showOperationType :: Show OperationType where
  show Query = "Query"
  show Mutation = "Mutation"

instance showValue :: Show Value where
  show (ValueVariable v) = show v
  show (ValueInt i) = show i
  show (ValueFloat f) = show f
  show (ValueString s) = show s
  show (ValueBoolean b) = show b
  show ValueNull = show "null"
  show (ValueEnum e) = show e
  show (ValueList l) = show l
  show (ValueObject o) = show o

instance showObjectField :: Show ObjectField where
  show (ObjectField n v) = (show n) <> ":" <> (show v)

instance showVariableDefinition :: Show VariableDefinition where
  show (VariableDefinition v i md) = "VariableDefinition(" <> (show v) <> (show i) <> (show md) <> ")"

instance showInputType :: Show InputType where
  show (TypeNamed n) = "TypedName(" <> (show n) <> ")"
  show (TypeList i) = "TypeList(" <> (show i) <> ")"
  show (TypeNonNull t) = "TypeNonNull(" <> (show t) <> ")"

instance showNonNullType :: Show NonNullType where
  show (NonNullTypeNamed n) = "NonNullTypedName(" <> (show n) <> ")"
  show (NonNullTypeList n) = "NonNullTypedList(" <> (show n) <> ")"

instance showDirective :: Show Directive where
  show (Directive n l) = "Directive("
                      <> (show n) <> " "
                      <> (show l) <> ")"
