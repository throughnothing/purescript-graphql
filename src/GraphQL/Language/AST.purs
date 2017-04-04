module GraphQL.Language.AST where

import Data.Generic (class Generic, gShow)
import Data.List.Types (List)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Show (class Show)

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

derive instance genericDefinition :: Generic Definition
instance showDefinition :: Show Definition where show = gShow

derive instance genericFragmentDefinition :: Generic FragmentDefinition
instance showFragmentDefinition :: Show FragmentDefinition where show = gShow

derive instance genericOperationDefinition :: Generic OperationDefinition
instance showperationDefinition :: Show OperationDefinition where show = gShow

derive instance genericSelection :: Generic Selection
instance showSelection :: Show Selection where show = gShow

derive instance genericField :: Generic Field
instance showField :: Show Field where show = gShow

derive instance genericFragmentSPread :: Generic FragmentSpread
instance showFragmentSPread :: Show FragmentSpread where show = gShow

derive instance genericInlineFragment :: Generic InlineFragment
instance showInlineFragment :: Show InlineFragment where show = gShow

derive instance genericOperationType :: Generic OperationType
instance showOperationType :: Show OperationType where show = gShow

derive instance genericVariableDefinition :: Generic VariableDefinition
instance showVariableDefinition :: Show VariableDefinition where show = gShow

derive instance genericInputType :: Generic InputType
instance showInputType :: Show InputType where show = gShow

derive instance genericValue :: Generic Value
instance showValue :: Show Value where show = gShow

derive instance genericObjectField :: Generic ObjectField
instance showObjectField :: Show ObjectField where show = gShow

derive instance genericDirective :: Generic Directive
instance showDirective :: Show Directive where show = gShow

derive instance genericNonNullType :: Generic NonNullType
instance showNonNullType :: Show NonNullType where show = gShow

derive instance genericArgument :: Generic Argument
instance showArgument :: Show Argument where show = gShow
