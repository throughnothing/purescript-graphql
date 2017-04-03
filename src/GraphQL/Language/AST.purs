module GraphQL.Language.AST where

import Data.Foldable (fold)
import Data.Function (($))
import Data.Generic (class Generic, gShow)
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

instance showDefinition :: Show Definition where
  show (DefinitionOperation o) = show o
  show (DefinitionFragment f) = show f

data OperationDefinition
  = OperationSelectionSet SelectionSet
  | OperationDefinition OperationType (Maybe Name) VariableDefinitions Directives SelectionSet

instance showOperationDefinition :: Show OperationDefinition where
  show (OperationSelectionSet s) = show s
  show (OperationDefinition o mn v d s) = "OperationDefinition"
                                       <> (show o) <> " "
                                       <> (show mn) <> " "
                                       <> (show d) <> (show s)

data OperationType = Query | Mutation

instance showOperationType :: Show OperationType where
  show Query = "Query"
  show Mutation = "Mutation"

-- * SelectionSet
type SelectionSet = NonEmpty List Selection

type SelectionSetOpt = List Selection

data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment

instance showSelection :: Show Selection where
  show (SelectionField f) = show f
  show (SelectionFragmentSpread f) = show f
  show (SelectionInlineFragment i) = show i

-- * Field

data Field = Field (Maybe Alias) Name Arguments Directives SelectionSetOpt

instance showField :: Show Field where
  show (Field ma n a d s) = "Field "
                         <> (show ma) <> " "
                         <> (show n) <> " "
                         <> (show d) <> " "
                         <> (show s)

type Alias = Name

-- * Arguments

type Arguments = List Argument

data Argument = Argument Name Value

instance showArgument :: Show Argument where
  show (Argument n v) = (show n) <> ":" <> (show v)

-- * Fragments
data FragmentSpread = FragmentSpread Name Directives

instance showFragmentSpread :: Show FragmentSpread where
  show (FragmentSpread n d) = "FragmentSPread("
                           <> (show n) <> " "
                           <> (show d) <> ")"

data InlineFragment = InlineFragment (Maybe TypeCondition) Directives SelectionSet

instance showInlineFragment :: Show InlineFragment where
  show (InlineFragment mt d s) = "InlineFragment("
                           <> (show mt) <> " "
                           <> (show d) <> " "
                           <> (show s) <> ")"

data FragmentDefinition = FragmentDefinition FragmentName TypeCondition Directives SelectionSet

instance showFragmentDefinition :: Show FragmentDefinition where
  show (FragmentDefinition f t d s) = "FragmentDefinition("
                                   <> (show f) <> " "
                                   <> (show t) <> " "
                                   <> (show d) <> " "
                                   <> (show s) <> ")"

type FragmentName = Name

type TypeCondition = Name

-- Input Values

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


type IntValue = Int

type FloatValue = Number

type StringValue = String

type BooleanValue = Boolean

type EnumValue = Name

type ListValue = List Value

type ObjectValue = List ObjectField

data ObjectField = ObjectField Name Value

instance showObjectField :: Show ObjectField where
  show (ObjectField n v) = (show n) <> ":" <> (show v)

-- * Variables

type VariableDefinitions = List VariableDefinition

data VariableDefinition = VariableDefinition Variable InputType (Maybe DefaultValue)

type Variable = Name

type DefaultValue = Value

-- * Input Types

data InputType
  = TypeNamed   Name
  | TypeList    InputType
  | TypeNonNull NonNullType

data NonNullType
  = NonNullTypeNamed Name
  | NonNullTypeList  InputType

-- * Directives
type Directives = List Directive
data Directive = Directive Name (List Argument)

instance showDirective :: Show Directive where
  show (Directive n l) = "Directive("
                      <> (show n) <> " "
                      <> (show l) <> ")"

