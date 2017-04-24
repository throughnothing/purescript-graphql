module GraphQL.Language.AST.Validated where

import GraphQL.Language.AST as A
import Data.List.Types (List)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)

data QueryDocument
  = LoneAnonymousQuery Operation
  | MultipleOperations Operations

type Operations = NonEmpty (Map A.Name) Operation

data Operation
  = Query    (NonEmpty List Field)
  | Mutation (NonEmpty List Field)

type Fields = List Field

data Field = Field (Maybe A.Alias) A.Name Arguments Fields

type Arguments = List Argument

data Argument = Argument A.Name Value

data Value
  = ValueInt Int
  | ValueFloat Number
  | ValueString String
  | ValueBoolean Boolean
  | ValueNull
  | ValueEnum A.Name
  | ValueList (List Value)
  | ValueObject (List ObjectField)

data ObjectField = ObjectField A.Name Value

