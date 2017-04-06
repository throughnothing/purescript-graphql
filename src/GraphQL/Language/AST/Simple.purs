module GraphQL.Language.AST.Simple where

import GraphQL.Language.AST as A
import Data.Generic (class Generic, gShow)
import Data.List.Types (List)
import Data.Maybe (Maybe)
import Data.NonEmpty (NonEmpty)
import Data.Show (class Show)


data Document = Document Operations

type Operations = NonEmpty List Operation

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

derive instance genericDocument :: Generic Document
instance showSDocument :: Show Document where show = gShow

derive instance genericOperation :: Generic Operation
instance showSOperation :: Show Operation where show = gShow

derive instance genericField :: Generic Field
instance showSField :: Show Field where show = gShow

derive instance genericArgument :: Generic Argument
instance showSArgument :: Show Argument where show = gShow

derive instance genericValue :: Generic Value
instance showValue :: Show Value where show = gShow

derive instance genericObjectField :: Generic ObjectField
instance showObjectField :: Show ObjectField where show = gShow

