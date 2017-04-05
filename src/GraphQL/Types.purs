module GraphQL.Types where

import Data.Either (Either)
import Data.Generic (class Generic, gShow)
import Data.List.Types (List)
import Data.NonEmpty (NonEmpty)
import Data.Show (class Show)
import GraphQL.Language.AST (Field)

type NEList a = NonEmpty List a

data Schema = Schema (NEList Resolver)

data Query = Query String

data Resolver = Resolver (Field -> Result)

data Result = Result (Either (NEList String) (NEList String))

derive instance genericResult :: Generic Result
instance showResult :: Show Result where show = gShow
