module GraphQL where

import Data.Either (either)
import GraphQL.Errors (parseError)
import GraphQL.Execution (execute)
import GraphQL.Language.Parser (parseDocument)
import GraphQL.Types (Result, Query, Schema)
import Prelude (class Monad)

graphql :: ∀ m. (Monad m) => Schema -> Query -> m Result
graphql s q = either parseError (execute s) (parseDocument q)
