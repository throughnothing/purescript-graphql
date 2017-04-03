module GraphQL.Execution
  ( execute
  ) where

import GraphQL.Types as GT
import Data.Either (Either(..))

execute :: ∀ f q m. GT.Schema q m -> GT.Document f -> Either GT.GraphQLError Int
execute _ _ = Right 1

