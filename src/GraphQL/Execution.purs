module GraphQL.Execution where

import Control.Monad (class Monad)
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.NonEmpty ((:|))
import GraphQL.Language.AST (Document)
import GraphQL.Language.AST.Transform (toSimple)
import GraphQL.Types (Schema, Result(..))
import Prelude (pure, show, ($), (<<<))

execute :: ∀ m. (Monad m) => Schema -> Document -> m Result
execute s d = pure $ Result (Right ((transform d) :| empty))
  where
    transform = show <<< toSimple

