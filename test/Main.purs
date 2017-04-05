module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Control.Plus (empty)
import Data.Either (Either(..))
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import GraphQL (graphql)
import GraphQL.Language.AST (Field(..))
import GraphQL.Types (Resolver(..), Result(..), Schema(..), Query(..))
import Test.GraphQL.Parser (parserSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

field :: Field
field = Field Nothing "name" Nil Nil Nil

resolver :: Resolver
resolver = Resolver \f -> Result $ Left ("Error" :| empty)

schema :: Schema
schema = Schema (resolver :| empty)

query :: Query
query = Query "{ user(id: 12){ name } }"

main :: Eff (RunnerEffects ()) Unit
main = do

  r <- graphql schema query
  log $ "R: " <> (show r)

  run [consoleReporter] do
    parserSpec



