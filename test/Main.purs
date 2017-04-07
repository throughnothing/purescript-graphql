module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.GraphQL.Parser (parserSpec)
import Test.GraphQL.Language.AST.Transform (transformSpec)
import Test.GraphQL.Resolver (resolverTests)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do

  -- Resolver
  resolverTests

  run [consoleReporter] do
    parserSpec
    transformSpec



