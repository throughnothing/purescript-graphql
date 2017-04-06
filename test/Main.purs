module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.GraphQL.Parser (parserSpec)
import Test.GraphQL.Language.AST.Transform (transformSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
    parserSpec
    transformSpec



