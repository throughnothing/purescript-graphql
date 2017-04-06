module Test.GraphQL.Parser where

import Prelude
import GraphQL.Parser as P
import Test.Data.Documents as D
import Control.Monad.Aff (Aff)
import Data.Either (isRight)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)
import Text.Parsing.StringParser (runParser, Parser)

canParse :: forall a e. Parser a -> String -> Aff ( | e) Unit
canParse p input = shouldEqual (isRight $ runParser p input) true

parserSpec :: ∀ e. Spec (RunnerEffects e) Unit
parserSpec = do
  describe "GraphQL.Parser" do
    it "parses selection" do canParse P.selection D.selection
    it "parses selectionSet" do canParse P.selectionSet D.selectionSet

    it "parses fields" do
      canParse P.field D.fieldWithAlias
      canParse P.field D.field1
      canParse P.field D.field2
      canParse P.field D.field3
      canParse P.field D.field4

    it "parses arguments" do
      canParse P.arguments D.arguments1
      canParse P.arguments D.arguments2
      canParse ((\s a -> Tuple s a) <$> P.name <*> P.arguments) D.nameArguments
      canParse P.argument D.argument

    it "parses operationType" do
      canParse P.operationType D.operationTypeQuery
      canParse P.operationType D.operationTypeMutation

    it "parses variableDefinition" do
      canParse P.variableDefinition D.variableDefinition
      canParse P.variableDefinitions D.variableDefinitions

    it "parses operationDefinition" do
      canParse P.operationDefinition D.operationDefinition

    it "parses simple documents" do
      canParse P.document D.swSimple
      canParse P.document D.swSimple2

    it "parses KitchenSink with no commetns" do
      canParse P.document D.kitchenSinkNoComments

    it "parses full KitchenSink example" do
      -- TODO
      -- canParse P.document D.kitchenSink
      pure unit
    pending "comments supported"

