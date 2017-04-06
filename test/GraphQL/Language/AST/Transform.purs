module Test.GraphQL.Language.AST.Transform where

import Prelude
import GraphQL.Language.AST as AST
import GraphQL.Language.AST.Transform as T
import GraphQL.Parser as P
import Test.Data.Documents as D
import Control.Monad.Aff (Aff)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Runner (RunnerEffects)
import Text.Parsing.StringParser (ParseError, Parser, runParser)

canTransform :: forall e. Either ParseError AST.Document -> Aff ( | e) Unit
canTransform (Left s)  = fail (show s)

canTransform (Right d) = shouldEqual (didParse (T.toSimple d)) true
  where
    didParse Nothing  = false
    didParse (Just _) = true

parse :: forall a. Parser a -> String -> Either ParseError a
parse p input = runParser p input

transformSpec :: ∀ e. Spec (RunnerEffects e) Unit
transformSpec = do
  describe "GraphQL.Language.AST.Transform" do
    it "transforms swsimple AST " do
      canTransform $ parse P.document D.swSimple
      canTransform $ parse P.document D.swSimple2
    it "transforms full KitchenSink example" do
      -- TODO
      -- canTransform $ parse P.document D.kitchenSink
      pure unit
    pending "variable subs not supported yet"

