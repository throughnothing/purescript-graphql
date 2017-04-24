module GraphQL.Validator where

import GraphQL.Language.AST as A
import GraphQL.Language.AST.Validated as V
import GraphQL.Schema as S
import Data.Either (Either(Left, Right))
import Data.Foldable (foldMap)
import Data.Functor (class Functor)
import Data.List.Types (List(..))
import Data.Map (Map)
import Data.Monoid (mempty)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (class Foldable)
import Data.Tuple (Tuple(..), fst)
import Prelude (pure, ($))


data ValidationError = ValidationError String
data Validation a = Validation (Either (NonEmpty List ValidationError) a)

-- Deliberately not going to do:
--
--   * field selections on compound types <https://facebook.github.io/graphql/#sec-Field-Selections-on-Objects-Interfaces-and-Unions-Types>
--   * leaf field selections <https://facebook.github.io/graphql/#sec-Leaf-Field-Selections>
--   * argument names <https://facebook.github.io/graphql/#sec-Argument-Names>
--   * argument value type correctness <https://facebook.github.io/graphql/#sec-Argument-Values-Type-Correctness>
--   * fragment spread type existence <https://facebook.github.io/graphql/#sec-Fragment-Spread-Type-Existence>
--   * fragments on compound types <https://facebook.github.io/graphql/#sec-Fragments-On-Composite-Types>
--   * fragment spread is possible <https://facebook.github.io/graphql/#sec-Fragment-spread-is-possible>
--   * directives are defined <https://facebook.github.io/graphql/#sec-Directives-Are-Defined>
--   * directives are in valid locations <https://facebook.github.io/graphql/#sec-Directives-Are-In-Valid-Locations>
--   * variable default values are correctly typed <https://facebook.github.io/graphql/#sec-Variable-Default-Values-Are-Correctly-Typed>
--   * variables are input types <https://facebook.github.io/graphql/#sec-Variables-Are-Input-Types>
--   * all variable usages are allowed <https://facebook.github.io/graphql/#sec-All-Variable-Usages-are-Allowed>
--
-- Because all of the above rely on type checking.
validate :: S.Schema -> A.Document -> Either (NonEmpty List ValidationError) V.QueryDocument
validate schema (A.Document defs) = do
  let opsAndDefs   = splitBy splitDefs defs
  let anonAndNamed = splitBy splitOps (fst opsAndDefs)

  -- TODO
  Left (ValidationError "lol" :| Nil)

  where
    splitBy :: ∀ a b c f
             . Functor f
            => Foldable f
            => (a -> List (Either b c)) -> f a -> Tuple (List b) (List c)
    splitBy f l = partitionEithers $ foldMap f l

    partitionEithers :: ∀ a b. List (Either a b) -> Tuple (List a) (List b)
    partitionEithers = foldMap part
      where
      part (Left x)  = Tuple (pure x)  mempty
      part (Right x) = Tuple mempty (pure x)

    splitDefs :: A.Definition -> List (Either A.OperationDefinition A.FragmentDefinition)
    splitDefs (A.DefinitionOperation op) = pure (Left op)
    splitDefs (A.DefinitionFragment frag) = pure (Right frag)

    splitOps :: A.OperationDefinition -> List (Either A.SelectionSet A.OperationDefinition)
    splitOps (A.OperationSelectionSet s) = pure (Left s)
    splitOps def@(A.OperationDefinition _ _ _ _ _) = pure (Right def)



-- | Validate a type condition that appears in a query.
validateTypeCondition :: S.Schema -> A.TypeCondition -> Validation S.TypeDefinition
-- TODO
validateTypeCondition schema typeCond = Validation (Right S.TypeDefinition)

