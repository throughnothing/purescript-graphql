module GraphQL.Validator where

import GraphQL.Language.AST as A
import GraphQL.Language.AST.Simple as S
import Data.Either (Either(Left, Right))
import Data.Foldable (foldMap)
import Data.Functor (class Functor)
import Data.List.Types (List(..))
import Data.Monoid (mempty)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (class Foldable)
import Data.Tuple (Tuple(..), fst)
import Prelude (pure, ($))


data ValidationError = ValidationError String

validate :: A.Document -> Either (NonEmpty List ValidationError) S.QueryDocument
validate (A.Document defs) = do
  let opsAndDefs   = splitBy splitDefs defs
  let anonAndNamed = splitBy splitOps (fst opsAndDefs)

  Left (ValidationError "lol" :| Nil)

  where
    splitBy :: ∀ a b c f. Functor f => Foldable f
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
