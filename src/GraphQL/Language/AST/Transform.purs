module GraphQL.Language.AST.Transform where

import GraphQL.Language.AST as A
import GraphQL.Language.AST.Simple as S
import Control.Bind (bind)
import Control.Monad ((=<<))
import Control.Plus (empty)
import Data.Array (foldr)
import Data.Either (Either(..), either)
import Data.Foldable (fold)
import Data.Function (flip)
import Data.Functor ((<$>))
import Data.List ((:))
import Data.List.Types (List(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.Semigroup (append)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Prelude (const, id, pure, ($), (<<<), (==), map)

type FragLookup = A.Name -> List S.Field

toSimple :: A.Document -> Maybe S.Document
toSimple (A.Document defs) = S.Document <$> ops fragOpsTup
  where
    ops :: Tuple (List FragLookup) (List A.OperationDefinition) -> Maybe S.Operations
    ops (Tuple fl os) = operations (fold fl) os

    fragOpsTup :: Tuple (List FragLookup) (List A.OperationDefinition)
    fragOpsTup = foldr accum (Tuple empty empty) (map getFragOrOp defs)

    accum :: ∀ a b. Either a b -> Tuple (List a) (List b) -> Tuple (List a) (List b)
    accum (Left x) (Tuple a b) = Tuple (Cons x a) b
    accum (Right x) (Tuple a b) = Tuple a (Cons x b)

operations :: FragLookup -> List A.OperationDefinition -> Maybe S.Operations
operations fr l = bind (traverse (operation fr) l) (toNE)
  where
    toNE :: ∀ a. List a -> Maybe (NonEmpty List a)
    toNE (Nil)    = Nothing
    toNE (a : as) = Just (a :| as)

operation :: FragLookup -> A.OperationDefinition -> Maybe S.Operation
operation f (A.OperationSelectionSet s) =
  operation f $ A.OperationDefinition A.Query empty empty empty s
operation f (A.OperationDefinition otype alias vars dirs sels) =
  case otype of
    A.Query    -> S.Query <$>traverse ((either (const Nothing) Just) <<< selection f) sels
    -- TODO: Mutations not supported yet
    A.Mutation -> Nothing

selection :: FragLookup -> A.Selection -> Either (List S.Field) S.Field
selection fr (A.SelectionField f) = Right $ field fr f
selection fr (A.SelectionFragmentSpread (A.FragmentSpread n _)) = Left $ fr n
-- TODO: Selection InlineFragment not yet supported
selection _ (A.SelectionInlineFragment _) = Left empty

field :: FragLookup -> A.Field -> S.Field
field fr (A.Field alias name args dirs sels) =
  -- TODO: Handle args properly
  S.Field alias name (fold $ traverse argument args) (foldr find empty sels)
  where
    find :: A.Selection -> List S.Field -> List S.Field
    find (A.SelectionFragmentSpread  (A.FragmentSpread n d)) fs = append (fr name) fs
    find sel fs = either id pure (selection fr sel)


getFragOrOp :: A.Definition -> Either FragLookup A.OperationDefinition
getFragOrOp (A.DefinitionOperation d) = Right d
getFragOrOp (A.DefinitionFragment f) = Left $ getFragment f

getFragment :: A.FragmentDefinition -> FragLookup
getFragment (A.FragmentDefinition n t d s) = \x -> matchName x
  where
    matchName :: String -> List S.Field
    matchName name' = if n == name'
      then either id pure =<< toList (selection mempty <$> s)
      else empty

argument :: A.Argument -> Maybe S.Argument
argument (A.Argument n v) = S.Argument n <$> value v

value :: A.Value -> Maybe S.Value
value (A.ValueVariable n) = Nothing -- TODO: Handle arg substitution
value (A.ValueInt      i) = pure $ S.ValueInt i
value (A.ValueFloat    f) = pure $ S.ValueFloat f
value (A.ValueString   x) = pure $ S.ValueString x
value (A.ValueBoolean  b) = pure $ S.ValueBoolean b
value  A.ValueNull        = pure $ S.ValueNull
value (A.ValueEnum     e) = pure $ S.ValueEnum e
value (A.ValueList     l) = S.ValueList <$> traverse value l
value (A.ValueObject   o) = S.ValueObject <$> traverse (objectField) o

objectField :: A.ObjectField -> Maybe S.ObjectField
objectField (A.ObjectField n v) = S.ObjectField n <$> value v

toList :: ∀ f a. NonEmpty f a -> f a
toList = fromNonEmpty $ flip const

