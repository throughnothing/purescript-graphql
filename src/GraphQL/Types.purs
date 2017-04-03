module GraphQL.Types
  -- | Primitive GraphQL Types
  ( String
  , String'
  , Int
  , Int'
  , Float
  , Float'
  , Boolean
  , Boolean'
  , Id
  , Id'
  , List

  , Schema(..)
  , GraphQLError(..)
  , Document(..)

  -- | Type Helpers / Combinators
  , Field
  , Field'
  , Arg
  , NoArgs
  , AndE
  , type (<&>)


  ) where

-- | GraphQL Primitive Types
-- | Prime types (') are on-nullable
data String'
data String
data Int
data Int'
data Float
data Float'
data Boolean
data Boolean'
data Id
data Id'
data List a

-- | Represents a generic GraphQL Document.
-- | Which is a record that can have any fields/types.
-- | Every obect will have to implement fromDocument and/or toDocument
data Document f = Document { | f }

-- | GraphQL Field
data Field (f :: Symbol) args a

-- | GraphQL Field with noArgs
type Field' f a = Field f NoArgs a

-- | GraphQL Field Arg
data Arg (a :: Symbol) t

-- | Type that represents NoArgs for a GraphQL Field
data NoArgs

data AndE a b = AndE a b
infixl 4 type AndE as <&>

-- | Represents a GraphQL Schema, with a Query object and a Mutation object
data Schema query mutation = Schema query mutation

data GraphQLError
  = SyntaxError Prim.String
  | LocatedError Prim.String
  | FormatError Prim.String
