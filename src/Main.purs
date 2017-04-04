module Main where
import Prelude
import GraphQL.Language.AST as GA
import GraphQL.Language.Parser as P
import GraphQL.Types as GT
import Text.Parsing.StringParser.Combinators as SC
import Text.Parsing.StringParser.String as S
import Control.Alternative (class Alternative, (<|>))
import Control.Applicative ((*>))
import Control.Apply ((<$), (<$>), (<*>))
import Control.Bind ((<*))
import Control.Category (id)
import Control.Monad (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Either (Either(..))
import Data.List.Types (List)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, append, mempty)
import Data.NonEmpty ((:|), NonEmpty)
import Data.String (singleton)
import Data.Traversable (fold)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Prelude ((<>), map, (<<<))
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.Combinators ((<?>))

main :: ∀ e. Eff (console :: CONSOLE | e) Unit
main = do
  log "but:"
  log $ show $ runParser ((P.name <* P.but (S.string "!")) `P.tryAlt` S.string "te") "test!"

  log "selection:"
  log $ show $ runParser P.selection selection

  log "selectionSet:"
  log $ show $ runParser P.selectionSet selectionSet

  log "field:"
  log $ show $ runParser P.field aliasField
  log $ show $ runParser P.field field
  log $ show $ runParser P.field field2
  log $ show $ runParser P.field field3
  log $ show $ runParser P.field field4

  log "arguments:"
  log $ show $ runParser P.arguments arguments
  log $ show $ runParser ((\s a -> Tuple s a) <$> P.name <*> P.arguments) nameArguments

  log "argument:"
  log $ show $ runParser P.argument argument

  log "operationType:"
  log $ show $ runParser P.operationType testOperationTypeQ
  log $ show $ runParser P.operationType testOperationTypeM

  log "variableDefinition:"
  log $ show $ runParser P.variableDefinition variableDefinition

  log "variableDefinitions:"
  log $ show $ runParser P.variableDefinitions variableDefinitions

  log "operationDefinition:"
  log $ show $ runParser P.operationDefinition operationDefinition

  log "document:"
  log $ show $ runParser P.document swsimple
  log $ show $ runParser P.document swSimple2
  log $ show $ runParser P.document kitchenSinkNoComments
  log $ show $ runParser P.document kitchenSink

testOperationTypeQ = "query,"
testOperationTypeM = ", mutation \t \r \n ,"

-- arguments = " (, id:4, test: lol)"
arguments = "(size:64), ,,,, \t\n "

nameArguments = "name_Thing   (size:64, a : b,   c , : d), ,,,, \t\n "

argument = "id : one"

selection = """
  id
"""

selectionSet = """
{
  id
  name
  address
  lookup(id:123)
}"""


aliasField = "smallPic: profilePic(size: 64)"

field = "profilePic_ture(size:64)"
field2 = "This_Other_thing123_(size:64 height:100, length: 54)"
field3 = "empty_field_lol123"
field4 = "selection_set_field(sizE: 23) { id }"

variableDefinition = "$foo: ComplexType"

variableDefinitions = "($foo: ComplexType, $site: Site = MOBILE)"

operationDefinition = """
query queryName($foo: ComplexType, $site: Site = MOBILE) {
  user (id: [123, 456]) {
    id,
    ... on User @defer {
      name
      address
    }
  }
}

"""

swsimple :: String
swsimple = """
{
  id,
  name,
}"""

swSimple2 :: String
swSimple2 = """
query A {
  allPeople(id: 23, string: test) {
      people {
      	id( name: 10 )
      	name
      }
  },
}"""

kitchenSinkNoComments :: String
kitchenSinkNoComments = """
query queryName($foo: ComplexType, $site: Site = MOBILE) {
  whoever123is: node(id: [123, 456]) {
    id ,
    ... on User @defer {
      field2 {
        id ,
        alias: field1(first:10, after:$foo,) @include(if: $foo) {
          id,
          ...frag
        }
      }
    }
  }
}

mutation likeStory {
  like(story: 123) @defer {
    story {
      id
    }
  }
}

fragment frag on Friend {
  foo(size: $size, bar: $b, obj: {key: "value"})
}

{
  unnamed(truthy: true, falsey: false),
  query
}
"""

kitchenSink :: String
kitchenSink = """
# Copyright (c) 2015, Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree. An additional grant
# of patent rights can be found in the PATENTS file in the same directory.

query queryName($foo: ComplexType, $site: Site = MOBILE) {
  whoever123is: node(id: [123, 456]) {
    id , # Inline test comment
    ... on User @defer {
      field2 {
        id ,
        alias: field1(first:10, after:$foo,) @include(if: $foo) {
          id,
          ...frag
        }
      }
    }
  }
}

mutation likeStory {
  like(story: 123) @defer {
    story {
      id
    }
  }
}

fragment frag on Friend {
  foo(size: $size, bar: $b, obj: {key: "value"})
}

{
  unnamed(truthy: true, falsey: false),
  query
}
"""

