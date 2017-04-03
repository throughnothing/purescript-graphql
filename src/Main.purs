module Main where
import Prelude
import Text.Parsing.StringParser.Combinators as SC
import Text.Parsing.StringParser.String as S
import Control.Alternative (class Alternative, (<|>))
import Control.Applicative ((*>))
import Control.Apply ((<$), (<$>), (<*>))
import Control.Bind ((<*))
import Control.Monad (pure)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Either (Either(..))
import Data.List.Types (List)
import Data.Monoid (class Monoid, append, mempty)
import Data.NonEmpty ((:|), NonEmpty)
import Data.String (singleton)
import Data.Traversable (fold)
import Data.Unit (Unit)
import Prelude ((<>), map, (<<<))
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators ((<?>))
import GraphQL.Language.AST as GA
import GraphQL.Types as GT
import GraphQL.Language.Parser as P

main :: ∀ e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ show $ runParser swSimpleParser testDocument
  pure unit
  where
    bracesParser = (P.whiteSpace *> (P.braces P.name))
    testBraces = "  , \t\n  {test_123_ABC}\n   \t "

    operationTypeParser = (P.whiteSpace *> P.operationType)
    testOperationTypeQ = "query,"
    testOperationTypeM = ", mutation \t \r \n ,"

    argumentsParser = (P.whiteSpace *> P.arguments)
    testArguments = "(, id:4, test: lol)"

    argumentParser = (P.whiteSpace *> P.argument)
    testArgument = "(id : one)"

    swSimpleParser = P.whiteSpace *> P.document
    testDocument = swSimple2


swsimple :: String
swsimple = """
  {
  allUsers {
    users {
      id,
      name,
    },
  },
}"""

swSimple2 :: String
swSimple2 = """
query TTT {
  allPeople {
      people {
      	id
      	name
      }
  },
}"""

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