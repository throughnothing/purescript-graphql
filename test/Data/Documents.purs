module Test.Data.Documents where

operationTypeQuery :: String
operationTypeQuery = "query,"

operationTypeMutation :: String
operationTypeMutation = ", mutation \t \r \n ,"

arguments1 :: String
arguments1 = " (, id:4, test: lol)"

arguments2 :: String
arguments2 = "(size:64), ,,,, \t\n "

nameArguments :: String
nameArguments = "name_Thing   (size:64, a : b,   c , : d), ,,,, \t\n "

argument :: String
argument = "id : one"

selection :: String
selection = """
  id
"""

selectionSet :: String
selectionSet = """
{
  id
  name
  address
  lookup(id:123)
}"""


fieldWithAlias :: String
fieldWithAlias = "smallPic: profilePic(size: 64)"

field1 :: String
field1 = "profilePic_ture(size:64)"

field2 :: String
field2 = "This_Other_thing123_(size:64 height:100, length: 54)"

field3 :: String
field3 = "empty_field_lol123"

field4 :: String
field4 = "selection_set_field(sizE: 23) { id }"

variableDefinition :: String
variableDefinition = "$foo: ComplexType"

variableDefinitions :: String
variableDefinitions = "($foo: ComplexType, $site: Site = MOBILE)"

operationDefinition :: String
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

