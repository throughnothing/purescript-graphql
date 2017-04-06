# purescript-graphql
[![Build Status](https://travis-ci.org/throughnothing/purescript-graphql.svg?branch=master)](https://travis-ci.org/throughnothing/purescript-graphql)

A graphQL library written in purescript.

_The status is currently WIP and very alpha.  See Status section for more info._

It is based off the Facebook [GraphQL Spec](https://facebook.github.io/graphql/#EnumValue)
and heavily influenced by  the
[graphql-haskell](https://github.com/jdnavarro/graphql-haskell/blob/master/Data/GraphQL/AST.hs)
implementation.

## Status

### Server / Infrastructure
- [X] Travis CI Builds
- [X] GraphQL AST
- [X] Document Parser
- [ ] Document Transformer (AST -> Simplified Version)
- [ ] Document/AST Printer
- [ ] Executor (Query)
- [ ] Executor (Mutation)
- [ ] Executor (Multiple Queries/Mutations)
- [ ] Improve type safety of Query -> Result
- [ ] Documentation
- [ ] Examples
- [ ] Release to Pursuit

### Client / Query Builder
- [ ] GraphQL Query Builder (type safe)
- [ ] GraphQL Client (w/Aff), server-side + browser-side support

### Utilities
- [ ] Schema AST (?)
- [ ] Schema Parser (?)
- [ ] Schema/AST Printer (?)
- [ ] Schema building / gen / introspection utilities

## Getting Started

* `git clone https://github.com/throughnothing/purescript-graphql`
* `npm install`
* `bower install`

## Building the library

* `npm run build`

## Running the tests

* `npm run test` or `pulp test`