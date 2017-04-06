# purescript-graphql
[![Build Status](https://travis-ci.org/throughnothing/purescript-graphql.svg?branch=master)](https://travis-ci.org/throughnothing/purescript-graphql)

A graphQL library written in purescript.

_The status is currently WIP and very alpha.  See Status section for more info._

It is based off the Facebook [GraphQL Spec](https://facebook.github.io/graphql/#EnumValue)
and heavily influenced by  the
[graphql-haskell](https://github.com/jdnavarro/graphql-haskell/blob/master/Data/GraphQL/AST.hs)
implementation.

## Goals

The `purescript-graphql` project aims to be a fast, type safe, easy-to-use,
implementation of the GraphQL spec in Purescript that can provide both a
server and a client for purescript applications.  At some point, various parts
of this projcet may be broken out into separate client/server, or other libraries.

## Status

### General
- [X] Travis CI Builds
- [ ] Purescript 0.11 support (when ready)
- [ ] Release to Pursuit when useable

### Server / Infrastructure
- [X] GraphQL AST
- [X] Document Parser
- [ ] Comment support in Document parser
- [ ] Document Transformer (AST -> Simplified Version)
- [ ] Document/AST Printer
- [ ] Variable Substitution (currently not supported)
- [ ] Executor (Query)
- [ ] Executor (Mutation)
- [ ] Executor (Multiple Queries/Mutations)
- [ ] Improve type safety of Query -> Result
- [ ] Documentation
- [ ] Examples

### Client / Query Builder
- [ ] GraphQL Query Builder (type safe)
- [ ] GraphQL Client (w/Aff), server-side + browser-side support

### Utilities / Other / Misc
- [ ] Support Directives?
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