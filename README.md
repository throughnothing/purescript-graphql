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
- [X] GraphQL AST
- [X] Travis CI Builds
- [X] Document Parser
- [X] Comment support in Document parser
- [X] Document Transformer (AST -> Simplified Version)
- [ ] [Validate](https://facebook.github.io/graphql/#sec-Validation) Schema on Transformation
- [ ] Executor + Resolvers/Schema (Query)
- [ ] Document/AST Printer
- [ ] Executor (Mutation)
- [ ] Executor (Multiple Queries/Mutations)
- [ ] Variable Substitution in Querie
- [ ] Documentation
- [ ] Examples
- [ ] Release to Pursuit when useable
- [ ] Purescript 0.11 support (when ready)

## Getting Started

* `git clone https://github.com/throughnothing/purescript-graphql`
* `npm install`
* `bower install`

## Building the library

* `npm run build`

## Running the tests

* `npm run test` or `pulp test`
