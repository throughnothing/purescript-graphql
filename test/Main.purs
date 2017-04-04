module Test.Main where

import Prelude
import GraphQL.Language.Parser as P
import Text.Parsing.StringParser.String as S
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (runParser)

import Test.Documents as D

main :: ∀ e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ show $ runParser ((P.name <* P.but (S.string "!")) `P.tryAlt` S.string "te") "test!"

  log "selection:"
  log $ show $ runParser P.selection D.selection

  log "selectionSet:"
  log $ show $ runParser P.selectionSet D.selectionSet

  log "field:"
  log $ show $ runParser P.field D.fieldWithAlias
  log $ show $ runParser P.field D.field1
  log $ show $ runParser P.field D.field2
  log $ show $ runParser P.field D.field3
  log $ show $ runParser P.field D.field4

  log "arguments:"
  log $ show $ runParser P.arguments D.arguments1
  log $ show $ runParser P.arguments D.arguments2
  log $ show $ runParser ((\s a -> Tuple s a) <$> P.name <*> P.arguments) D.nameArguments

  log "argument:"
  log $ show $ runParser P.argument D.argument

  log "operationType:"
  log $ show $ runParser P.operationType D.operationTypeQuery
  log $ show $ runParser P.operationType D.operationTypeMutation

  log "variableDefinition:"
  log $ show $ runParser P.variableDefinition D.variableDefinition

  log "variableDefinitions:"
  log $ show $ runParser P.variableDefinitions D.variableDefinitions

  log "operationDefinition:"
  log $ show $ runParser P.operationDefinition D.operationDefinition

  log "document:"
  log $ show $ runParser P.document D.swsimple
  log $ show $ runParser P.document D.swSimple2
  log $ show $ runParser P.document D.kitchenSinkNoComments
  log $ show $ runParser P.document D.kitchenSink


