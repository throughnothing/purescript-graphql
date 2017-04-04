module Main where
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


