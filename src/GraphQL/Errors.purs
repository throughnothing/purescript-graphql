module GraphQL.Errors where

import Control.Plus (empty)
import Data.Either (Either(..))
import Data.NonEmpty ((:|))
import GraphQL.Types (Result(..))
import Prelude (class Monad, ($), pure)
import Text.Parsing.StringParser (ParseError(..))


-- TODO: Get rid of needing StringParser/ParseError in here
parseError :: ∀ m. Monad m => ParseError -> m Result
parseError (ParseError err) = pure $ Result $ Left $ err :| empty
