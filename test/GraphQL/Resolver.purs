module Test.GraphQL.Resolver where

import Prelude
import Control.Monad.Eff (Eff)



-- Types: https://facebook.github.io/graphql/#sec-Types
-- Scalars, Objects, Interfaces, Unions, Enums, (NonNulls)

data Object (name :: Symbol) interfaces fields
data List elemType

-- TODO: Implement Interface
data Interface (name :: Symbol) fields
data Field (name :: Symbol) fieldType
data Argument (name :: Symbol) argType

data NoArguments
data NoInterfaces

data Comp a b = Comp a b
infixl 9 type Comp as :>

type Person = Object "person" NoArguments (Field "id" String :> Field "name" String)

data Handler m r = Handler m r

person :: ∀ m. Monad m =>  Handler m Person
person = pure (\who -> pure ("Person: " <> who))

-- runPerson :: ∀ e a. String -> Eff e Response
-- runPerson = person

-- interpretAnonymousQuery :: Handler m api -> String -> m Response
-- interpretAnonymousQuery h q =


resolverTests :: ∀ e. Eff e Unit
resolverTests = do
  pure unit




