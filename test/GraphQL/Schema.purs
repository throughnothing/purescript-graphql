module Test.GraphQL.Schema where
import Data.Maybe (Maybe)
data ArgAnd a b = ArgAnd a b
infixr 8 type ArgAnd as :>

data And a b = AndField a b
infixl 4 type And as <&>


data Object (name :: Symbol) (interfaces :: Type) (fields :: Type)
data Enum (name :: Symbol) (values :: Type)
data Union (name :: Symbol) (types :: Type)
data List (elemType :: Type)

data Interface (name :: Symbol) (fields :: Type)
data Field (name :: Symbol) (fieldType :: Type)
data Argument (name :: Symbol) (argType :: Type)

data DefaultArgument (name :: Symbol) (argType :: Type)


-- | Sample Schema from Spec

data DogCommand = Sit | Down | Heel


type Pet = Interface "Pet" (Field "name" String)

type Dog =  Object "Dog" (Pet)
  (   Field "name" String
  <&> Field "nickname" String
  <&> Field "barkVolume" String
  <&> Argument "dogCommand" (Enum "DogCommand" DogCommand) :> Field "doesKnowCommand" Boolean
  <&> Argument "atOtherHomes" (Maybe Boolean) :> Field "isHouseTrained" Boolean
  <&> Field "owner" Human
  <&> Field "barkVolume" String
  )


type Sentient = Interface "Sentient" (Field "name" String)

type Alien = Object "Alien" (Sentient)
  (   Field "name" String
  <&> Field "homePlanet" (Maybe String)
  )


type Human = Object "Human" (Sentient) (Field "name" String)
