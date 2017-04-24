module SWAPI.Schema where

import GraphQL.Schema as S

data ArgAnd a b = Args a b
infixr 8 type ArgAnd as :>

data And a b = And a b
infixl 8 type And as <&>

type Person
  =   S.Field' "id" S.String'
  <&> S.Field' "name" S.String'
  <&> S.Field' "birthYear" S.String'
  <&> S.Field' "eyeColor" S.String'
  <&> S.Field' "gender" S.String'
  <&> S.Field' "hairColor" S.String'
  <&> S.Field' "height" S.Int'
  <&> S.Field' "mass" S.Int'
  <&> S.Field' "skinColor" S.String'


  -- TODO: How to handle these
--   <&> GT.Field' "homeworld" Planet
--   <&> GT.Field' "species" Species
--   <&> GT.Field' "starshipConnection" (GT.List Starship)
--   <&> GT.Field' "vehicleConnection" (GT.List Vehicle)

-- type Planet = String
-- type Species = String
-- type Starship = String
-- type Vehicle = String


-- type Query
--   =   GT.Field "user" (GT.Arg "id" GT.Id' <|> GT.Arg "name" GT.String') User


