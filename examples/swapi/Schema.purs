module SWAPI.Schema
  ( Person
  )where

import GraphQL.Types (type (<&>))
import GraphQL.Types as GT

type Person
  =   GT.Field' "id" GT.String'
  <&> GT.Field' "name" GT.String'
  <&> GT.Field' "birthYear" GT.String'
  <&> GT.Field' "eyeColor" GT.String'
  <&> GT.Field' "gender" GT.String'
  <&> GT.Field' "hairColor" GT.String'
  <&> GT.Field' "height" GT.Int'
  <&> GT.Field' "mass" GT.Int'
  <&> GT.Field' "skinColor" GT.String'


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


