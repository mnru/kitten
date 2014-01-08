{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Kitten.Infer.Locations
  ( diagnosticLocations
  ) where

import Kitten.Location
import Kitten.Type
import Kitten.Util.Text

-- | A list of locations and associated types, suitable for
-- presenting to the end user for diagnostic purposes (e.g.
-- type errors).
diagnosticLocations :: (ToText (Type a)) => Type a -> [(Location, Text)]
diagnosticLocations type_ = case type_ of
  a :& b -> locations a ++ locations b
  a :. b -> locations a ++ locations b
  (:?) a -> locations a
  a :| b -> locations a ++ locations b
  Bool loc -> yield loc
  Char loc -> yield loc
  Const _ loc -> yield loc
  Empty loc -> yield loc
  Float loc -> yield loc
  Function r s loc -> yield loc ++ locations r ++ locations s
  Handle loc -> yield loc
  Int loc -> yield loc
  Named _ loc -> yield loc
  Quantified _ loc -> yield loc
  Unit loc -> yield loc
  Var _ loc -> yield loc
  Vector a loc -> yield loc ++ locationsIfUnhinted loc a

  where
  yield :: Origin -> [(Location, Text)]
  yield (Origin _ loc) = [(loc, toVerbose type_)]

  locations
    :: (ToText (Type a)) => Type a -> [(Location, Text)]
  locations = diagnosticLocations

  locationsIfUnhinted
    :: (ToText (Type a)) => Origin -> Type a -> [(Location, Text)]
  locationsIfUnhinted (Origin NoHint _) = locations
  locationsIfUnhinted (Origin _ _) = const []
