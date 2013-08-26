module Kitten.Def
  ( Def(..)
  ) where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Anno (Anno)
import Kitten.Location

data Def a = Def
  { defName :: !(Vector (Maybe Text))
  , defTerm :: !a
  , defAnno :: !(Maybe Anno)
  , defLocation :: !Location
  } deriving (Eq, Show)
