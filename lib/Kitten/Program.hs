module Kitten.Program
  ( Program(..)
  ) where

import Data.HashMap (HashMap)
import Data.Text (Text)

import Kitten.IR

data Program = Program
  { programBlocks :: !(Vector BasicBlock)
  , programBlockNames :: !(HashMap Text Int)
  }
