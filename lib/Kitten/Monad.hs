module Kitten.Monad
  ( Kitten
  , runKitten
  , declareBlock
  ) where

import Control.Monad.Trans.State
import Data.Text (Text)

import qualified Data.HashMap as HashMap

import Kitten.Name
import Kitten.Program

import qualified Kitten.NameMap as NameMap

newtype Kitten a = Kitten (StateT Program IO a)
  deriving (Applicative, Functor, Monad)

runKitten :: Kitten a -> IO a
runKitten (Kitten action) = runStateT action Program
  { programBlocks = NameMap.empty
  , programNameGen = mkNameGen
  , programSymbols = HashMap.empty
  }

declareBlock :: Maybe Text -> Block -> Kitten Name
declareBlock symbol block = do
  nameGen <- Kitten $ gets programNameGen
  let (name, nameGen') = genName nameGen
  modify $ \p -> p
    { programBlocks = NameMap.insert name block (programBlocks p)
    , programNameGen = nameGen'
    , programSymbols = maybe (`HashMap.insert` name) id
      symbol (programSymbols p)
    }
