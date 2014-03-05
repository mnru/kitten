{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.IR
  ( Instruction(..)
  , Index
  , Label
  , Offset
  , Value(..)
  , irTerm
  ) where

import Control.Applicative hiding (some)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.HashMap (HashMap)
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)
import System.IO

import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Name
import Kitten.NameMap (NameMap)
import Kitten.Tree (Pass(..), Term)
import Kitten.Util.Monad
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.Tree as Tree
import qualified Kitten.Builtin as Builtin
import qualified Kitten.Type as Type

data Program = Program
  { programBlocks :: !(NameMap Block)
  , programNameGen :: !NameGen
  , programSymbols :: !(HashMap Text Name)
  }

type Label = Int
type Offset = Int
type Index = Int

type Block = Vector Instruction

data Instruction
  = Act !Label !(Vector ClosedName)
  | Builtin !Builtin
  | Call !Label
  | Closure !Index
  | Comment !Text
  | Enter
  | Leave
  | Label !Label
  | Local !Index
  | MakeVector !Int
  | Push !Value
  | Return

instance Show Instruction where
  show = T.unpack . toText

instance ToText Instruction where
  toText instruction = T.unwords $ case instruction of
    Act label names
      -> "act" : showText label : map showClosedName (V.toList names)
      where
      showClosedName :: ClosedName -> Text
      showClosedName (ClosedName (Name index)) = "local:" <> showText index
      showClosedName (ReclosedName (Name index)) = "closure:" <> showText index

    Builtin builtin -> ["builtin", toText builtin]
    Call label -> ["call", showText label]
    Closure index -> ["closure", showText index]
    Comment comment -> ["\n;", comment]
    Enter -> ["enter"]
    Leave -> ["leave"]
    Label label -> ["\nlabel", showText label]
    Local index -> ["local", showText index]
    MakeVector size -> ["vector", showText size]
    Push value -> ["push", toText value]
    Return -> ["ret"]

data Value
  = Bool !Bool
  | Char !Char
  | Choice !Bool !Value
  | Float !Double
  | Handle !Handle
  | Int !Int
  | Option !(Maybe Value)
  | Pair !Value !Value
  | Unit
  | String !Text

instance Show Value where
  show = T.unpack . toText

instance ToText Value where
  toText value = T.unwords $ case value of
    Bool bool -> ["bool", if bool then "1" else "0"]
    Char char -> ["char", showText (Char.ord char)]
    Choice which choice
      -> [if which then "right" else "left", toText choice]
    Float float -> ["float", showText float]

    -- FIXME Unnecessary?
    Handle handle -> (:[]) $ case () of
      _ | handle == stderr -> "handle 2"
        | handle == stdin -> "handle 0"
        | handle == stdout -> "handle 1"
        | otherwise -> "handle 0"

    Int int -> ["int", showText int]
    Option Nothing -> ["none"]
    Option (Just option) -> ["some", toText option]
    Pair a b -> ["pair", toText a, toText b]
    String string
      -> "vector"
      : showText (T.length string)
      : map (\char -> "char " <> showText (Char.ord char))
        (T.unpack string)
    Unit -> ["unit"]

data Env = Env { envClosures :: [Block] }

type IR a = State Env a

irTerm :: Term Typed -> IR Block
irTerm term = case term of
  Tree.Builtin builtin _ _ -> return $ V.singleton (Builtin builtin)
  Tree.Call (Name index) _ _ -> return $ V.singleton (Call index)
  Tree.Compose terms _ _ -> concatMapM irTerm terms
  Tree.From{} -> return V.empty
  Tree.PairTerm a b _ _ -> do
    a' <- irTerm a
    b' <- irTerm b
    return $ a' <> b' <> V.singleton (Builtin Builtin.Pair)
  Tree.Push value _ _ -> irValue value
  Tree.Scoped terms _ _ -> do
    instructions <- irTerm terms
    return $ V.singleton Enter <> instructions <> V.singleton Leave
  Tree.To{} -> return V.empty
  Tree.VectorTerm values _ _ -> do
    values' <- concatMapM irTerm values
    return $ values' <> (V.singleton . MakeVector $ V.length values)

irValue :: Tree.Value Typed -> IR Block
irValue resolved = case resolved of
  Tree.Bool x -> value $ Bool x
  Tree.Char x -> value $ Char x
  Tree.Closed (Name index) -> return $ V.singleton (Closure index)
  Tree.Closure names terms -> do
    instructions <- irTerm terms
    index <- irClosure instructions
    return $ V.singleton (Act index names)
  Tree.Float x -> value $ Float x
  Tree.Int x -> value $ Int x
  Tree.Local (Name index) -> return $ V.singleton (Local index)
  Tree.Unit -> value Unit
  Tree.String x -> value $ String x
  where
  value :: Value -> IR Block
  value = return . V.singleton . Push

irClosure :: Block -> IR Label
irClosure terms = do
  closureOffset <- ask
  label <- lift . gets $ length . envClosures
  lift . modify $ \env@Env{..} -> env
    { envClosures = terms : envClosures }
  return $ label + closureOffset
