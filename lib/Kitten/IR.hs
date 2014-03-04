{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kitten.IR
  ( Instruction(..)
  , Index
  , Label
  , Offset
  , Value(..)
  , ir
  ) where

import Control.Applicative hiding (some)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
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
import Kitten.Tree (Term)
import Kitten.Util.Monad
import Kitten.Util.Text (ToText(..), showText)

import qualified Kitten.Builtin as Builtin
import qualified Kitten.Type as Type

type Label = Int
type Offset = Int
type Index = Int

data Instruction
  = Act !Label !(Vector ClosedName)
  | Builtin !Builtin
  | Call !Label
  | Closure !Index
  | Comment !Text
  | Enter
  | EntryLabel
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
    EntryLabel -> ["\nentry"]
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

data Env = Env
  { envClosures :: [Vector Instruction]
  }

type IR a = ReaderT Int (State Env) a

ir
  :: Tree Typed
  -> Vector Instruction
ir Fragment{..}
  = collectClosures . withClosureOffset $ (<>)
    <$> irEntry fragmentTerms
    <*> concatMapM (uncurry irDef)
      (V.zip fragmentDefs (V.fromList [0..V.length fragmentDefs]))

  where
  closureOffset :: Int
  closureOffset = V.length fragmentDefs

  withClosureOffset
    :: IR a
    -> State Env a
  withClosureOffset = flip runReaderT closureOffset

  collectClosures
    :: State Env (Vector Instruction)
    -> Vector Instruction
  collectClosures action = let
    (instructions, Env{..}) = runState action Env { envClosures = [] }
    in instructions <> V.concatMap (uncurry collectClosure)
      (V.fromList (zip [closureOffset..] (reverse envClosures)))

  collectClosure
    :: Int
    -> Vector Instruction
    -> Vector Instruction
  collectClosure index instructions = V.concat
    [ V.singleton (Label index)
    , instructions
    , V.fromList [Return]
    ]

irDef
  :: TypedDef
  -> Int
  -> IR (Vector Instruction)
irDef Def{..} index = do
  instructions <- irTerm (Type.unScheme defTerm)
  return $ V.concat
    [ V.fromList [Comment defName, Label index]
    , instructions
    , V.fromList [Return]
    ]

irEntry :: Vector Typed -> IR (Vector Instruction)
irEntry terms = do
  instructions <- concatMapM irTerm terms
  return $ V.concat
    [ V.singleton EntryLabel
    , instructions
    , V.singleton Return
    ]

irTerm :: Typed -> IR (Vector Instruction)
irTerm term = case term of
  Typed.Builtin builtin _ _ -> return $ V.singleton (Builtin builtin)
  Typed.Call (Name index) _ _ -> return $ V.singleton (Call index)
  Typed.Compose terms _ _ -> concatMapM irTerm terms
  Typed.From{} -> return V.empty
  Typed.PairTerm a b _ _ -> do
    a' <- irTerm a
    b' <- irTerm b
    return $ a' <> b' <> V.singleton (Builtin Builtin.Pair)
  Typed.Push value _ _ -> irValue value
  Typed.Scoped terms _ _ -> do
    instructions <- irTerm terms
    return $ V.singleton Enter <> instructions <> V.singleton Leave
  Typed.To{} -> return V.empty
  Typed.VectorTerm values _ _ -> do
    values' <- concatMapM irTerm values
    return $ values' <> (V.singleton . MakeVector $ V.length values)

irValue
  :: Typed.Value
  -> IR (Vector Instruction)
irValue resolved = case resolved of
  Typed.Bool x -> value $ Bool x
  Typed.Char x -> value $ Char x
  Typed.Closed (Name index) -> return $ V.singleton (Closure index)
  Typed.Closure names terms -> do
    instructions <- irTerm terms
    index <- irClosure instructions
    return $ V.singleton (Act index names)
  Typed.Float x -> value $ Float x
  Typed.Int x -> value $ Int x
  Typed.Local (Name index) -> return $ V.singleton (Local index)
  Typed.Unit -> value Unit
  Typed.String x -> value $ String x
  where
  value :: Value -> IR (Vector Instruction)
  value = return . V.singleton . Push

irClosure :: Vector Instruction -> IR Label
irClosure terms = do
  closureOffset <- ask
  label <- lift . gets $ length . envClosures
  lift . modify $ \env@Env{..} -> env
    { envClosures = terms : envClosures }
  return $ label + closureOffset
