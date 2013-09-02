{-# LANGUAGE OverloadedStrings #-}

module Kitten.C
  ( toC
  ) where

import Control.Applicative
import Control.Monad.Trans.State
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.Name
import Kitten.NameMap (NameMap)
import Kitten.Util.List
import Kitten.Util.Text (showText, toText)
import Kitten.Yarn

import qualified Kitten.Builtin as Builtin
import qualified Kitten.NameMap as N

data Env = Env
  { envNext :: Name
  , envOffsets :: NameMap Int
  }

newLabel :: Int -> State Env Name
newLabel offset = do
  current <- gets envNext
  modify $ \ e -> e
    { envNext = succ (envNext e)
    , envOffsets = N.insert current offset (envOffsets e)
    }
  return current

advance :: State Env Text
advance = do
  offsets <- fmap N.toList $ gets envOffsets
  let (labels, remaining) = foldr go ([], []) offsets
  modify $ \ e -> e { envOffsets = N.fromList remaining }
  return . T.unlines . for labels
    $ \ (Name label) -> "local" <> showText label <> ":"
  where
  go (name, 0) (names, remaining)
    = (name : names, remaining)
  go (name, offset) (names, remaining)
    = (names, (name, pred offset) : remaining)

toC :: Vector Instruction -> Vector Text
toC instructions = preamble
  <> evalState (V.mapM go instructions) Env
  { envNext = Name 0
  , envOffsets = N.empty
  }
  where
  preamble = V.fromList
    $ "#include \"kitten.h\""
    : mapMaybe (\ instruction -> case instruction of
      Label label -> Just $ "void " <> global label <> "(void);"
      _ -> Nothing)
      (V.toList instructions)

  go instruction = (<>) <$> advance <*> case instruction of

    Act label names -> return $ T.concat
      [ "*kitten_data-- = "
      , k "retain"
        [ k "new_activation"
          $ global label
          : showText (V.length names)
          : map closedName (V.toList names)
        ]
      , ";"
      ]
      where
      closedName (ClosedName (Name index))
        = "kitten_locals[" <> showText index <> "]"
      closedName (ReclosedName (Name index))
        = "kitten_closure[" <> showText index <> "]"

    Builtin builtin -> return $ toCBuiltin builtin

    Call label
      -> return $ global label <> "();"

    Closure index -> return $ T.concat
      [ "*kitten_data-- = kitten_retain(kitten_closure["
      , showText index
      , "]);"
      ]

    Comment text -> return $ T.unwords ["/*", text, "*/"]

    EndDef -> return "}"

    EndEntry -> return
      "return 0;\n\
      \}"

    Enter -> return "*kitten_locals-- = *kitten_data++;"

    EntryLabel -> return
      "int main(int argc, char** argv) {\n\
      \kitten_init();"

    Jump offset -> do
      label <- newLabel offset
      return $ T.concat ["goto ", local label, ";"]

    JumpIfFalse offset -> do
      label <- newLabel offset
      return $
        "{\n\
        \  KittenObject* top = *kitten_data++;\n\
        \  int test = top->as_int.value;\n\
        \  kitten_release(top);\n\
        \  if (!test) {\n\
        \    goto " <> local label <> ";\n\
        \  }\n\
        \}"

    JumpIfNone offset -> do
      label <- newLabel offset
      return $
        "{\n\
        \  KittenObject* top = *kitten_data++;\n\
        \  int test = top->type == KITTEN_UNIT;\n\
        \  if (test) {\n\
        \    kitten_release(top);\n\
        \    goto " <> local label <> ";\n\
        \  } else {\n\
        \    *kitten_data-- = kitten_retain(top->as_box.value);\n\
        \    kitten_release(top);\n\
        \  }\n\
        \}"

    JumpIfRight offset -> do
      label <- newLabel offset
      return $
        "{\n\
        \  KittenObject* top = *kitten_data++;\n\
        \  int test = top->type == KITTEN_RIGHT;\n\
        \  *kitten_data-- = kitten_retain(top->as_box.value);\n\
        \  if (test) {\n\
        \    goto " <> local label <> ";\n\
        \  }\n\
        \}"

    Leave -> return
      "kitten_release(*kitten_locals);\n\
      \++kitten_locals;"

    Label label -> return
      $ "void " <> global label <> "(void) {"

    Local index -> return $ T.concat
      ["*kitten_data-- = kitten_retain(kitten_locals[", showText index, "]);"]

    MakeVector _size -> return "// TODO compile vector"

    Push x -> return $ T.concat
      ["*kitten_data-- = kitten_retain(", toCValue x, ");"]

    Return -> return "return;"

k :: Text -> [Text] -> Text
k f xs = T.concat ["kitten_", f, "(", T.intercalate ", " xs, ")"]

toCValue :: Value -> Text
toCValue value = case value of
  Bool x -> toCValue $ Int (fromEnum x)
  Char x -> toCValue $ Int (fromEnum x)
  Choice False x -> k "new_left" [toCValue x]
  Choice True x -> k "new_right" [toCValue x]
  Float x -> k "new_float" [showText x]
  Int x
    | x >= 0 && x <= 127
    -> T.concat ["kitten_ints[", showText x, "]"]
    | otherwise
    -> k "new_int" [showText x]
  Option Nothing -> "kitten_unit"
  Option (Just x) -> k "new_some" [toCValue x]
  Pair x y -> k "new_pair" [toCValue x, toCValue y]
  Unit -> "kitten_unit"
  _ -> error $ "Kitten.C.toCValue: TODO convert value " ++ show value

global :: Int -> Text
global label = "global" <> showText label

local :: Name -> Text
local (Name label) = "local" <> showText label

toCBuiltin :: Builtin -> Text
toCBuiltin builtin = case builtin of

  Builtin.AddFloat ->
    "{\n\
    \  KittenObject* b = *kitten_data++;\n\
    \  if ((*kitten_data)->refcount == 1) {\n\
    \    (*kitten_data)->as_float.value += b->as_float.value;\n\
    \  } else {\n\
    \    KittenObject* a = *kitten_data++;\n\
    \    *kitten_data-- = kitten_retain(kitten_new_float(\n\
    \      a->as_float.value + b->as_float.value));\n\
    \    kitten_release(a);\n\
    \  }\n\
    \  kitten_release(b);\n\
    \}"

  _ -> "// TODO compile builtin " <> toText builtin

  {-
  Builtin.AddInt
  Builtin.AddVector
  Builtin.AndBool
  Builtin.AndInt
  Builtin.Apply
  Builtin.CharToInt
  Builtin.Close
  Builtin.DivFloat
  Builtin.DivInt
  Builtin.EqFloat
  Builtin.EqInt
  Builtin.Exit
  Builtin.First
  Builtin.FromLeft
  Builtin.FromRight
  Builtin.FromSome
  Builtin.GeFloat
  Builtin.GeInt
  Builtin.Get
  Builtin.GetLine
  Builtin.GtFloat
  Builtin.GtInt
  Builtin.Impure
  Builtin.Init
  Builtin.IntToChar
  Builtin.LeFloat
  Builtin.LeInt
  Builtin.Left
  Builtin.Length
  Builtin.LtFloat
  Builtin.LtInt
  Builtin.ModFloat
  Builtin.ModInt
  Builtin.MulFloat
  Builtin.MulInt
  Builtin.NeFloat
  Builtin.NeInt
  Builtin.NegFloat
  Builtin.NegInt
  Builtin.None
  Builtin.NotBool
  Builtin.NotInt
  Builtin.OpenIn
  Builtin.OpenOut
  Builtin.OrBool
  Builtin.OrInt
  Builtin.Pair
  Builtin.Print
  Builtin.Rest
  Builtin.Right
  Builtin.Set
  Builtin.ShowFloat
  Builtin.ShowInt
  Builtin.Some
  Builtin.Stderr
  Builtin.Stdin
  Builtin.Stdout
  Builtin.SubFloat
  Builtin.SubInt
  Builtin.Tail
  Builtin.UnsafePurify11
  Builtin.XorBool
  Builtin.XorInt
  -}
