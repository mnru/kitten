{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Kitten.C
  ( toC
  ) where

import Control.Applicative
import Control.Monad.Trans.State
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Text as T
import qualified Data.Vector as V

import Kitten.Builtin (Builtin)
import Kitten.ClosedName
import Kitten.C.Quote
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
toC instructions = V.cons preamble
  $ evalState (V.mapM go instructions) Env
  { envNext = Name 0
  , envOffsets = N.empty
  }
  where
  preamble =
    "#include \"kitten.h\"\n\
    \int main(int argc, char** argv) {\n\
    \kitten_init();\n"

  go instruction = (<>) <$> advance <*> case instruction of

    Act label names -> return [qc|
      *kitten_data-- = kitten_retain(kitten_new_activation(
        &&#{ global label },
        #{ T.intercalate ", "
          $ showText (V.length names)
          : map closedName (V.toList names) }
      ));
    |]
      where
      closedName (ClosedName (Name index))
        = [qc|kitten_retain(kitten_locals[#{ showText index }])|]
      closedName (ReclosedName (Name index))
        = [qc|kitten_retain(*kitten_closure[#{ showText index }])|]

    Builtin builtin -> toCBuiltin builtin

    Call label -> do
      next <- newLabel 0
      return $
        "*kitten_return-- = (KittenReturn){\n\
        \  .address = &&" <> local next <> ", .closure = 0 };\n\
        \goto " <> global label <> ";"

    Closure index -> return $ T.concat
      [ "*kitten_data-- = kitten_retain(*kitten_closure["
      , showText index
      , "]);"
      ]

    Comment text -> return $ T.unwords ["/*", text, "*/"]

    EndDef -> return ""

    EndEntry -> return
      "return 0;\n\
      \}"

    Enter -> return "*kitten_locals-- = *kitten_data++;"

    EntryLabel -> return ""

    Jump offset -> do
      label <- newLabel offset
      return $ "goto " <> local label <> ";"

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
      return [qc|
        {
          KittenObject* top = *kitten_data++;
          int test = top->type == KITTEN_UNIT;
          if (test) {
            kitten_release(top);
            goto #{ local label };
          } else {
            *kitten_data-- = kitten_retain(top->as_box.value);
            kitten_release(top);
          }
        }
      |]

    JumpIfRight offset -> do
      label <- newLabel offset
      return [qc|
        {
          KittenObject* top = *kitten_data++;
          int test = top->type == KITTEN_RIGHT;
          *kitten_data-- = kitten_retain(top->as_box.value);
          if (test) {
            goto #{ local label };
          }
        }
      |]

    Leave -> return [qc|
      kitten_release(*kitten_locals);
      ++kitten_locals;
    |]

    Label label -> return [qc|
      #{ global label }:
    |]

    Local index -> return [qc|
      *kitten_data-- = kitten_retain(kitten_locals[#{ showText index }]);
    |]

    MakeVector _size -> return "// TODO compile vector"

    Push x -> return [qc|
      *kitten_data-- = kitten_retain(#{ toCValue x });
    |]

    Return -> return [qc|
      {
        KittenReturn call = *kitten_return++;
        if (call.closure) {
          for (KittenObject** p = *kitten_closure; *p; ++p) {
            kitten_release(*p);
          }
          ++kitten_closure;
        }
        goto *call.address;
      }
    |]

-- k :: Text -> [Text] -> Text
-- k f xs = [qc|kitten_#{ f }(#{ T.intercalate ", " xs })|]

toCValue :: Value -> Text
toCValue value = case value of
  Bool x -> toCValue $ Int (fromEnum x)
  Char x -> toCValue $ Int (fromEnum x)
  Choice False x -> [qc|kitten_new_left(#{ toCValue x })|]
  Choice True x -> [qc|kitten_new_right(#{ toCValue x })|]
  Float x -> [qc|kitten_new_float(#{ showText x })|]
  Int x
    | x >= 0 && x <= 127
    -> [qc|kitten_ints[#{ showText x }]|]
    | otherwise
    -> [qc|kitten_new_int(#{ showText x })|]
  Option Nothing -> "kitten_unit"
  Option (Just x) -> [qc|kitten_new_some(#{ toCValue x })|]
  Pair x y -> [qc|kitten_new_pair(#{ toCValue x }, #{ toCValue y })|]
  Unit -> "kitten_unit"
  _ -> error $ "Kitten.C.toCValue: TODO convert value " ++ show value

global :: Int -> Text
global label = "global" <> showText label

local :: Name -> Text
local (Name label) = "local" <> showText label

toCBuiltin :: Builtin -> State Env Text
toCBuiltin builtin = case builtin of
  Builtin.AddFloat -> binary "float" "+"
  Builtin.AddInt -> binary "int" "+"
  Builtin.AndBool -> relational "int" "&&"
  Builtin.AndInt -> binary "int" "&"

  Builtin.Apply -> do
    next <- newLabel 0
    return $
      "{\n\
      \  KittenObject* f = *kitten_data++;\n\
      \  const size_t size = f->as_activation.begin - f->as_activation.end;\n\
      \  *kitten_closure-- = calloc(size + 1, sizeof(KittenObject*));\n\
      \  for (size_t i = 0; i < size; ++i) {\n\
      \    *kitten_closure[i] = kitten_retain(f->as_activation.begin[i]);\n\
      \  }\n\
      \  *kitten_return-- = (KittenReturn){\n\
      \    .address = &&" <> local next <> ", .closure = 1 };\n\
      \  void* const function = f->as_activation.function;\n\
      \  kitten_release(f);\n\
      \  goto *function;\n\
      \}"

  Builtin.CharToInt -> return ""
  Builtin.Close -> return
    "fclose((*kitten_data)->as_handle.value);\n\
    \kitten_release(*kitten_data++);"

  Builtin.DivFloat -> binary "float" "/"
  Builtin.DivInt -> binary "int" "/"
  Builtin.EqFloat -> relational "float" "=="
  Builtin.EqInt -> relational "int" "=="
  Builtin.Exit -> return "exit((*kitten_data)->as_int.value);"
  Builtin.GeFloat -> relational "float" ">="
  Builtin.GeInt -> relational "int" ">="
  Builtin.GtFloat -> relational "float" ">"
  Builtin.GtInt -> relational "int" ">"
  Builtin.LeFloat -> relational "float" "<="
  Builtin.LeInt -> relational "int" "<="
  Builtin.LtFloat -> relational "float" "<"
  Builtin.LtInt -> relational "int" "<"
  Builtin.ModInt -> binary "int" "%"
  Builtin.MulFloat -> binary "float" "*"
  Builtin.MulInt -> binary "int" "*"
  Builtin.NeFloat -> relational "float" "!="
  Builtin.NeInt -> relational "int" "!="
  Builtin.NegFloat -> unary "float" "-"
  Builtin.NegInt -> unary "int" "-"
  Builtin.NotBool -> unary "int" "!"
  Builtin.NotInt -> unary "int" "~"
  Builtin.OrBool -> relational "int" "||"
  Builtin.OrInt -> binary "int" "|"
  Builtin.SubFloat -> binary "float" "-"
  Builtin.SubInt -> binary "int" "-"

  _ -> return $ "// TODO compile builtin " <> toText builtin

  {-
  Builtin.AddVector
  Builtin.First
  Builtin.FromLeft
  Builtin.FromRight
  Builtin.FromSome
  Builtin.Get
  Builtin.GetLine
  Builtin.Impure
  Builtin.Init
  Builtin.IntToChar
  Builtin.Left
  Builtin.Length
  Builtin.ModFloat
  Builtin.None
  Builtin.OpenIn
  Builtin.OpenOut
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
  Builtin.Tail
  Builtin.UnsafePurify11
  Builtin.XorBool
  Builtin.XorInt
  -}

  where

  -- a a -> a
  binary type_ operation = return $
    "{\n\
    \  KittenObject* b = *kitten_data++;\n\
    \  if ((*kitten_data)->refcount == 1) {\n\
    \    (*kitten_data)->as_" <> type_ <> ".value "
      <> operation <> "= b->as_" <> type_ <> ".value;\n\
    \  } else {\n\
    \    KittenObject* a = *kitten_data++;\n\
    \    *kitten_data-- = kitten_retain(kitten_new_" <> type_ <> "(\n\
    \      a->as_" <> type_ <> ".value "
      <> operation <> " b->as_" <> type_ <> ".value));\n\
    \    kitten_release(a);\n\
    \  }\n\
    \  kitten_release(b);\n\
    \}"

  -- a -> a
  unary type_ operation = return $
    "{\n\
    \  if ((*kitten_data)->refcount == 1) {\n\
    \    (*kitten_data)->as_" <> type_ <> ".value = "
      <> operation <> "(*kitten_data)->as_" <> type_ <> ".value;\n\
    \  } else {\n\
    \    KittenObject* a = *kitten_data++;\n\
    \    *kitten_data-- = kitten_retain(kitten_new_" <> type_ <> "(\n\
    \      " <> operation <> "a->as_" <> type_ <> ".value));\n\
    \    kitten_release(a);\n\
    \  }\n\
    \}"

  -- a a -> Bool
  relational type_ operation = return $
    "{\n\
    \  KittenObject* b = *kitten_data++;\n\
    \  KittenObject* a = *kitten_data++;\n\
    \  *kitten_data-- = kitten_retain(kitten_new_int(\n\
    \      a->as_" <> type_ <> ".value "
      <> operation <> " b->as_" <> type_ <> ".value));\n\
    \  kitten_release(a);\n\
    \  kitten_release(b);\n\
    \}"
