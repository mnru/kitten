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
  preamble = [qc|
    #include "kitten.h"
    int main(int argc, char** argv) {
    kitten_init();
    goto entry;
  |]

  go instruction = (<>) <$> advance <*> case instruction of

    Act label names -> return [qc|
      *++kitten_data = kitten_retain(kitten_new_activation(
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
        = [qc|kitten_retain((*kitten_closure)[#{ showText index }])|]

    Builtin builtin -> toCBuiltin builtin

    Call label -> do
      next <- newLabel 0
      return $
        "*++kitten_return = (KittenReturn){\n\
        \  .address = &&" <> local next <> ", .closure = 0 };\n\
        \goto " <> global label <> ";"

    Closure index -> return $ T.concat
      [ "*++kitten_data = kitten_retain((*kitten_closure)["
      , showText index
      , "]);"
      ]

    Comment text -> return $ T.unwords ["/*", text, "*/"]

    EndDef -> return ""

    EndEntry -> return
      "return 0;\n\
      \}"

    Enter -> return "*++kitten_locals = *kitten_data--;"

    EntryLabel -> return "entry:"

    Jump offset -> do
      label <- newLabel offset
      return $ "goto " <> local label <> ";"

    JumpIfFalse offset -> do
      label <- newLabel offset
      return $
        "{\n\
        \  KittenObject* top = *kitten_data--;\n\
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
          KittenObject* top = *kitten_data--;
          int test = top->type == KITTEN_UNIT;
          if (test) {
            kitten_release(top);
            goto #{ local label };
          } else {
            *++kitten_data = kitten_retain(top->as_box.value);
            kitten_release(top);
          }
        }
      |]

    JumpIfRight offset -> do
      label <- newLabel offset
      return [qc|
        {
          KittenObject* top = *kitten_data--;
          int test = top->type == KITTEN_RIGHT;
          *++kitten_data = kitten_retain(top->as_box.value);
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
      *++kitten_data = kitten_retain(kitten_locals[#{ showText index }]);
    |]

    MakeVector size -> return [qc|
      {
        KittenObject* vector = kitten_make_vector(#{ showText size });
        *++kitten_data = kitten_retain(vector);
      }
    |]

    Push x -> return [qc|
      *++kitten_data = kitten_retain(#{ toCValue x });
    |]

    Return -> return [qc|
      {
        KittenReturn call = *kitten_return--;
        if (call.closure) {
          for (KittenObject** p = *kitten_closure; *p; ++p) {
            kitten_release(*p);
          }
          ++kitten_closure;
        }
        goto *call.address;
      }
    |]

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
    return [qc|
      {
        KittenObject* f = *kitten_data--;
        const size_t size = f->as_activation.begin - f->as_activation.end;
        *++kitten_closure = calloc(size + 1, sizeof(KittenObject*));
        for (size_t i = 0; i < size; ++i) {
          (*kitten_closure)[i] = kitten_retain(f->as_activation.begin[i]);
        }
        *++kitten_return = (KittenReturn){
          .address = &&#{ local next }, .closure = 1 };
        void* const function = f->as_activation.function;
        kitten_release(f);
        goto *function;
      }
    |]

  Builtin.CharToInt -> return "// __char_to_int"

  Builtin.Close -> return [qc|
    fclose((*kitten_data)->as_handle.value);
    kitten_release(*kitten_data--);
  |]

  Builtin.DivFloat -> binary "float" "/"
  Builtin.DivInt -> binary "int" "/"
  Builtin.EqFloat -> relational "float" "=="
  Builtin.EqInt -> relational "int" "=="

  Builtin.Exit -> return [qc|
    exit((*kitten_data)->as_int.value);
  |]

  Builtin.GeFloat -> relational "float" ">="
  Builtin.GeInt -> relational "int" ">="
  Builtin.GtFloat -> relational "float" ">"
  Builtin.GtInt -> relational "int" ">"

  Builtin.First -> return [qc|
    {
      KittenObject* a = *kitten_data;
      *kitten_data = a->as_pair.first;
      a->as_pair.first = NULL;
      kitten_release(a);
    }
  |]

  Builtin.FromLeft -> fromBox
  Builtin.FromRight -> fromBox
  Builtin.FromSome -> fromBox

  Builtin.IntToChar -> return "// __int_to_char"
  Builtin.LeFloat -> relational "float" "<="
  Builtin.LeInt -> relational "int" "<="

  Builtin.Left -> return [qc|
    *kitten_data = kitten_retain(kitten_new_left(*kitten_data));
  |]

  Builtin.Length -> return [qc|
    {
      KittenObject* a = *kitten_data;
      *kitten_data = kitten_retain(kitten_new_int(
        a->as_vector.end - a->as_vector.begin));
      kitten_release(a);
    }
  |]

  Builtin.LtFloat -> relational "float" "<"
  Builtin.LtInt -> relational "int" "<"
  Builtin.ModInt -> binary "int" "%"
  Builtin.MulFloat -> binary "float" "*"
  Builtin.MulInt -> binary "int" "*"
  Builtin.NeFloat -> relational "float" "!="
  Builtin.NeInt -> relational "int" "!="
  Builtin.NegFloat -> unary "float" "-"
  Builtin.NegInt -> unary "int" "-"

  Builtin.None -> return [qc|
    *++kitten_data = kitten_retain(kitten_unit);
  |]

  Builtin.NotBool -> unary "int" "!"
  Builtin.NotInt -> unary "int" "~"
  Builtin.OrBool -> relational "int" "||"
  Builtin.OrInt -> binary "int" "|"

  Builtin.Pair -> return [qc|
    {
      KittenObject* b = *kitten_data--;
      KittenObject* a = *kitten_data--;
      *++kitten_data = kitten_retain(kitten_new_pair(a, b));
    }
  |]

  Builtin.Rest -> return [qc|
    {
      KittenObject* a = *kitten_data;
      *kitten_data = a->as_pair.rest;
      a->as_pair.rest = NULL;
      kitten_release(a);
    }
  |]

  Builtin.Right -> return [qc|
    *kitten_data = kitten_retain(kitten_new_right(*kitten_data));
  |]

  Builtin.Some -> return [qc|
    *kitten_data = kitten_retain(kitten_new_some(*kitten_data));
  |]

  Builtin.Stderr -> return [qc|
    *++kitten_data = kitten_retain(kitten_stderr);
  |]

  Builtin.Stdin -> return [qc|
    *++kitten_data = kitten_retain(kitten_stdin);
  |]

  Builtin.Stdout -> return [qc|
    *++kitten_data = kitten_retain(kitten_stdout);
  |]

  Builtin.SubFloat -> binary "float" "-"
  Builtin.SubInt -> binary "int" "-"

  Builtin.UnsafePurify11 -> return "// __unsafe_purify11"

  Builtin.XorBool -> relational "int" "!="
  Builtin.XorInt -> relational "int" "^"

  _ -> return [qc|
    assert(!"TODO compile builtin #{ toText builtin }");
  |]

  {-
  Builtin.AddVector
  Builtin.Get
  Builtin.GetLine
  Builtin.Impure
  Builtin.Init
  Builtin.ModFloat
  Builtin.OpenIn
  Builtin.OpenOut
  Builtin.Print
  Builtin.Set
  Builtin.ShowFloat
  Builtin.ShowInt
  Builtin.Tail
  -}

  where

  -- a a -> a
  binary type_ operation = return [qc|
    {
      KittenObject* b = *kitten_data--;
      if ((*kitten_data)->refcount == 1) {
        (*kitten_data)->as_#{ type_ }.value
          #{ operation }= b->as_#{ type_ }.value;
      } else {
        KittenObject* a = *kitten_data--;
        *++kitten_data = kitten_retain(kitten_new_#{ type_ }(
          a->as_#{ type_ }.value #{ operation } b->as_#{ type_ }.value));
        kitten_release(a);
      }
      kitten_release(b);
    }
  |]

  fromBox = return [qc|
    {
      KittenObject* a = *kitten_data;
      *kitten_data = a->as_box.value;
      a->as_box.value = NULL;
      kitten_release(a);
    }
  |]

  -- a a -> Bool
  relational type_ operation = return [qc|
    {
      KittenObject* b = *kitten_data--;
      KittenObject* a = *kitten_data--;
      *++kitten_data = kitten_retain(kitten_new_int(
        a->as_#{ type_ }.value
        #{ operation } b->as_#{ type_ }.value));
      kitten_release(a);
      kitten_release(b);
    }
  |]

  -- a -> a
  unary type_ operation = return [qc|
    {
      if ((*kitten_data)->refcount == 1) {
        (*kitten_data)->as_#{ type_ }.value =
          #{ operation }(*kitten_data)->as_#{ type_ }.value;
      } else {
        KittenObject* a = *kitten_data--;
        *++kitten_data = kitten_retain(kitten_new_#{ type_ }(
          #{ operation }a->as_#{ type_ }.value));
        kitten_release(a);
      }
    }
  |]
