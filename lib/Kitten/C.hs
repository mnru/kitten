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
toC instructions = V.concat
  [ V.singleton begin
  , evalState (V.mapM go instructions) Env
    { envNext = Name 0
    , envOffsets = N.empty
    }
  , V.singleton end
  ]

  where
  begin = [qc|
    #include "kitten.h"
    int main(int argc, char** argv) {
      k_init();
      *++k_return = (KR){ .address = &&exit, .closure = 0 };
      goto entry;
  |]

  end = [qc|
    exit:
      return 0;
    }
  |]

  go instruction = (<>) <$> advance <*> case instruction of

    Act label names -> return [qc|
      k_push_data(k_retain(k_activation(
        &&#{ global label },
        #{ T.intercalate ", "
          $ showText (V.length names)
          : map closedName (V.toList names) }
      )));
    |]
      where
      closedName (ClosedName (Name index))
        = [qc|k_retain(k_get_local(#{ showText index }))|]
      closedName (ReclosedName (Name index))
        = [qc|k_retain(k_get_closure(#{ showText index }))|]

    Builtin builtin -> toCBuiltin builtin

    Call label -> do
      next <- newLabel 0
      return [qc|
        *++k_return = (KR){ .address = &&#{ local next }, .closure = 0 };
        goto #{ global label };
      |]

    Closure index -> return [qc|
      k_push_data(k_retain(k_get_closure(#{ showText index })));
    |]

    Comment text -> return $ T.unwords ["/*", text, "*/"]

    Enter -> return [qc|
      k_push_locals(k_pop_data());
    |]

    EntryLabel -> return "entry:"

    Jump offset -> do
      label <- newLabel offset
      return $ "goto " <> local label <> ";"

    JumpIfFalse offset -> do
      label <- newLabel offset
      return [qc|
        if (!k_pop_data().data)
          goto #{ local label };
      |]

    JumpIfNone offset -> do
      label <- newLabel offset
      return [qc|
        {
          KObject top = k_pop_data();
          if (top.type == K_NONE)
            goto #{ local label };
          else
            k_push_data(*((KObject*)top.data));
        }
      |]

    JumpIfRight offset -> do
      label <- newLabel offset
      return [qc|
        {
          KObject top = k_pop_data();
          k_push_data(*((KObject*)top.data));
          if (top.type == K_RIGHT)
            goto #{ local label };
        }
      |]

    Leave -> return [qc|
      k_release(k_get_local(0));
      k_drop_locals();
    |]

    Label label -> return [qc|
      #{ global label }:
    |]

    Local index -> return [qc|
      k_push_data(k_get_local(#{ showText index }));
    |]

    MakeVector size -> return [qc|
      {
        const KObject vector = k_make_vector(#{ showText size });
        k_push_data(vector);
      }
    |]

    Push x -> return [qc|
      k_push_data(#{ toCValue x });
    |]

    Return -> return [qc|
      {
        KR call = k_pop_return();
        if (call.closure) {
          // TODO Release closure.
          --k_closure;
        }
        goto *call.address;
      }
    |]

toCValue :: Value -> Text
toCValue value = case value of
  Bool x -> toCValue $ Int (fromEnum x)
  Char x -> toCValue $ Int (fromEnum x)
  Choice False x -> [qc|k_left(#{ toCValue x })|]
  Choice True x -> [qc|k_right(#{ toCValue x })|]
  Float x -> [qc|k_float(#{ showText x })|]
  Int x -> [qc|k_int(#{ showText x })|]
  Option Nothing -> "k_none()"
  Option (Just x) -> [qc|k_some(#{ toCValue x })|]
  Pair x y -> [qc|k_pair(#{ toCValue x }, #{ toCValue y })|]
  Unit -> "k_unit()"
  _ -> error $ "Kitten.C.toCValue: TODO convert value " ++ show value

global :: Int -> Text
global label = "global" <> showText label

local :: Name -> Text
local (Name label) = "local" <> showText label

toCBuiltin :: Builtin -> State Env Text
toCBuiltin builtin = case builtin of
  Builtin.AddFloat -> binary "float" "+"
  Builtin.AddInt -> binary "int" "+"

  -- TODO
  {-
  Builtin.AddVector -> return [qc|
    {
      KObject* b = *k_data--;
      KObject* a = *k_data--;
      *++k_data = k_append_vector(a, b);
      k_release(b);
      k_release(a);
    }
  |]
  -}

  Builtin.AndBool -> relational "int" "&&"
  Builtin.AndInt -> binary "int" "&"

  -- TODO
  {-
  Builtin.Apply -> do
    next <- newLabel 0
    return [qc|
      {
        KObject* f = k_pop_data();
        const size_t size = f->as_activation.begin - f->as_activation.end;
        *++k_closure = calloc(size + 1, sizeof(KObject*));
        for (size_t i = 0; i < size; ++i) {
          (*k_closure)[i] = k_retain(f->as_activation.begin[i]);
        }
        *++k_return = (KR){ .address = &&#{ local next }, .closure = 1 };
        void* const function = f->as_activation.function;
        k_release(f);
        goto *function;
      }
    |]
  -}

  Builtin.CharToInt -> return "// __char_to_int"

  Builtin.Close -> return [qc|
    fclose((FILE*)k_data[0].data);
    k_release(k_data[0]);
  |]

  Builtin.DivFloat -> binary "float" "/"
  Builtin.DivInt -> binary "int" "/"
  Builtin.EqFloat -> relational "float" "=="
  Builtin.EqInt -> relational "int" "=="

  Builtin.Exit -> return [qc|
    exit(k_data[0].data);
  |]

  Builtin.GeFloat -> relational "float" ">="
  Builtin.GeInt -> relational "int" ">="
  Builtin.GtFloat -> relational "float" ">"
  Builtin.GtInt -> relational "int" ">"

  Builtin.First -> return [qc|
    {
      KObject a = k_data[0];
      k_data[0] = k_retain(((KPair*)a.data)->first);
      k_release(a);
    }
  |]

  Builtin.FromLeft -> fromBox
  Builtin.FromRight -> fromBox
  Builtin.FromSome -> fromBox

  Builtin.Get -> return [qc|
    {
      KObject index = k_pop_data();
      KObject vector = k_pop_data();
      k_push_data(k_retain(
        ((KVector*)vector.data)->begin[index.data]));
      k_release(index); // Redundant.
      k_release(vector);
    }
  |]

  Builtin.IntToChar -> return "// __int_to_char"
  Builtin.LeFloat -> relational "float" "<="
  Builtin.LeInt -> relational "int" "<="

  Builtin.Left -> return [qc|
    {
      KObject left = k_left(k_pop_data());
      k_push_data(left);
    }
  |]

  Builtin.Length -> return [qc|
    {
      KObject a = k_data[0];
      k_push_data(k_int(((KVector*)a.data)->end - ((KVector*)a.data)->begin));
      k_release(a);
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
    k_push_data(k_unit());
  |]

  Builtin.NotBool -> unary "int" "!"
  Builtin.NotInt -> unary "int" "~"
  Builtin.OrBool -> relational "int" "||"
  Builtin.OrInt -> binary "int" "|"

  Builtin.Pair -> return [qc|
    {
      KObject b = k_pop_data();
      KObject a = k_pop_data();
      k_push_data(k_pair(a, b));
    }
  |]

  -- TODO Unicode output.
  Builtin.Print -> return [qc|
    {
      KObject handle = k_pop_data();
      KObject string = k_pop_data();
      for (KObject* p = ((KVector*)string.data)->begin;
           p != ((KVector*)string.data)->end; ++p) {
        fputc(p->data, (FILE*)handle.data);
      }
      k_release(handle);
      k_release(string);
    }
  |]

  Builtin.Rest -> return [qc|
    {
      KObject a = k_data[0];
      k_data[0] = k_retain(((KPair*)a.data)->rest);
      k_release(a);
    }
  |]

  Builtin.Right -> return [qc|
    {
      KObject right = k_right(k_pop_data());
      k_push_data(right);
    }
  |]

  -- TODO
  {-
  Builtin.ShowFloat -> return [qc|
    {
      KObject* a = *k_data;
      *k_data = k_retain(k_vector(0));
      k_release(a);
    }
  |]

  -- TODO
  Builtin.ShowInt -> return [qc|
    {
      KObject* a = *k_data;
      *k_data = k_retain(k_vector(0));
      k_release(a);
    }
  |]
  -}

  Builtin.Some -> return [qc|
    {
      KObject some = k_some(k_pop_data());
      k_push_data(some);
    }
  |]

  Builtin.Stderr -> return [qc|
    k_push_data(k_handle(stderr));
  |]

  Builtin.Stdin -> return [qc|
    k_push_data(k_handle(stdin));
  |]

  Builtin.Stdout -> return [qc|
    k_push_data(k_handle(stdout));
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
  Builtin.GetLine
  Builtin.Impure
  Builtin.Init
  Builtin.ModFloat
  Builtin.OpenIn
  Builtin.OpenOut
  Builtin.Set
  Builtin.Tail
  -}

  where

  -- a a -> a
  binary :: (Monad m) => Text -> Text -> m Text
  binary _type_ _operation = return [qc|
    assert(!"TODO binary");
  |]
  {-
  return [qc|
    {
      KObject* b = *k_data--;
      if ((*k_data)->refcount == 1) {
        (*k_data)->as_#{ type_ }.value
          #{ operation }= b->as_#{ type_ }.value;
      } else {
        KObject* a = *k_data--;
        *++k_data = k_retain(k_#{ type_ }(
          a->as_#{ type_ }.value #{ operation } b->as_#{ type_ }.value));
        k_release(a);
      }
      k_release(b);
    }
  |]
  -}

  fromBox = return [qc|
    {
      KObject a = k_pop_data();
      k_push_data(*((KObject*)a.data));
    }
  |]

  -- a a -> Bool
  relational :: (Monad m) => Text -> Text -> m Text
  relational _type_ _operation = return [qc|
    assert(!"TODO relational");
  |]

  {-
  return [qc|
    {
      KObject* b = *k_data--;
      KObject* a = *k_data--;
      *++k_data = k_retain(k_int(
        a->as_#{ type_ }.value
        #{ operation } b->as_#{ type_ }.value));
      k_release(a);
      k_release(b);
    }
  |]
  -}

  -- a -> a
  unary :: (Monad m) => Text -> Text -> m Text
  unary _type_ _operation = return [qc|
    assert(!"TODO unary");
  |]

  {-
  return [qc|
    {
      if ((*k_data)->refcount == 1) {
        (*k_data)->as_#{ type_ }.value =
          #{ operation }(*k_data)->as_#{ type_ }.value;
      } else {
        KObject* a = *k_data--;
        *++k_data = k_retain(k_#{ type_ }(
          #{ operation }a->as_#{ type_ }.value));
        k_release(a);
      }
    }
  |]
  -}
