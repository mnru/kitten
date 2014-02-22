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
import Kitten.Util.Text (showText)
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
  modify $ \e -> e
    { envNext = succ (envNext e)
    , envOffsets = N.insert current offset (envOffsets e)
    }
  return current

advance :: State Env Text
advance = do
  offsets <- N.toList <$> gets envOffsets
  let (labels, remaining) = foldr go ([], []) offsets
  modify $ \e -> e { envOffsets = N.fromList remaining }
  return . T.unlines . for labels $ \label -> local label <> ":"
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
      K_PUSH_RETURN(((KR){ .address = &&exit, .closure = 0 }));
      goto entry;
  |]

  end = [qc|
    exit:
      return 0;
    }
  |]

  go instruction = (<>) <$> advance <*> case instruction of
    Act label names -> return [qc|K_ACT(#{ global label }, #{
      T.intercalate ", " $ showText (V.length names)
        : map closedName (V.toList names) });|]
      where
      closedName (ClosedName (Name index))
        = [qc|K_CLOSED, #{ showText index }|]
      closedName (ReclosedName (Name index))
        = [qc|K_RECLOSED, #{ showText index }|]
    Builtin builtin -> toCBuiltin builtin
    Call label -> do
      next <- newLabel 0
      return [qc|K_CALL(#{ global label }, #{ local next });|]
    Closure index -> return
      [qc|k_push_data(K_GET_CLOSURE(#{ showText index }));|]
    Comment text -> return [qc|/* #{ text } */|]
    Enter -> return [qc|k_push_locals(k_pop_data());|]
    EntryLabel -> return "entry:"
    Leave -> return [qc|K_DROP_LOCALS();|]
    Label label -> return [qc|#{ global label }:|]
    Local index -> return [qc|k_push_data(K_GET_LOCAL(#{ showText index }));|]
    MakeVector size -> return [qc|K_MAKE_VECTOR(#{ showText size });|]
    Push x -> return [qc|k_push_data(#{ toCValue x });|]
    Return -> return [qc|K_RETURN();|]

toCValue :: Value -> Text
toCValue value = case value of
  Bool x -> [qc|k_bool(#{ showText (fromEnum x :: Int) })|]
  Char x -> [qc|k_char(#{ showText (fromEnum x :: Int) })|]
  Choice False x -> [qc|k_left(#{ toCValue x })|]
  Choice True x -> [qc|k_right(#{ toCValue x })|]
  Float x -> [qc|k_float(#{ showText x })|]
  Int x -> [qc|k_int(#{ showText x })|]
  Option Nothing -> "k_none()"
  Option (Just x) -> [qc|k_some(#{ toCValue x })|]
  Pair x y -> [qc|k_pair(#{ toCValue x }, #{ toCValue y })|]
  String x
    | T.null x -> [qc|k_vector(0)|]
    | otherwise -> [qc|k_vector(#{ showText (T.length x) }, #{
      T.intercalate ", " . map char $ T.unpack x })|]
      where char c = [qc|k_char(#{ showText c })|]
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
  Builtin.AddVector -> return [qc|K_ADD_VECTOR();|]
  Builtin.AndBool -> relational "int" "&&"
  Builtin.AndInt -> binary "int" "&"
  Builtin.Apply -> do
    next <- newLabel 0
    return [qc|K_APPLY(#{ local next });|]
  Builtin.CharToInt -> return "/* __char_to_int */"
  Builtin.Choice -> return [qc|K_CHOICE();|]
  Builtin.ChoiceElse -> return [qc|K_CHOICE_ELSE();|]
  Builtin.Close -> return [qc|K_CLOSE();|]
  Builtin.DivFloat -> binary "float" "/"
  Builtin.DivInt -> binary "int" "/"
  Builtin.EqFloat -> relational "float" "=="
  Builtin.EqInt -> relational "int" "=="
  Builtin.Exit -> return [qc|exit(k_data[0].data);|]
  Builtin.GeFloat -> relational "float" ">="
  Builtin.GeInt -> relational "int" ">="
  Builtin.GtFloat -> relational "float" ">"
  Builtin.GtInt -> relational "int" ">"
  Builtin.First -> return [qc|K_FIRST();|]
  Builtin.FromLeft -> return [qc|K_FROM_BOX();|]
  Builtin.FromRight -> return [qc|K_FROM_BOX();|]
  Builtin.FromSome -> return [qc|K_FROM_BOX();|]
  Builtin.Get -> return [qc|K_GET();|]
  Builtin.If -> return [qc|K_IF();|]
  Builtin.IfElse -> return [qc|K_IF_ELSE();|]
  Builtin.IntToChar -> return "/* __int_to_char */"
  Builtin.LeFloat -> relational "float" "<="
  Builtin.LeInt -> relational "int" "<="
  Builtin.Left -> return [qc|K_LEFT();|]
  Builtin.Length -> return [qc|K_LENGTH();|]
  Builtin.LtFloat -> relational "float" "<"
  Builtin.LtInt -> relational "int" "<"
  Builtin.ModInt -> binary "int" "%"
  Builtin.MulFloat -> binary "float" "*"
  Builtin.MulInt -> binary "int" "*"
  Builtin.NeFloat -> relational "float" "!="
  Builtin.NeInt -> relational "int" "!="
  Builtin.NegFloat -> unary "float" "-"
  Builtin.NegInt -> unary "int" "-"
  Builtin.None -> return [qc|k_push_data(k_none());|]
  Builtin.NotBool -> unary "int" "!"
  Builtin.NotInt -> unary "int" "~"
  Builtin.Option -> return [qc|K_OPTION();|]
  Builtin.OptionElse -> return [qc|K_OPTION_ELSE();|]
  Builtin.OrBool -> relational "int" "||"
  Builtin.OrInt -> binary "int" "|"
  Builtin.Pair -> return [qc|K_PAIR();|]
  Builtin.Print -> return [qc|K_PRINT();|]
  Builtin.Rest -> return [qc|K_REST();|]
  Builtin.Right -> return [qc|K_RIGHT();|]
  Builtin.ShowInt -> return [qc|K_SHOW_INT();|]
  Builtin.Some -> return [qc|K_SOME();|]
  Builtin.Stderr -> return [qc|k_push_data(k_handle(stderr));|]
  Builtin.Stdin -> return [qc|k_push_data(k_handle(stdin));|]
  Builtin.Stdout -> return [qc|k_push_data(k_handle(stdout));|]
  Builtin.SubFloat -> binary "float" "-"
  Builtin.SubInt -> binary "int" "-"
  Builtin.UnsafePurify11 -> return "/* __unsafe_purify11 */"
  Builtin.XorBool -> relational "int" "!="
  Builtin.XorInt -> relational "int" "^"

  Builtin.GetLine -> return [qc|assert(!"TODO stdio __get_line");|]
  Builtin.Init -> return [qc|K_INIT();|]
  Builtin.ModFloat -> return [qc|K_MOD_FLOAT();|]
  Builtin.OpenIn -> return [qc|assert(!"TODO stdio __open_in");|]
  Builtin.OpenOut -> return [qc|assert(!"TODO stdio __open_out");|]
  Builtin.Set -> return [qc|K_SET();|]
  Builtin.ShowFloat -> return [qc|assert(!"TODO builtin __show_float");|]
  Builtin.Tail -> return [qc|K_TAIL();|]

  where

  -- a a -> a
  binary :: (Monad m) => Text -> Text -> m Text
  binary type_ operation = return [qc|K_BINARY(#{ type_ }, #{ operation });|]

  -- a a -> Bool
  relational :: (Monad m) => Text -> Text -> m Text
  relational type_ operation = return
    [qc|K_RELATIONAL(#{ type_ }, #{ operation });|]

  -- a -> a
  unary :: (Monad m) => Text -> Text -> m Text
  unary type_ operation = return [qc|K_UNARY(#{ type_ }, #{ operation });|]
