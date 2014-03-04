{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Kitten.Tree
  ( Pass(..)
  , Term(..)
  , Value(..)
  ) where

import Data.Text (Text)
import Data.Vector (Vector)

import Kitten.Builtin
import Kitten.ClosedName
import Kitten.Location
import Kitten.Name
import Kitten.Type (Kind(..), Type)

data Pass
  = Parsed
  | Resolved
  | Typed

data Term (p :: Pass) where
  Builtin :: !Builtin -> !Location -> ExprMeta p -> Term p
  Call :: !(ExprLabel p) -> !Location -> ExprMeta p -> Term p
  Compose :: !(Vector (Term p)) -> !Location -> ExprMeta p -> Term p
  From :: !Text -> !Location -> ExprMeta p -> Term p
  Lambda :: !Text -> !(Term Parsed) -> !Location -> ExprMeta Parsed -> Term Parsed
  PairTerm :: !(Term p) -> !(Term p) -> !Location -> ExprMeta p -> Term p
  Push :: !(Value p) -> !Location -> ExprMeta p -> Term p
  To :: !Text -> !Location -> ExprMeta p -> Term p
  Scoped :: !Text -> !Location -> ExprMeta p -> Term p
  VectorTerm :: !(Vector (Term p)) -> !Location -> ExprMeta p -> Term p

instance (Eq (ExprLabel p), Eq (ExprMeta p)) => Eq (Term p) where
  Builtin a1 b1 c1 == Builtin a2 b2 c2 = (a1, b1, c1) == (a2, b2, c2)
  Call a1 b1 c1 == Call a2 b2 c2 = (a1, b1, c1) == (a2, b2, c2)
  Compose a1 b1 c1 == Compose a2 b2 c2 = (a1, b1, c1) == (a2, b2, c2)
  From a1 b1 c1 == From a2 b2 c2 = (a1, b1, c1) == (a2, b2, c2)
  Lambda a1 b1 c1 d1 == Lambda a2 b2 c2 d2
    = (a1, b1, c1, d1) == (a2, b2, c2, d2)
  PairTerm a1 b1 c1 d1 == PairTerm a2 b2 c2 d2
    = (a1, b1, c1, d1) == (a2, b2, c2, d2)
  Push a1 b1 c1 == Push a2 b2 c2 = (a1, b1, c1) == (a2, b2, c2)
  To a1 b1 c1 == To a2 b2 c2 = (a1, b1, c1) == (a2, b2, c2)
  Scoped a1 b1 c1 == Scoped a2 b2 c2 = (a1, b1, c1) == (a2, b2, c2)
  VectorTerm a1 b1 c1 == VectorTerm a2 b2 c2 = (a1, b1, c1) == (a2, b2, c2)
  _ == _ = False

data Value (p :: Pass) where
  Bool :: !Bool -> !Location -> Value p
  Char :: !Char -> !Location -> Value p
  Closed :: !Name -> Value p
  Closure :: !(Vector ClosedName) -> !(Term p) -> Value p
  Float :: !Double -> !Location -> Value p
  Function :: !(Vector (Term Parsed)) -> !Location -> Value Parsed
  Int :: !Int -> !Location -> Value p
  Local :: !Name -> Value p
  Unit :: !Location -> Value p
  String :: !Text -> !Location -> Value p

instance (Eq (ExprLabel p), Eq (ExprMeta p)) => Eq (Value p) where
  Bool a1 b1 == Bool a2 b2 = (a1, b1) == (a2, b2)
  Char a1 b1 == Char a2 b2 = (a1, b1) == (a2, b2)
  Closed a1 == Closed a2 = a1 == a2
  Closure a1 b1 == Closure a2 b2 = (a1, b1) == (a2, b2)
  Float a1 b1 == Float a2 b2 = (a1, b1) == (a2, b2)
  Function a1 b1 == Function a2 b2 = (a1, b1) == (a2, b2)
  Int a1 b1 == Int a2 b2 = (a1, b1) == (a2, b2)
  Local a1 == Local a2 = a1 == a2
  Unit a1 == Unit a2 = a1 == a2
  String a1 b1 == String a2 b2 = (a1, b1) == (a2, b2)
  _ == _ = False

type family ExprLabel (p :: Pass) :: *
type family ExprMeta (p :: Pass) :: *

type instance ExprLabel Parsed = Text
type instance ExprMeta Parsed = ()

type instance ExprLabel Resolved = Name
type instance ExprMeta Resolved = ()

type instance ExprLabel Typed = Name
type instance ExprMeta Typed = Type Scalar

{-
defTypeScheme :: TypedDef -> TypeScheme
defTypeScheme def = type_ <$ defTerm def
  where
  type_ = typedType $ unScheme (defTerm def)

typedType :: Term Typed -> Type Scalar
typedType typed = case typed of
  Builtin _ _ t -> t
  Call _ _ t -> t
  Compose _ _ t -> t
  From _ _ t -> t
  PairTerm _ _ _ t -> t
  Push _ _ t -> t
  To _ _ t -> t
  Scoped _ _ t -> t
  VectorTerm _ _ t -> t
-}
