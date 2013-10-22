module Kitten.Infer.Type
  ( fromAnno
  ) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Map (Map)
import Data.Text (Text)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Kitten.Anno (Anno(Anno))
import Kitten.Infer.Monad (Inferred, freshVarM)
import Kitten.Name
import Kitten.Type
import Kitten.Util.Monad

import qualified Kitten.Anno as Anno

data Env = Env
  { envRows :: [Name]
  , envScalars :: !(Map Text Name)
  , envEffects :: !(Map Text Name)
  }

type Converted a = StateT Env Inferred a

fromAnno :: Anno -> Inferred (Scheme (Type Scalar))
fromAnno (Anno annoType loc) = do
  (type_, env) <- flip runStateT Env
    { envRows = []
    , envScalars = M.empty
    , envEffects = M.empty
    } $ fromAnnoType' annoType
  return $ Forall
    (S.fromList (map row (envRows env)))
    (S.fromList . map scalar . M.elems $ envScalars env)
    (S.fromList . map effect . M.elems $ envEffects env)
    type_
  where

  fromAnnoType' :: Anno.Type -> Converted (Type Scalar)
  fromAnnoType' type_ = case type_ of
    Anno.Bool -> return (Bool loc)
    Anno.Char -> return (Char loc)
    Anno.Choice a b -> (:|) <$> fromAnnoType' a <*> fromAnnoType' b
    Anno.Function a b e -> do
      Var r _ <- lift freshVarM
      modify $ \env -> env { envRows = r : envRows env }
      Function
        <$> (V.foldl' (:.) (Var r loc) <$> V.mapM fromAnnoType' a)
        <*> (V.foldl' (:.) (Var r loc) <$> V.mapM fromAnnoType' b)
        <*> fromAnnoRowEffect e
        <*> pure loc
    Anno.Float -> return (Float loc)
    Anno.Handle -> return (Handle loc)
    Anno.Int -> return (Int loc)
    Anno.Named name -> return (Named name loc)
    Anno.Option a -> (:?) <$> fromAnnoType' a
    Anno.Pair a b -> (:&) <$> fromAnnoType' a <*> fromAnnoType' b
    Anno.Unit -> return (Unit loc)
    Anno.Var name -> do
      mExisting <- gets
        $ \env -> M.lookup name (envScalars env)
      case mExisting of
        Just existing -> return (Var existing loc)
        Nothing -> do
          Var var _ <- lift freshVarM
          modify $ \env -> env
            { envScalars = M.insert name var (envScalars env) }
          return (Var var loc)
    Anno.Vector a -> Vector <$> fromAnnoType' a <*> pure loc
    _ -> error "converting effect annotation to non-effect type"

  fromAnnoRowEffect :: Anno.Type -> Converted (Type ERow)
  fromAnnoRowEffect e = case e of
    Anno.NoEffect -> return (NoEffect loc)
    Anno.SomeEffect -> freshEffectVar Nothing
    Anno.Var name -> do
      mExisting <- gets
        $ \env -> M.lookup name (envEffects env)
      case mExisting of
        Just existing -> return (Var existing loc)
        Nothing -> freshEffectVar (Just name)
    Anno.Join a b -> (:+) <$> fromAnnoScalarEffect a <*> fromAnnoRowEffect b
    _ -> (:+) <$> fromAnnoScalarEffect e <*> freshEffectVar Nothing

  fromAnnoScalarEffect :: Anno.Type -> Converted (Type EScalar)
  fromAnnoScalarEffect e = case e of
    Anno.IOEffect -> return (IOEffect loc)
    _ -> error "converting scalar effect annotation to row effect type"

  freshEffectVar :: Maybe Text -> Converted (Type a)
  freshEffectVar mName = do
    Var var _ <- lift freshVarM
    whenJust mName $ \name -> modify $ \env -> env
      { envEffects = M.insert name var (envEffects env) }
    return (Var var loc)
