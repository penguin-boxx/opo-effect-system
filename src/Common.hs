{-# OPTIONS_GHC "-Wno-orphans" #-}
{-# LANGUAGE UndecidableInstances #-} -- for apply collection instance

module Common where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Text.PrettyPrint.GenericPretty
import Data.Coerce
import Data.Generics.Uniplate.Data (Uniplate)
import Data.Generics.Uniplate.Data qualified as Uniplate
import Optics

type VarName = String
type OpName = String
type CtorName = String

instance (Out k, Out v) => Out (Map k v) where
  doc = doc . Map.toList
  docPrec = const doc

newtype OutShow a = OutShow a

instance Out a => Show (OutShow a) where
  show (OutShow x) = pretty x

subTrees :: Uniplate a => Getter a [a]
subTrees = to Uniplate.universe

toSetOf :: (Is k A_Fold, Ord a) => Optic' k is s a -> s -> Set a
toSetOf o = foldMapOf o Set.singleton

class From from to where
  from :: from -> to

class Apply f arg res | f arg -> res where
  (@) :: f -> arg -> res

instance (Apply f arg res, Functor collection) => Apply f (collection arg) (collection res) where
  f @ args = fmap (f @) args
