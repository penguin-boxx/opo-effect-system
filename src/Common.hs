{-# OPTIONS_GHC "-Wno-orphans" #-}

module Common where

import Data.Map (Map)
import Data.Map qualified as Map
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

universe :: Uniplate a => Getter a [a]
universe = to Uniplate.universe
