{-# OPTIONS_GHC "-Wno-orphans" #-}

module Common where

import Data.Map (Map)
import Data.Map qualified as Map
import Text.PrettyPrint.GenericPretty
import Data.Generics.Uniplate.Data (Uniplate)
import Data.Generics.Uniplate.Data qualified as Uniplate
import Optics

type VarName = String
type OpName = String

instance (Out k, Out v) => Out (Map k v) where
  doc = doc . Map.toList
  docPrec = const doc

class Apply f arg res | f arg -> res where
  (@) :: f -> arg -> res

universe :: Uniplate a => Getter a [a]
universe = to Uniplate.universe
