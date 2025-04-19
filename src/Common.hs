{-# OPTIONS_GHC "-Wno-orphans" #-}

module Common where

import Data.Map (Map)
import Data.Map qualified as Map
import Text.PrettyPrint.GenericPretty

type VarName = String
type OpName = String

instance (Out k, Out v) => Out (Map k v) where
  doc = doc . Map.toList
  docPrec = const doc
