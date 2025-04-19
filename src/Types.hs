module Types where

import Common
import Data.Map (Map)
import Text.PrettyPrint.GenericPretty

type TyName = String
type TyIdx = Int

data MonoTy = MonoTy TyName TyIdx [MonoTy]
  deriving Eq

type Effs = Map OpName Ty

data Ty
  = Effs :=> MonoTy
  | Forall TyName Ty
  deriving Eq

deriving stock instance Generic MonoTy
instance Out MonoTy
instance Show MonoTy where
  show = pretty
deriving stock instance Generic Ty
instance Out Ty
instance Show Ty where
  show = pretty
