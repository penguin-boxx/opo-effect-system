module Types where

import Common
import Text.PrettyPrint.GenericPretty

type TyName = String
type TyIdx = Int

data MonoTy = MonoTy TyName TyIdx [MonoTy]
  deriving Eq

data Eff = Eff OpName [MonoTy]
  deriving Eq

type Effs = [Eff]

data Ty
  = Effs :=> MonoTy
  | Forall TyName Ty
  deriving Eq

deriving stock instance Generic MonoTy
instance Out MonoTy
instance Show MonoTy where
  show = pretty
deriving stock instance Generic Eff
instance Out Eff
instance Show Eff where
  show = pretty
deriving stock instance Generic Ty
instance Out Ty
instance Show Ty where
  show = pretty
