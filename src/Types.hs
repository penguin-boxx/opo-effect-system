module Types where

import Common
import Data.Data
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Char
import Data.Typeable
import Text.PrettyPrint.GenericPretty

type TyName = String

data MonoTy = MonoTy { tyCtor :: TyName, tyArgs :: [Ty] }
  deriving (Eq, Data, Typeable)

mkVar :: TyName -> MonoTy
mkVar = monoFromName

monoFromName :: TyName -> MonoTy
monoFromName name = MonoTy { tyCtor = name, tyArgs = [] }

isVarCtor :: TyName -> Bool
isVarCtor = isLowerCase . head

type Effs = Map OpName MonoTy

data Ty = Ty { tyParams :: [TyName], effs :: Effs, monoTy :: MonoTy }
  deriving (Eq, Data, Typeable)

tyFromMono :: MonoTy -> Ty
tyFromMono monoTy = Ty { tyParams = [], effs = Map.empty, monoTy }

deriving stock instance Generic MonoTy
instance Out MonoTy
instance Show MonoTy where
  show = pretty
deriving stock instance Generic Ty
instance Out Ty
instance Show Ty where
  show = pretty
