module Types where

import Common
import Data.Data
import Data.Char
import Data.Typeable
import Text.PrettyPrint.GenericPretty


type TyName = String

isVarCtor :: TyName -> Bool
isVarCtor = isLowerCase . head


data MonoTy = MonoTy { ctor :: TyName, args :: [MonoTy] }
  deriving (Eq, Ord, Data, Typeable)

mkVar :: TyName -> MonoTy
mkVar = monoFromName

monoFromName :: TyName -> MonoTy
monoFromName name = MonoTy { ctor = name, args = [] }


type Effs = [MonoTy]

data EffTy
  = NoEff MonoTy -- \tau
  | EffTy { effs :: Effs, from :: EffTy, to :: EffTy } -- ctx(e) \xi -> \eta
  deriving (Eq, Data, Typeable)

effTyFromName :: TyName -> EffTy
effTyFromName = NoEff . monoFromName


data TySchema = TySchema { params :: [TyName], ty :: EffTy }
  deriving (Eq, Data, Typeable)

tySchemaFromMono :: MonoTy -> TySchema
tySchemaFromMono monoTy = TySchema { params = [], ty = NoEff monoTy }

tySchemaFromEff :: EffTy -> TySchema
tySchemaFromEff effTy = TySchema { params = [], ty = effTy }


deriving stock instance Generic MonoTy
instance Out MonoTy
instance Show MonoTy where
  show = pretty
deriving stock instance Generic EffTy
instance Out EffTy
instance Show EffTy where
  show = pretty
deriving stock instance Generic TySchema
instance Out TySchema
instance Show TySchema where
  show = pretty
