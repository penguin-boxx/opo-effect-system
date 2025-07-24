module Types where

import Common
import Data.Data
import Data.Char
import Data.Typeable
import Data.Map (Map)
import Text.PrettyPrint.GenericPretty

type TyName = String
type LtName = String

-- | Lifetimes.
data Lt
  = LtVar LtName
  | LtLocal
  | LtFree
  | LtIntersect [Lt]
  deriving (Eq, Ord, Data, Typeable)

type EffRow = [MonoTy]

data MonoTy
  = TyVar TyName
  | TyCtor { ctor :: TyName, lt :: Lt, args :: [MonoTy] }
  | TyFun { ctx :: EffRow, lt :: Lt, args :: [MonoTy], res :: MonoTy }
  deriving stock (Eq, Ord, Data, Typeable)

data TyParam = TyParam { name :: TyName, bound :: MonoTy }
  deriving stock (Eq, Ord, Data, Typeable)

data TySchema = TySchema
  { ltParams :: [LtName]
  , tyParams :: [TyParam]
  , ty :: MonoTy
  }
  deriving stock (Eq, Ord, Data, Typeable)

data TyCtxEntry
  = TyCtxVar { name :: VarName, tySchema :: TySchema }
  | TyCtxCap { name :: VarName, monoTy :: MonoTy }
  | TyCtxTy { name :: TyName, bound :: MonoTy }
  | TyCtxCtor
    { name :: CtorName
    , ltParams :: [LtName], tyParams :: [TyParam]
    , args :: [MonoTy], res :: MonoTy
    }
  deriving stock (Eq, Ord, Data, Typeable)

type TyCtx = [TyCtxEntry]

data OpSig = OpSig { tyParams :: [TyParam], args :: [MonoTy], res :: MonoTy }
  deriving stock (Eq, Ord, Data, Typeable)

type EffSig = Map OpName OpSig

data EffCtxEntry = EffCtxEntry
  { capCtor :: TyName
  , tyParams :: [TyName]
  , sig :: EffSig
  , tyCtor :: TyName
  }
  deriving stock (Eq, Ord, Data, Typeable)

type EffCtx = [EffCtxEntry]


emptyTySchema :: MonoTy -> TySchema
emptyTySchema = TySchema [] []


deriving stock instance Generic Lt
instance Out Lt
instance Show Lt where
  show = pretty
deriving stock instance Generic MonoTy
instance Out MonoTy
instance Show MonoTy where
  show = pretty
deriving stock instance Generic TyParam
instance Out TyParam
instance Show TyParam where
  show = pretty
deriving stock instance Generic TySchema
instance Out TySchema
instance Show TySchema where
  show = pretty
deriving stock instance Generic TyCtxEntry
instance Out TyCtxEntry
instance Show TyCtxEntry where
  show = pretty
deriving stock instance Generic OpSig
instance Out OpSig
instance Show OpSig where
  show = pretty
deriving stock instance Generic EffCtxEntry
instance Out EffCtxEntry
instance Show EffCtxEntry where
  show = pretty
