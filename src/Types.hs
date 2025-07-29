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
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow Lt

data MonoTy = TyVar TyName | TyCtor TyCtor | TyFun TyFun
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow MonoTy

data TyCtor = MkTyCtor { name :: CtorName, lt :: Lt, args :: [MonoTy] }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyCtor

data TyFun = MkTyFun
  { ctx :: EffRow
  , lt :: Lt
  , args :: [MonoTy]
  , res :: MonoTy
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyFun

type EffRow = [MonoTy]

data TyParam = MkTyParam { name :: TyName, bound :: MonoTy }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyParam

data TySchema = TySchema
  { ltParams :: [LtName]
  , tyParams :: [TyParam]
  , ty :: MonoTy
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TySchema

data TyCtxEntry
  = TyCtxVar TyCtxVar
  | TyCtxCap TyCtxCap
  | TyCtxTy TyCtxTy
  | TyCtxCtor TyCtxCtor
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyCtxEntry

data TyCtxVar = MkTyCtxVar { name :: VarName, tySchema :: TySchema }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyCtxVar

data TyCtxCap = MkTyCtxCap { name :: VarName, monoTy :: MonoTy }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyCtxCap

data TyCtxTy = MkTyCtxTy { name :: TyName, bound :: MonoTy }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyCtxTy

data TyCtxCtor = MkTyCtxCtor
  { name :: CtorName
  , ltParams :: [LtName]
  , tyParams :: [TyParam]
  , args :: [MonoTy]
  , res :: TyCtor
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyCtxCtor

type TyCtx = [TyCtxEntry]

data OpSig = MkOpSig
  { tyParams :: [TyParam]
  , args :: [MonoTy]
  , res :: MonoTy
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow OpSig

type EffSig = Map OpName OpSig

data EffCtxEntry = EffCtxEntry
  { capCtor :: TyName
  , tyParams :: [TyName]
  , sig :: EffSig
  , tyCtor :: TyName
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow EffCtxEntry

type EffCtx = [EffCtxEntry]


emptyTySchema :: MonoTy -> TySchema
emptyTySchema = TySchema [] []
