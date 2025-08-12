module Types where

import Common

import Data.Data
import Data.Char
import Data.Typeable
import Data.Map (Map)
import Text.PrettyPrint.GenericPretty
import Optics

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

data TyCtor = MkTyCtor { name :: TyName, lt :: Lt, args :: [MonoTy] }
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

data TySchema = MkTySchema
  { ltParams :: [LtName]
  , tyParams :: [TyParam]
  , ty :: MonoTy
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TySchema

type EffSig = Map OpName OpSig

data OpSig = MkOpSig
  { tyParams :: [TyName]
  , params :: [MonoTy]
  , res :: MonoTy
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow OpSig

emptyTySchema :: MonoTy -> TySchema
emptyTySchema = MkTySchema [] []

class Top ty where
  top :: ty
instance Top TyCtor where
  top = MkTyCtor { name = "Any", lt = LtLocal, args = [] }
instance Top MonoTy where
  top = TyCtor top

makePrisms ''MonoTy
makePrisms ''Lt
