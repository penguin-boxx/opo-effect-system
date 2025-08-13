module Types where

import Common

import Data.Char
import Data.Data
import Data.Function
import Data.Typeable
import Data.Map (Map)
import Data.List qualified as List
import Data.Set qualified as Set
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

makePrisms ''Lt
makePrisms ''MonoTy

class Eq ty => NormalizedEq ty where
  (===) :: ty -> ty -> Bool

instance NormalizedEq MonoTy where
  TyVar name1 === TyVar name2 = name1 == name2
  (===)
    (TyCtor MkTyCtor { name = name1, lt = normalizeLt -> lt1, args = args1 })
    (TyCtor MkTyCtor { name = name2, lt = normalizeLt -> lt2, args = args2 }) = name1 ==
      name2 && lt1 == lt2 && and (zipWith (===) args1 args2)
  (===)
    (TyFun MkTyFun { ctx = ctx1, lt = normalizeLt -> lt1, args = args1, res = res1 })
    (TyFun MkTyFun { ctx = ctx2, lt = normalizeLt -> lt2, args = args2, res = res2 }) =
      and $ zipWith (===) ctx1 ctx2 ++ (lt1 == lt2) : zipWith (===) args1 args2 ++ [res1 === res2]
  _ === _ = False

normalizeLt :: Lt -> Lt
normalizeLt lt =
  let o = subTrees % filtered (_LtIntersect `isn't`) % filtered (_LtFree `isn't`) in
  let primitives = o `toSetOf` lt in
  if null primitives then LtFree
  else if LtLocal `Set.member` primitives then LtLocal
  else LtIntersect (Set.toAscList primitives)
