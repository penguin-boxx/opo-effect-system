module TypingCtx where

import Common
import Types

import Data.Data
import Data.List qualified as List
import Data.Typeable
import Prelude hiding (lookup)
import Control.Monad.Except
import GHC.Stack
import Text.PrettyPrint.GenericPretty

type TyCtx = [TyCtxEntry]

data TyCtxEntry
  = TyCtxVar TyCtxVar
  | TyCtxCap TyCtxCap
  | TyCtxTy TyParam
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

data TyCtxCtor = MkTyCtxCtor
  { name :: CtorName
  , ltParams :: [LtName]
  , tyParams :: [TyParam]
  , params :: [MonoTy]
  , res :: TyCtor
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow TyCtxCtor

type EffCtx = [EffCtxEntry]

data EffCtxEntry = MkEffCtxEntry
  { capCtor :: CtorName
  , tyParams :: [TyName]
  , ops :: EffSig
  , effName :: TyName
  }
  deriving stock (Eq, Ord, Data, Typeable, Generic)
  deriving anyclass Out
  deriving Show via OutShow EffCtxEntry


class Lookup ctx key result | result -> ctx key where
  lookup :: HasCallStack => ctx -> key -> result

instance MonadError String m => Lookup TyCtx VarName (m TySchema) where
  lookup tyCtx targetName = case tyCtx of
    [] -> throwError $ "Name not found " <> targetName <> " in ctx " <> show tyCtx
    TyCtxVar MkTyCtxVar { name, tySchema } : _ | name == targetName -> pure tySchema
    TyCtxCap MkTyCtxCap { name, monoTy } : _ | name == targetName -> pure (emptyTySchema monoTy)
    TyCtxCtor MkTyCtxCtor { name, ltParams, tyParams, params, res } : _ | name == targetName ->
      pure MkTySchema
        { ltParams, tyParams
        , ty = TyFun MkTyFun { ctx = [], lt = LtFree, args = params, res = TyCtor res }
        }
    _ : rest -> rest `lookup` targetName

lookupBound :: MonadError String m => TyCtx -> VarName -> m MonoTy
lookupBound tyCtx targetName = case tyCtx of
    [] -> throwError $ "Name not found " <> targetName <> " in ctx " <> show tyCtx
    TyCtxTy MkTyParam { name, bound } : _ | name == targetName -> pure bound
    _ : rest -> rest `lookupBound` targetName

instance Lookup TyCtx TyName [TyCtxCtor] where
  lookup tyCtx targetName =
    [ ctor
    | TyCtxCtor ctor@MkTyCtxCtor { res = MkTyCtor { name } } <- tyCtx
    , name == targetName
    ]

instance MonadError String m => Lookup EffCtx TyName (m EffCtxEntry) where
  lookup :: MonadError String m => EffCtx -> TyName -> m EffCtxEntry
  lookup effCtx targetName =
    case List.find (\MkEffCtxEntry { effName } -> effName == targetName) effCtx of
      Nothing -> throwError $ "Effect not found " <> targetName
      Just entry -> pure entry
