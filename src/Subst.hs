{-# LANGUAGE UndecidableInstances #-}

module Subst where

import Common
import Syntax
import Types

import Control.Monad
import Control.Monad.Except
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe


class Apply f arg res | f arg -> res where
  (@) :: f -> arg -> res

instance (Apply f arg res, Functor collection) => Apply f (collection arg) (collection res) where
  f @ args = fmap (f @) args


class DoSubst target where
  onLt :: Subst target -> String -> Lt -> Lt
  onMonoTy :: Subst target -> String -> MonoTy -> MonoTy

instance DoSubst Lt where
  onLt (Subst subst) name or = Map.findWithDefault or name subst
  onMonoTy _ _ = id

instance DoSubst MonoTy where
  onLt _ _ = id
  onMonoTy (Subst subst) name or = Map.findWithDefault or name subst


newtype Subst target = Subst { getSubst :: Map String target }
  deriving newtype Show

mkSubst :: MonadError String m => [String] -> [target] -> m (Subst target)
mkSubst names target = do
  unless (length names == length target) $
    throwError "Unexpected number of lifetime parameters"
  pure $ Subst $ Map.fromList $ zip names target

instance DoSubst target => Apply (Subst target) Lt Lt where
  f @ arg = case arg of
    LtVar name -> onLt f name arg
    LtLocal -> LtLocal
    LtFree -> LtFree
    LtIntersect lts -> LtIntersect (f @ lts)

instance DoSubst target => Apply (Subst target) MonoTy MonoTy where
  f @ arg = case arg of
    TyVar name -> onMonoTy f name arg
    TyCtor ctor -> TyCtor $ f @ ctor
    TyFun MkTyFun { ctx, lt, args, res } -> TyFun MkTyFun { ctx = f @ ctx, lt = f @ lt, args = f @ args, res = f @ res}

instance DoSubst target => Apply (Subst target) TyCtor TyCtor where
  f @ MkTyCtor { name, lt, args } = MkTyCtor { name, lt = f @ lt, args = f @ args }

instance DoSubst target => Apply (Subst target) Expr Expr where
  f @ arg = case arg of
    Const _ -> arg
    Plus lhs rhs -> Plus (f @ lhs) (f @ rhs)
    Var _ -> arg
    TLam {} -> error "TLam is not supported"
    TApp MkTApp { lhs, ltArgs, tyArgs } -> TApp MkTApp { lhs = f @ lhs, ltArgs = f @ ltArgs, tyArgs = f @ tyArgs }
    CapCtor MkCapCtor { name, tyArgs, marker, handler } -> CapCtor MkCapCtor { name, tyArgs = f @ tyArgs, marker, handler = f @ handler }
    Lam MkLam { ctxParams, params, body } -> Lam MkLam { ctxParams = f @ ctxParams, params = f @ params, body = f @ body }
    App MkApp { callee, ctxArgs, args } -> App MkApp { callee = f @ callee, ctxArgs = f @ ctxArgs, args = f @ args }
    Match MkMatch { scrutinee, branches } -> Match MkMatch { scrutinee = f @ scrutinee, branches = f @ branches }
    Perform MkPerform { opName, cap, tyArgs, args } -> Perform MkPerform { opName, cap = f @ cap, tyArgs = f @ tyArgs, args = f @ args }
    Handle MkHandle { capName, effTy, handler, body } -> Handle MkHandle { capName, effTy = f @ effTy, handler = f @ handler, body = f @ body }
    RtHandler MkRtHandler { marker, body } -> RtHandler MkRtHandler { marker, body = f @ body }

instance DoSubst target => Apply (Subst target) HandlerEntry HandlerEntry where
  f @ MkHandlerEntry { opName, body } = MkHandlerEntry { opName, body = f @ body }

instance DoSubst target => Apply (Subst target) Param Param where
  f @ MkParam { name, ty } = MkParam { name, ty = f @ ty }

instance DoSubst target => Apply (Subst target) Branch Branch where
  f @ MkBranch { ctorName, varPatterns, body } = MkBranch { ctorName, varPatterns, body = f @ body }
