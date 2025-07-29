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


-- class Apply f arg res | f arg -> res where
--   (@) :: f -> arg -> res

-- instance (Apply f arg res, Functor collection) => Apply f (collection arg) (collection res) where
--   f @ args = fmap (f @) args


-- class DoSubst target where
--   onLt :: Subst target -> String -> Lt -> Lt
--   onMonoTy :: Subst target -> String -> MonoTy -> MonoTy

-- instance DoSubst Lt where
--   onLt (Subst subst) name or = Map.findWithDefault or name subst
--   onMonoTy _ _ = id

-- instance DoSubst MonoTy where
--   onLt _ _ = id
--   onMonoTy (Subst subst) name or = Map.findWithDefault or name subst


-- newtype Subst target = Subst { getSubst :: Map String target }

-- mkSubst :: MonadError String m => [String] -> [target] -> m (Subst target)
-- mkSubst names target = do
--   unless (length names == length target) $
--     throwError "Unexpected number of lifetime parameters"
--   pure $ Subst $ Map.fromList $ zip names target

-- instance DoSubst target => Apply (Subst target) Lt Lt where
--   f @ arg = case arg of
--     LtVar name -> onLt f name arg
--     LtLocal -> LtLocal
--     LtFree -> LtFree
--     LtIntersect lts -> LtIntersect (f @ lts)

-- instance DoSubst target => Apply (Subst target) MonoTy MonoTy where
--   f @ arg = case arg of
--     TyVar name -> onMonoTy f name arg
--     TyCtor MkTyCtor { ctor, lt, args } -> TyCtor MkTyCtor { ctor, lt = f @ lt, args = f @ args }
--     TyFun MkTyFun { ctx, lt, args, res } -> TyFun MkTyFun { ctx = f @ ctx, lt = f @ lt, args = f @ args, res = f @ res}

-- instance DoSubst target => Apply (Subst target) Expr Expr where
--   f @ arg = case arg of
--     Const _ -> arg
--     Plus lhs rhs -> Plus (f @ lhs) (f @ rhs)
--     Var _ -> arg
--     TLam {} -> error "TLam is not supported"
--     TApp { lhs, ltArgs, tyArgs } -> TApp { lhs = f @ lhs, ltArgs = f @ ltArgs, tyArgs = f @ tyArgs }
--     Ctor { name, ltArgs, tyArgs, args } -> Ctor { name, ltArgs = f @ ltArgs, tyArgs = f @ tyArgs, args = f @ args }
--     CapCtor { name, tyArgs, marker, handler } -> CapCtor { name, tyArgs = f @ tyArgs, marker, handler = f @ handler }
--     Lam { ctxParams, params, body } -> Lam { ctxParams = f @ ctxParams, params = f @ params, body = f @ body }
--     App { callee, ctxArgs, args } -> App { callee = f @ callee, ctxArgs = f @ ctxArgs, args = f @ args }
--     Match { scrutinee, branches } -> Match { scrutinee = f @ scrutinee, branches = f @ branches }
--     Perform { opName, cap, tyArgs, args } -> Perform { opName, cap = f @ cap, tyArgs = f @ tyArgs, args = f @ args }
--     Handle { capName, effTy, handler, body } -> Handle { capName, effTy = f @ effTy, handler = f @ handler, body = f @ body }
--     RtHandler { marker, body } -> RtHandler { marker, body = f @ body }

-- instance DoSubst target => Apply (Subst target) HandlerEntry HandlerEntry where
--   f @ HandlerEntry { opName, body } = HandlerEntry { opName, body = f @ body }

-- instance DoSubst target => Apply (Subst target) Param Param where
--   f @ Param { name, ty } = Param { name, ty = f @ ty }

-- instance DoSubst target => Apply (Subst target) Branch Branch where
--   f @ Branch { ctorName, varPatterns, body } = Branch { ctorName, varPatterns, body = f @ body }
