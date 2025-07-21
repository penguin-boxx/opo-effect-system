module Typing where

import Common
import Types
import Syntax
import Subst

import Control.Monad
import Control.Monad.Except
import Data.List qualified as List
import Optics

inferExpr :: MonadError String m => EffCtx -> TyCtx -> Expr -> m TySchema
inferExpr _ tyCtx = \case
  TApp { lhs = Var varName, ltArgs, tyArgs } -> do
    TySchema { ltParams, tyParams, ty } <- tyCtx `tyCtxLookupSchema` varName
    ltSubst <- mkSubst ltParams ltArgs
    tySubst <- mkSubst (toListOf (each % #name) tyParams) tyArgs
    let bounds = ltSubst @ toListOf (each % #bound) tyParams
    checkBounds tyCtx tyArgs bounds
    pure $ emptyTySchema $ ltSubst @ (tySubst @ ty)

  unsupported -> error $ "Unsupported construct: " <> show unsupported

checkBounds :: MonadError String m => TyCtx -> [MonoTy] -> [MonoTy] -> m ()
checkBounds tyCtx args bounds = do
  unless (length args == length bounds) $
    throwError "Arguments and parameters number mismatch"
  unless (and $ zipWith (subTyOf tyCtx) args bounds) $
    throwError "Type argument do not satisfy bound"

subTyOf :: TyCtx -> MonoTy -> MonoTy -> Bool
subTyOf _ _ _ = True -- TODO

tyCtxLookupSchema :: MonadError String m => TyCtx -> TyName -> m TySchema
tyCtxLookupSchema tyCtx keyName = case tyCtx of
  [] -> throwError $ "Name not found " <> keyName
  TyCtxVar { name, tySchema } : _ | name == keyName -> pure tySchema
  TyCtxCap { name, monoTy } : _ | name == keyName -> pure (emptyTySchema monoTy)
  _ : rest -> rest `tyCtxLookupSchema` keyName


