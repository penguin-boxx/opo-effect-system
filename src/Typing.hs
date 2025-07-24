module Typing where

import Common
import Types
import Syntax
import Subst

import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Function (fix)
import Data.List qualified as List
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Optics

inferExpr :: MonadError String m => EffCtx -> TyCtx -> Expr -> m TySchema
inferExpr effCtx tyCtx = \case
  TApp { lhs = Var varName, ltArgs, tyArgs } -> do
    TySchema { ltParams, tyParams, ty } <- tyCtx `tyCtxLookupSchema` varName
    ltSubst <- mkSubst ltParams ltArgs
    tySubst <- mkSubst (toListOf (each % #name) tyParams) tyArgs
    let bounds = ltSubst @ toListOf (each % #bound) tyParams
    checkBounds tyCtx tyArgs bounds
    pure $ emptyTySchema $ ltSubst @ (tySubst @ ty)
  Lam { ctxParams, params, body } -> do
    let tyCtx' = map (paramsToTyCtxEntry True) ctxParams ++ map (paramsToTyCtxEntry False) params ++ tyCtx
    res <- ensureMonoTy =<< inferExpr effCtx tyCtx' body
    freeVarsSchemas <- mapM (tyCtxLookupSchema tyCtx') (Set.toList $ freeVars body)
    freeLts <- mconcat <$> mapM (lifetimes tyCtx' PositivePos) freeVarsSchemas
    let capturingLt = LtIntersect $ Set.toList freeLts
    resLts <- lifetimes tyCtx' PositivePos (emptyTySchema res)
    when (LtLocal `Set.member` resLts) $
      throwError "Tracked value escapes via return value"
    pure $ emptyTySchema $ TyFun
      { ctx = toListOf (each % #ty) ctxParams, lt = capturingLt
      , args = toListOf (each % #ty) params, res }
  App { callee, ctxArgs, args } -> do
    (expectedCtxArgs, expectedArgs, res) <- inferExpr effCtx tyCtx callee >>= ensureMonoTy >>= \case
      TyFun { ctx, args, res } -> pure (ctx, args, res)
      other -> throwError $ "Expected function, got " <> show other
    actualCtxArgs <- mapM (ensureMonoTy <=< inferExpr effCtx tyCtx) ctxArgs
    actualArgs <- mapM (ensureMonoTy <=< inferExpr effCtx tyCtx) args
    unless (length actualCtxArgs == length expectedCtxArgs) $
      throwError "Ctx arguments number mismatch"
    unless (length actualArgs == length expectedArgs) $
      throwError "Arguments number mismatch"
    forM_ (zip (actualCtxArgs <> actualArgs) (expectedCtxArgs <> expectedArgs)) \(actual, expected) ->
      unless (subTyOf tyCtx actual expected) $
        throwError $ "Type mismatch: " <> show actual <> " is not a subtype of " <> show expected
    pure $ emptyTySchema res
  unsupported -> error $ "Unsupported construct: " <> show unsupported

checkBounds :: MonadError String m => TyCtx -> [MonoTy] -> [MonoTy] -> m ()
checkBounds tyCtx args bounds = do
  unless (length args == length bounds) $
    throwError "Arguments and parameters number mismatch"
  unless (and $ zipWith (subTyOf tyCtx) args bounds) $
    throwError "Type argument do not satisfy bound"

subTyOf :: TyCtx -> MonoTy -> MonoTy -> Bool
subTyOf tyCtx = curry \case
  (TyVar name1, TyVar name2) -> name1 == name2
  (TyCtor { ctor = ctor1, lt = lt1, args = args1 }, TyCtor { ctor = ctor2, lt = lt2, args = args2 }) ->
    ctor1 `subTyCtorOf` ctor2 && lt1 `subLtOf` lt2 && args1 == args2
  (TyFun { lt = lt1, args = args1, res = res1 }, TyFun { lt = lt2, args = args2, res = res2}) ->
    lt1 `subLtOf` lt2 && length args1 == length args2 && and (zipWith (subTyOf tyCtx) args2 args1) && subTyOf tyCtx res1 res2 -- TODO ctx
  _ -> False

subTyCtorOf :: CtorName -> CtorName -> Bool
subTyCtorOf ctor1 ctor2 = ctor1 == ctor2 || ctor1 == "Any"

subLtOf :: Lt -> Lt -> Bool
subLtOf = curry \case
  (LtVar name1, LtVar name2) -> name1 == name2
  (LtIntersect lts, lt) -> all (`subLtOf` lt) lts
  (lt, LtIntersect lts) -> any (lt `subLtOf`) lts
  (lt1, lt2) -> lt1 == LtFree || lt2 == LtLocal

tyCtxLookupSchema :: MonadError String m => TyCtx -> VarName -> m TySchema
tyCtxLookupSchema tyCtx keyName = case tyCtx of
  [] -> throwError $ "Name not found " <> keyName
  TyCtxVar { name, tySchema } : _ | name == keyName -> pure tySchema
  TyCtxCap { name, monoTy } : _ | name == keyName -> pure (emptyTySchema monoTy)
  _ : rest -> rest `tyCtxLookupSchema` keyName

tyCtxLookupBound :: MonadError String m => TyCtx -> TyName -> m MonoTy
tyCtxLookupBound tyCtx keyName = case tyCtx of
  [] -> throwError $ "Name not found " <> keyName
  TyCtxTy { name, bound } : _ | name == keyName -> pure bound
  _ : rest -> rest `tyCtxLookupBound` keyName

paramsToTyCtxEntry :: Bool -> Param -> TyCtxEntry
paramsToTyCtxEntry contextual Param { name, ty }
  | contextual = TyCtxCap { name, monoTy = ty }
  | otherwise = TyCtxVar { name, tySchema = emptyTySchema ty }

ensureMonoTy :: MonadError String m => TySchema -> m MonoTy
ensureMonoTy = \case
  TySchema { ltParams = [], tyParams = [], ty } -> pure ty
  schema -> throwError $ "Expected mono type, got " <> show schema

freeVars :: Expr -> Set VarName
freeVars = \case
  Const _ -> Set.empty
  Plus lhs rhs -> freeVars lhs <> freeVars rhs
  Var name -> Set.singleton name
  TLam { body } -> freeVars body
  TApp { lhs } -> freeVars lhs
  Ctor { args } -> foldMap freeVars args
  CapCtor { } -> error "Unused"
  Lam { ctxParams, params, body } ->
    freeVars body \\ foldMapOf (each % #name) Set.singleton (ctxParams <> params)
  App { callee, ctxArgs, args } ->
    freeVars callee <> foldMap freeVars ctxArgs <> foldMap freeVars args
  other -> error $ "unsupported " <> show other

typesOf :: MonadError String m => TyCtx -> Set VarName -> m (Set TySchema)
typesOf tyCtx (Set.toList -> vars) =
  Set.fromList <$> mapM (tyCtxLookupSchema tyCtx) vars

data PositionSign = PositivePos | NegativePos deriving Eq

changeSign :: PositionSign -> PositionSign
changeSign = \case PositivePos -> NegativePos; NegativePos -> PositivePos

lifetimes :: MonadError String m => TyCtx -> PositionSign -> TySchema -> m (Set Lt)
lifetimes tyCtx expectedSign TySchema{ ltParams, tyParams, ty } = do
  allLts <- execWriterT $ lifetimesMono PositivePos ty
  pure $ foldr (Set.delete . LtVar) allLts ltParams
  where
    lifetimesMono
      :: (MonadWriter (Set Lt) m, MonadError String m)
      => PositionSign -> MonoTy -> m ()
    lifetimesMono currSign = \case
      TyVar name ->
        unless (each % #name `elemOf` name $ tyParams) do
          bound <- tyCtx `tyCtxLookupBound` name
          lifetimesMono currSign bound
      TyCtor { lt, args } -> do
        -- Type parameters are invariant, include them in both ways.
        forM_ args (lifetimesMono currSign)
        forM_ args (lifetimesMono $ changeSign currSign)
        when (currSign == expectedSign) $
          tell $ Set.singleton lt
      TyFun { ctx, lt, args, res } -> do
        forM_ (ctx <> args) (lifetimesMono (changeSign currSign))
        when (currSign == expectedSign) $
          tell $ Set.singleton lt
        lifetimesMono currSign res
