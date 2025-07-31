module Typing where

import Common
import Types
import Syntax
import Subst

import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Foldable
import Data.Function (fix)
import Data.List qualified as List
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Optics

inferExpr :: MonadError String m => EffCtx -> TyCtx -> Expr -> m TySchema
inferExpr effCtx tyCtx = \case
  Const _ -> pure $ emptyTySchema $ TyCtor MkTyCtor { name = "Int", lt = LtFree, args = [] }
  Var name -> tyCtx `tyCtxLookupSchema` name
  TApp MkTApp { lhs = Var varName, ltArgs, tyArgs } -> do
    MkTySchema { ltParams, tyParams, ty } <- tyCtx `tyCtxLookupSchema` varName
    ltSubst <- mkSubst ltParams ltArgs
    tySubst <- mkSubst (toListOf (each % #name) tyParams) tyArgs
    let bounds = ltSubst @ toListOf (each % #bound) tyParams
    checkBounds tyCtx tyArgs bounds
    pure $ emptyTySchema $ ltSubst @ (tySubst @ ty)
  Lam MkLam { ctxParams, params, body } -> do
    let tyCtx' =
          map (paramsToTyCtxEntry True) ctxParams ++
          map (paramsToTyCtxEntry False) params ++
          tyCtx
    res <- ensureMonoTy =<< inferExpr effCtx tyCtx' body
    let boundVars = foldMapOf (folded % #name) Set.singleton (ctxParams <> params)
    let freeVarNames = freeVars body \\ boundVars
    freeVarsSchemas <- mapM (tyCtxLookupSchema tyCtx') (Set.toList freeVarNames)
    freeLts <- mconcat <$> mapM (lifetimes tyCtx' PositivePos) freeVarsSchemas
    let capturingLt = LtIntersect $ Set.toList freeLts
    resLts <- lifetimes tyCtx' PositivePos (emptyTySchema res)
    when (LtLocal `Set.member` resLts) $
      throwError "Tracked value escapes via return value"
    pure $ emptyTySchema $ TyFun MkTyFun
      { ctx = toListOf (each % #ty) ctxParams, lt = capturingLt
      , args = toListOf (each % #ty) params, res }
  TLam MkTLam { ltParams, tyParams, body } -> do
    ty <- ensureMonoTy =<< inferExpr effCtx (fmap TyCtxTy tyParams ++ tyCtx) body
    pure MkTySchema { ltParams, tyParams, ty }
  App MkApp { callee, ctxArgs, args } -> do
    (expectedCtxArgs, expectedArgs, res) <- inferExpr effCtx tyCtx callee >>= ensureMonoTy >>= \case
      TyFun MkTyFun { ctx, args, res } -> pure (ctx, args, res)
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
  TApp MkTApp { lhs, ltArgs, tyArgs } -> do
    MkTySchema { ltParams, tyParams, ty } <- inferExpr effCtx tyCtx lhs
    unless (length tyParams == length tyArgs) $
      throwError "Type arguments number mismatch"
    ltSubst <- mkSubst ltParams ltArgs
    tyParamNames <- forM (zip tyParams tyArgs) \(MkTyParam { name, bound }, arg) -> do
      unless (subTyOf tyCtx arg (ltSubst @ bound)) $
        throwError $ "Type argument " <> show arg <> " is not a subtype of bound " <> show bound
      pure name
    tySubst <- mkSubst tyParamNames tyArgs
    pure $ emptyTySchema $ ltSubst @ (tySubst @ ty)
  Match MkMatch { scrutinee, branches } -> do
    MkTyCtor { name = tyCtor, lt, args = tyArgs } <- inferExpr effCtx tyCtx scrutinee >>= ensureMonoTy >>= \case
      TyCtor ctor -> pure ctor -- todo check optics for this
      other -> throwError $ "Expected type ctor, got " <> show other
    when (null branches) $
      throwError "Pattern matching should contain at least one branch" -- how to type it otherwise?
    resTys <- forM branches \MkBranch{ ctorName, varPatterns, body } -> do
      MkTyCtxCtor { name = branchTyCtor, tyParams, params } <- tyCtx `tyCtxLookupCtor` ctorName
      unless (branchTyCtor == tyCtor) $
        throwError $ "Pattern has wrong type: " <> branchTyCtor
      unless (length varPatterns == length params) $
        throwError "Number of patterns mismatch"
      tySubst <- mkSubst (toListOf (each % #name) tyParams) tyArgs
      params' <- forM params \param -> do
        posLts <- Set.toList <$> freeLtVars tyCtx PositivePos param
        negLts <- Set.toList <$> freeLtVars tyCtx NegativePos param
        posSubst <- mkSubst posLts (repeat lt)
        negSubst <- mkSubst negLts (repeat LtFree)
        pure $ tySubst @ (posSubst @ (negSubst @ param)) -- todo check order
      let mkCtxVar name param = TyCtxVar MkTyCtxVar { name, tySchema = emptyTySchema param }
      let tyCtx' = zipWith mkCtxVar varPatterns params' ++ tyCtx
      inferExpr effCtx tyCtx' body
    unless (and $ zipWith (==) resTys (drop 1 resTys)) $
      throwError "Branch result types shouls be equal" -- todo LUB
    pure $ head resTys

  unsupported -> error $ "Unsupported construct: " <> show unsupported

checkBounds :: MonadError String m => TyCtx -> [MonoTy] -> [MonoTy] -> m ()
checkBounds tyCtx args bounds = do
  unless (length args == length bounds) $
    throwError "Arguments and parameters number mismatch"
  forM_ (zip args bounds) \(arg, bound) ->
    unless (subTyOf tyCtx arg bound) $
      throwError $ "Type argument " <> show arg <> " do not satisfy bound " <> show bound

subTyOf :: TyCtx -> MonoTy -> MonoTy -> Bool
subTyOf tyCtx = curry \case
  (TyVar name1, TyVar name2) -> name1 == name2;
  ( TyCtor MkTyCtor { name = ctor1, lt = lt1, args = args1 },
    TyCtor MkTyCtor { name = ctor2, lt = lt2, args = args2 } ) ->
    ctor1 `subTyCtorOf` ctor2 && lt1 `subLtOf` lt2 && args1 == args2
  ( TyFun MkTyFun { lt = lt1, args = args1, res = res1 },
    TyFun MkTyFun { lt = lt2, args = args2, res = res2 } ) ->
    and $ zipWith (subTyOf tyCtx) args2 args1 ++
      [ lt1 `subLtOf` lt2
      , length args1 == length args2
      , subTyOf tyCtx res1 res2
      ] -- TODO ctx
  _ -> False

subTyCtorOf :: CtorName -> CtorName -> Bool
subTyCtorOf ctor1 ctor2 = ctor1 == ctor2 || ctor2 == "Any"

subLtOf :: Lt -> Lt -> Bool
subLtOf = curry \case
  (LtVar name1, LtVar name2) -> name1 == name2
  (LtIntersect lts, lt) -> all (`subLtOf` lt) lts
  (lt, LtIntersect lts) -> any (lt `subLtOf`) lts
  (lt1, lt2) -> lt1 == LtFree || lt2 == LtLocal

tyCtxLookupSchema :: MonadError String m => TyCtx -> VarName -> m TySchema
tyCtxLookupSchema tyCtx targetName = case tyCtx of
  [] -> throwError $ "Name not found " <> targetName
  TyCtxVar MkTyCtxVar { name, tySchema } : _ | name == targetName -> pure tySchema
  TyCtxCap MkTyCtxCap { name, monoTy } : _ | name == targetName -> pure (emptyTySchema monoTy)
  TyCtxCtor MkTyCtxCtor { name, ltParams, tyParams, params, res } : _ | name == targetName ->
    pure MkTySchema
      { ltParams, tyParams
      , ty = TyFun MkTyFun { ctx = [], lt = LtFree, args = params, res = TyCtor res }
      }
  _ : rest -> rest `tyCtxLookupSchema` targetName

tyCtxLookupBound :: MonadError String m => TyCtx -> TyName -> m MonoTy
tyCtxLookupBound tyCtx targetName = case tyCtx of
  [] -> throwError $ "Name not found " <> targetName
  TyCtxTy MkTyParam { name, bound } : _ | name == targetName -> pure bound
  _ : rest -> rest `tyCtxLookupBound` targetName

tyCtxLookupCtor :: MonadError String m => TyCtx -> CtorName -> m TyCtxCtor
tyCtxLookupCtor tyCtx targetName = case tyCtx of
  [] -> throwError $ "Ctor not found " <> targetName
  TyCtxCtor ctor : _ -> pure ctor
  _ : rest -> rest `tyCtxLookupCtor` targetName

paramsToTyCtxEntry :: Bool -> Param -> TyCtxEntry
paramsToTyCtxEntry contextual MkParam { name, ty }
  | contextual = TyCtxCap MkTyCtxCap { name, monoTy = ty }
  | otherwise = TyCtxVar MkTyCtxVar { name, tySchema = emptyTySchema ty }

ensureMonoTy :: MonadError String m => TySchema -> m MonoTy
ensureMonoTy = \case
  MkTySchema { ltParams = [], tyParams = [], ty } -> pure ty
  schema -> throwError $ "Expected mono type, got " <> show schema

freeVars :: Expr -> Set VarName
freeVars = \case
  Const _ -> Set.empty
  Plus lhs rhs -> freeVars lhs <> freeVars rhs
  Var name -> Set.singleton name
  TLam MkTLam { body } -> freeVars body
  TApp MkTApp { lhs } -> freeVars lhs
  CapCtor MkCapCtor { } -> error "Unused"
  Lam MkLam { ctxParams, params, body } ->
    freeVars body \\ foldMapOf (each % #name) Set.singleton (ctxParams <> params)
  App MkApp { callee, ctxArgs, args } ->
    freeVars callee <> foldMap freeVars ctxArgs <> foldMap freeVars args
  other -> error $ "unsupported " <> show other

typesOf :: MonadError String m => TyCtx -> Set VarName -> m (Set TySchema)
typesOf tyCtx (Set.toList -> vars) =
  Set.fromList <$> mapM (tyCtxLookupSchema tyCtx) vars

data PositionSign = PositivePos | NegativePos deriving Eq

changeSign :: PositionSign -> PositionSign
changeSign = \case PositivePos -> NegativePos; NegativePos -> PositivePos

freeLtVars :: MonadError String m => TyCtx -> PositionSign -> MonoTy -> m (Set LtName)
freeLtVars tyCtx expectedSign =
  fmap (foldMap f) . lifetimes tyCtx expectedSign . emptyTySchema
  where
    f = \case
      LtVar name -> Set.singleton name
      LtLocal -> Set.empty
      LtFree -> Set.empty
      LtIntersect lts -> foldMap f lts

lifetimes :: MonadError String m => TyCtx -> PositionSign -> TySchema -> m (Set Lt)
lifetimes tyCtx expectedSign MkTySchema { ltParams, tyParams, ty } = do
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
      TyCtor MkTyCtor { lt, args } -> do
        -- Type parameters are invariant, include them in both ways.
        forM_ args (lifetimesMono currSign)
        forM_ args (lifetimesMono $ changeSign currSign)
        when (currSign == expectedSign) $
          tell $ Set.singleton lt
      TyFun MkTyFun { ctx, lt, args, res } -> do
        forM_ (ctx <> args) (lifetimesMono (changeSign currSign))
        when (currSign == expectedSign) $
          tell $ Set.singleton lt
        lifetimesMono currSign res
