module TypingUtils where

import Common
import Syntax
import Types
import TypingCtx

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Foldable
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Maybe
import Optics
import Data.Kind
import GHC.Stack


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
    throwError "Unexpected number of parameters"
  pure $ Subst $ Map.fromList $ zip names target

mkSubst2 :: MonadError String m => [String] -> [target] -> [String] -> [target] -> m (Subst target)
mkSubst2 names1 target1 names2 target2 = do
  unless (length names1 == length target1 && length names2 == length target2) $
    throwError "Unexpected number of parameters"
  pure $ Subst $ Map.fromList $ zip names1 target1 ++ zip names2 target2

instance DoSubst target => Apply (Subst target) Lt Lt where
  f @ arg = case arg of
    LtLocal -> LtLocal
    LtMin names -> foldr lub ltFree $ (\name -> onLt f name (ltVar name)) <$> Set.toList names

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
    TLam {} -> error "TLam is not supported" -- TODO
    TApp MkTApp { lhs, ltArgs, tyArgs } -> TApp MkTApp { lhs = f @ lhs, ltArgs = f @ ltArgs, tyArgs = f @ tyArgs }
    CapCtor MkCapCtor { name, tyArgs, marker, handler } -> CapCtor MkCapCtor { name, tyArgs = f @ tyArgs, marker, handler = f @ handler }
    Lam MkLam { ctxParams, params, body } -> Lam MkLam { ctxParams = f @ ctxParams, params = f @ params, body = f @ body }
    App MkApp { callee, ctxArgs, args } -> App MkApp { callee = f @ callee, ctxArgs = f @ ctxArgs, args = f @ args }
    Match MkMatch { scrutinee, branches } -> Match MkMatch { scrutinee = f @ scrutinee, branches = f @ branches }
    Perform MkPerform { opName, cap, tyArgs, args } -> Perform MkPerform { opName, cap = f @ cap, tyArgs = f @ tyArgs, args = f @ args }
    Handle MkHandle { capName, effTy, handler, body } -> Handle MkHandle { capName, effTy = f @ effTy, handler = f @ handler, body = f @ body }
    RtHandler MkRtHandler { marker, body } -> RtHandler MkRtHandler { marker, body = f @ body }

instance DoSubst target => Apply (Subst target) HandlerEntry HandlerEntry where
  f @ MkHandlerEntry { opName, paramNames, body } = MkHandlerEntry { opName, paramNames, body = f @ body }

instance DoSubst target => Apply (Subst target) Param Param where
  f @ MkParam { name, ty } = MkParam { name, ty = f @ ty }

instance DoSubst target => Apply (Subst target) Branch Branch where
  f @ MkBranch { ctorName, varPatterns, body } = MkBranch { ctorName, varPatterns, body = f @ body }

instance DoSubst target => Apply (Subst target) OpSig OpSig where
  f @ MkOpSig { tyParams, args, res } = MkOpSig { tyParams, args, res = f @ res } -- TODO


infix 5 `lub`
class LeastUpperBound ty where
  type LubC ty :: Constraint
  lub :: (LubC ty, HasCallStack) => ty -> ty -> ty

instance LeastUpperBound Lt where
  type LubC Lt = ()
  lub LtLocal _ = LtLocal
  lub _ LtLocal = LtLocal
  lub (LtMin names1) (LtMin names2) = LtMin (names1 <> names2)

instance LeastUpperBound MonoTy where
  type LubC MonoTy = (?tyCtx :: TyCtx)
  lub var@(TyVar name1) (TyVar name2)
    | name1 == name2 = var
    | otherwise = ?tyCtx `lookupBound` name1 `lub` ?tyCtx `lookupBound` name2
  lub
    (TyCtor MkTyCtor { name = name1, lt = lt1, args = args1 })
    (TyCtor MkTyCtor { name = name2, lt = lt2, args = args2 })
    | name1 == name2 && args1 == args2 =
      TyCtor MkTyCtor { name = name1, lt = lt1 `lub` lt2, args = args1 }
  lub
    (TyFun MkTyFun { ctx = ctx1, lt = lt1, args = args1, res = res1 })
    (TyFun MkTyFun { ctx = ctx2, lt = lt2, args = args2, res = res2 })
    | ctx1 == ctx2 && args1 == args2 = -- TODO greatest lower bound
      TyFun MkTyFun { ctx = ctx1, lt = lt1 `lub` lt2, args = args1, res = res1 `lub` res2 }
  lub ty1 ty2 =
    let lt = lubAll (ty1 `ltsAt` PositivePos) `lub` lubAll (ty2 `ltsAt` PositivePos) in
    TyCtor MkTyCtor { name = "Any", lt, args = [] }

lubAll :: Foldable f => f Lt -> Lt
lubAll = foldr lub ltFree


class MonadFresh res m where
  fresh :: m res

runFreshT :: Functor m => StateT Int m a -> m a
runFreshT = fmap fst . flip runStateT 0

instance Monad m => MonadFresh LtName (StateT Int m) where
  fresh = do
    curr <- get
    modify' (+1)
    pure $ "$l" <> show curr


infix 6 `subTyOf`
subTyOf :: (?tyCtx :: TyCtx) => MonoTy -> MonoTy -> Bool
subTyOf = curry \case
  (ty, TyCtor MkTyCtor { name = "Any", lt = lt2 }) ->
    lubAll (ty `ltsAt` PositivePos) `subLtOf` lt2

  (TyVar name1, TyVar name2) -> name1 == name2
  (TyVar name1, ty) -> (?tyCtx `lookupBound` name1) `subTyOf` ty

  ( TyCtor MkTyCtor { name = ctor1, lt = lt1, args = args1 },
    TyCtor MkTyCtor { name = ctor2, lt = lt2, args = args2 } ) ->
    ctor1 == ctor2 && lt1 `subLtOf` lt2 && args1 == args2

  ( TyFun MkTyFun { ctx = ctx1, lt = lt1, args = args1, res = res1 },
    TyFun MkTyFun { ctx = ctx2, lt = lt2, args = args2, res = res2 } ) -> and
    [ length ctx1 == length ctx2
    , and $ zipWith subTyOf ctx2 ctx1
    , length args1 == length args2
    , and $ zipWith subTyOf args2 args1
    , lt1 `subLtOf` lt2
    , res1 `subTyOf` res2
    ]

  _ -> False

subTySchemaOf :: TySchema -> TySchema -> Bool
subTySchemaOf
  MkTySchema{ ltParams = ltParams1, tyParams = tyParams1, ty = ty1 }
  MkTySchema{ ltParams = ltParams2, tyParams = tyParams2, ty = ty2 } =
  let ?tyCtx = TyCtxTy <$> tyParams2 in
  ltParams1 == ltParams2 && tyParams1 == tyParams2 && ty1 `subTyOf` ty2

subLtOf :: (?tyCtx :: TyCtx) => Lt -> Lt -> Bool
subLtOf = curry \case
  (LtMin lts1, lt2@(LtMin lts2)) -> flip all lts1 \name ->
    name `Set.member` lts2 || (?tyCtx `lookupBound` name) `subLtOf` lt2
  (_, lt) -> lt == LtLocal

paramsToTyCtxEntry :: Bool -> Param -> TyCtxEntry
paramsToTyCtxEntry contextual MkParam { name, ty }
  | contextual = TyCtxCap MkTyCtxCap { name, monoTy = ty }
  | otherwise = TyCtxVar MkTyCtxVar { name, tySchema = emptyTySchema ty }

ensureMonoTy :: (HasCallStack, MonadError String m) => TySchema -> m MonoTy
ensureMonoTy = \case
  MkTySchema { ltParams = [], tyParams = [], ty } -> pure ty
  schema -> throwError $ "Expected mono type, got " <> show schema <> " at\n" <> prettyCallStack callStack

freeVarsOf :: HasCallStack => Expr -> Set VarName
freeVarsOf = \case
  Const _ -> Set.empty
  Plus lhs rhs -> freeVarsOf lhs <> freeVarsOf rhs
  Var name -> Set.singleton name
  TLam MkTLam { body } -> freeVarsOf body
  TApp MkTApp { lhs } -> freeVarsOf lhs
  CapCtor MkCapCtor { } -> error "Unused"
  Lam MkLam { ctxParams, params, body } ->
    freeVarsOf body \\ toSetOf (folded % #name) (ctxParams <> params)
  App MkApp { callee, ctxArgs, args } ->
    freeVarsOf callee <> foldMap freeVarsOf ctxArgs <> foldMap freeVarsOf args
  Match MkMatch { scrutinee, branches } ->
    freeVarsOf scrutinee <> flip foldMap branches \MkBranch{ varPatterns, body } ->
      freeVarsOf body \\ Set.fromList varPatterns
  Perform MkPerform { cap, args } ->
    freeVarsOf cap <> foldMap freeVarsOf args
  Handle MkHandle { capName, handler, body } ->
    capName `Set.delete` freeVarsOf body <>
    flip foldMap handler \MkHandlerEntry { paramNames, body } ->
      freeVarsOf body \\ Set.fromList ("resume" : paramNames)
  other -> error $ "unsupported free vars " <> show other

data PositionSign = PositivePos | NegativePos deriving Eq

changeSign :: PositionSign -> PositionSign
changeSign = \case PositivePos -> NegativePos; NegativePos -> PositivePos

freeLtVarsOf
  :: (HasCallStack, ?tyCtx :: TyCtx)
  => MonoTy -> Set LtName
freeLtVarsOf ty =
  let lts = ty `ltsAt` PositivePos <> ty `ltsAt` NegativePos in
  folded % _LtMin % folded `toSetOf` lts

class LifetimesAt ty where
  ltsAt
    :: (HasCallStack, ?tyCtx :: TyCtx)
    => ty -> PositionSign -> Set Lt

instance LifetimesAt ty => LifetimesAt [ty] where
  ltsAt tys expectedSign = fold $ tys <&> (`ltsAt` expectedSign)

instance LifetimesAt TySchema where
  ltsAt MkTySchema{ ltParams, tyParams, ty } expectedSign =
    let ?tyCtx = filterVars (each % #name `toSetOf` tyParams) ?tyCtx in
    let lts :: Set Lt = ty `ltsAt` expectedSign in
    flip foldMap lts \case
      LtMin names -> Set.singleton $ LtMin (names \\ Set.fromList ltParams)
      lt -> Set.singleton lt

instance LifetimesAt MonoTy where
  ltsAt monoTy expectedSign = execWriter $ monoTy `goFrom` PositivePos
    where
      goFrom :: MonadWriter (Set Lt) m => MonoTy -> PositionSign -> m ()
      goFrom ty currSign = case ty of
        TyVar name ->
          runExceptT (?tyCtx `lookupBound` name) >>= \case
            Left _ -> pure () -- ignore bounds of unknown type parameters
            Right bound -> bound `goFrom` currSign
        TyCtor MkTyCtor { lt, args } -> do
          when (currSign == expectedSign) $
            tell $ Set.singleton lt
          -- Type parameters are invariant, include them in both ways.
          forM_ args (`goFrom` currSign)
          forM_ args (`goFrom` changeSign currSign)
        TyFun MkTyFun { ctx, lt, args, res } -> do
          forM_ (ctx <> args) (`goFrom` changeSign currSign)
          when (currSign == expectedSign) $
            tell $ Set.singleton lt
          res `goFrom` currSign

freeTyVarsAt :: MonoTy -> PositionSign -> Set TyName
freeTyVarsAt ty expectedSign = execWriter (ty `goAt` PositivePos)
  where
    goAt :: MonadWriter (Set TyName) m => MonoTy -> PositionSign -> m ()
    goAt ty currSign = case ty of
      TyVar name ->
        when (currSign == expectedSign) $
          tell $ Set.singleton name
      TyCtor MkTyCtor { args } -> do
        forM_ args (`goAt` currSign)
        forM_ args (`goAt` changeSign currSign)
      TyFun MkTyFun { ctx, args, res } -> do
        forM_ ctx (`goAt` changeSign currSign)
        forM_ args (`goAt` changeSign  currSign)
        res `goAt` currSign
