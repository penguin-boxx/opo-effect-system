module Typing where

import Common
import Types
import TypingCtx
import Syntax
import TypingUtils

import Control.Monad
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Foldable
import Data.Function (fix)
import Data.List qualified as List
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Map ((!?))
import Data.Map qualified as Map
import Debug.Trace
import GHC.Stack
import Optics
import Prelude hiding (lookup)

inferExpr
  :: (MonadError String m, HasCallStack, ?effCtx :: EffCtx, ?tyCtx :: TyCtx)
  => Expr -> m TySchema
inferExpr = \case
  Const _ ->
    pure $ emptyTySchema $ TyCtor MkTyCtor { name = "Int", lt = LtFree, args = [] }

  Var name -> ?tyCtx `lookup` name

  TLam MkTLam { ltParams, tyParams, body } -> do
    let ?tyCtx = fmap TyCtxTy tyParams ++ ?tyCtx
    ty <- inferExpr body >>= ensureMonoTy
    pure MkTySchema { ltParams, tyParams, ty }

  TApp MkTApp { lhs, ltArgs, tyArgs } -> do
    MkTySchema { ltParams, tyParams, ty } <- inferExpr lhs
    ltSubst <- mkSubst ltParams ltArgs
    tyParamNames <- forM (zip tyParams (ltSubst @ tyArgs)) \(MkTyParam { name, bound }, arg) -> do
      unless (arg `subTyOf` bound) $
        throwError $ "Type argument " <> show arg <> " is not a subtype of bound " <> show bound
      pure name
    tySubst <- mkSubst tyParamNames tyArgs
    pure $ emptyTySchema $ tySubst @ (ltSubst @ ty)

  Lam MkLam { ctxParams, params, body } -> do
    let ctxDiff = map (paramsToTyCtxEntry True) ctxParams <> map (paramsToTyCtxEntry False) params
    res <- let ?tyCtx = ctxDiff ++ ?tyCtx in inferExpr body >>= ensureMonoTy
    -- Do not consider bounds in escape checking.
    let resLts = let ?tyCtx = [] in emptyTySchema res `lifetimesOn` PositivePos
    when (LtLocal `Set.member` resLts) $
      throwError $ "Tracked value escapes via return value of type " <> show res
    let boundVars = folded % #name `toSetOf` (ctxParams <> params)
    let freeVarNames = freeVars body \\ boundVars
    freeVarsSchemas <- mapM (?tyCtx `lookup`) (Set.toList freeVarNames)
    let paramFreeTyVars = (ctxParams <> params) & folded % #ty `foldMapOf` (`freeTyVarsAt` NegativePos)
    let resFreeTyVars = res `freeTyVarsAt` PositivePos
    let freeLts =
          let ?tyCtx = filterVars (paramFreeTyVars <> resFreeTyVars) ?tyCtx in
          foldMap (`lifetimesOn` PositivePos) freeVarsSchemas
    pure $ emptyTySchema $ TyFun MkTyFun
      { ctx = each % #ty `toListOf` ctxParams
      , args = each % #ty `toListOf` params
      , lt = foldr lub LtFree freeLts
      , res
      }

  App MkApp { callee, ctxArgs, args } -> do
    MkTyFun { ctx = expectedCtxArgs, args = expectedArgs, res } <-
      inferExpr callee >>= ensureMonoTy >>= \case
        TyFun fun -> pure fun
        other -> throwError $ "Expected function, got " <> show other
    actualCtxArgs <- mapM (ensureMonoTy <=< inferExpr) ctxArgs
    actualArgs <- mapM (ensureMonoTy <=< inferExpr) args
    unless (length actualCtxArgs == length expectedCtxArgs) $
      throwError "Ctx arguments number mismatch"
    unless (length actualArgs == length expectedArgs) $
      throwError "Arguments number mismatch"
    forM_ (zip (actualCtxArgs <> actualArgs) (expectedCtxArgs <> expectedArgs)) \(actual, expected) ->
      unless (actual `subTyOf` expected) $
        throwError $ "Type mismatch: " <> show actual <> " is not a subtype of " <> show expected <> " from \n" <> prettyCallStack callStack
    pure $ emptyTySchema res

  Match MkMatch { scrutinee, branches } -> do
    MkTyCtor { name = tyCtor, lt, args = tyArgs } <-
      inferExpr scrutinee >>= ensureMonoTy >>= \case
        TyCtor ctor -> pure ctor
        other -> throwError $ "Expected type ctor, got " <> show other
    let ctorCandidates = ?tyCtx `lookup` tyCtor
    resTys <- forM branches \MkBranch{ ctorName, varPatterns, body } -> do
      MkTyCtxCtor { tyParams, params } <- ctorCandidates
        & List.find @[] (\MkTyCtxCtor { name } -> name == ctorName)
        & maybe (throwError $ "Ctor " <> ctorName <> " do not have expected type") pure
      unless (length varPatterns == length params) $
        throwError $ "Number of var patterns mismatch for " <> ctorName
      tySubst <- mkSubst (toListOf (each % #name) tyParams) tyArgs
      params' <- forM params \param -> do
        let posLts = param `freeLtVarsOn` PositivePos
        let negLts = param `freeLtVarsOn` NegativePos
        -- TODO
        -- unless (posLts `Set.disjoint` negLts) $
        --   throwError "A lifetime variable should not occur in both positive and negative positions"
        posSubst <- mkSubst (Set.toList posLts) (replicate (Set.size posLts) lt)
        negSubst <- mkSubst (Set.toList negLts) (replicate (Set.size negLts) LtFree)
        pure $ tySubst @ (posSubst @ (negSubst @ param))
      let mkCtxVar name param = TyCtxVar MkTyCtxVar { name, tySchema = emptyTySchema param }
      let ?tyCtx = zipWith mkCtxVar varPatterns params' ++ ?tyCtx
      inferExpr body >>= ensureMonoTy
    when (null resTys) $
      throwError "There should be at least one branch" -- todo make Bot
    pure $ emptyTySchema $ foldr1 lub resTys

  Perform MkPerform { opName, cap, tyArgs = opTyArgs, args } -> do
    MkTyCtor { name = effName, args = tyArgs } <-
      inferExpr cap >>= ensureMonoTy >>= \case
        TyCtor ctor -> pure ctor
        other -> throwError $ "Expected effect type, got " <> show other
    MkEffCtxEntry { tyParams, ops } <- ?effCtx `lookup` effName
    MkOpSig { tyParams = opTyParams, params, res } <- case ops !? opName of
      Nothing -> throwError $ "Effect " <> effName <> " do not include operation " <> opName
      Just op -> pure op
    subst <- mkSubst2 tyParams tyArgs opTyParams opTyArgs
    argTys <- mapM (ensureMonoTy <=< inferExpr) args
    unless (length params == length argTys) $
      throwError "Operation arguments number mismatch"
    forM_ (zip params argTys) \((subst @) -> param, arg) ->
      unless (arg `subTyOf` param) $
        throwError $ "Type mismatch: " <> show arg <> " is not subtype of " <> show param
    pure $ emptyTySchema $ subst @ res

  Handle MkHandle { capName, effTy, handler, body } -> do
    let MkTyCtor { name = effName, args = effTyArgs } = effTy
    unless (#lt % only LtLocal `has` effTy) $
      throwError "Capabilities can only have local lifetime"
    let capCtx = TyCtxCap MkTyCtxCap { name = capName, monoTy = TyCtor effTy }
    resTy <- let ?tyCtx = capCtx : ?tyCtx in inferExpr body >>= ensureMonoTy
    let resLts = let ?tyCtx = [] in emptyTySchema resTy `lifetimesOn` PositivePos
    when (LtLocal `Set.member` resLts) $
      throwError "Tracked value is escaping out of the handler body"
    let resFv = resTy `freeLtVarsOn` PositivePos <> resTy `freeLtVarsOn` NegativePos
    MkEffCtxEntry { tyParams = effTyParams, ops } <- ?effCtx `lookup` effName
    subst <- mkSubst effTyParams effTyArgs
    unless (length handler == Map.size ops) $
      throwError "Wrong number of implemented operations"
    forM_ handler \MkHandlerEntry { opName, paramNames, body } -> do
      MkOpSig { tyParams = opTyParams, params, res = opResTy } <-
        case ops !? opName of
          Nothing -> throwError $ "Operation " <> opName <> " is not specified for effect " <> effName
          Just sig -> pure (subst @ sig)
      unless (Set.fromList opTyParams `Set.disjoint` resFv) $
        throwError "Operation generics should not leak"
      unless (length paramNames == length params) $
        throwError "Operation parameter number mismatch"
      let opParamCtx = TyCtxVar <$> zipWith MkTyCtxVar paramNames (emptyTySchema <$> params)
      let resumeCtx = TyCtxVar MkTyCtxVar
            { name = "resume", tySchema = emptyTySchema $ TyFun MkTyFun
                { ctx = [], lt = LtFree, args = [opResTy], res = resTy }
            }
      opRetTy <- let ?tyCtx = opParamCtx ++ resumeCtx : ?tyCtx in
        inferExpr (subst @ body) >>= ensureMonoTy
      unless (opRetTy == resTy) $ -- todo
        throwError $ "Operation " <> opName <> " return type \n" <> show opRetTy <> "\nmismatch\n" <> show resTy
    pure $ emptyTySchema resTy

  unsupported -> error $ "Unsupported construct: " <> show unsupported

infix 6 `subTyOf`
subTyOf :: (?tyCtx :: TyCtx) => MonoTy -> MonoTy -> Bool
subTyOf = curry \case
  (TyVar name1, TyVar name2) -> name1 == name2
  (TyVar name1, ty) -> (?tyCtx `lookupBound'` name1) `subTyOf` ty
  ( TyCtor MkTyCtor { name = ctor1, lt = lt1, args = args1 },
    TyCtor MkTyCtor { name = ctor2, lt = lt2, args = args2 } ) ->
    ctor1 `subTyCtorOf` ctor2 && lt1 `subLtOf` lt2 && args1 == args2
  ( TyFun MkTyFun { lt = lt1, args = args1, res = res1 },
    TyFun MkTyFun { lt = lt2, args = args2, res = res2 } ) ->
    and $ zipWith subTyOf args2 args1 ++
      [ lt1 `subLtOf` lt2
      , length args1 == length args2
      , res1 `subTyOf` res2
      ] -- TODO ctx
  ( TyFun MkTyFun { lt = lt1 },
    TyCtor MkTyCtor { name = "Any", lt = lt2 } ) -> lt1 `subLtOf` lt2
  _ -> False

subTySchemaOf :: TySchema -> TySchema -> Bool
subTySchemaOf
  MkTySchema{ ltParams = ltParams1, tyParams = tyParams1, ty = ty1 }
  MkTySchema{ ltParams = ltParams2, tyParams = tyParams2, ty = ty2 } =
  -- TODO handle params properly
  let ?tyCtx = TyCtxTy <$> tyParams2 in
  ltParams1 == ltParams2 && tyParams1 == tyParams2 && ty1 `subTyOf` ty2

subTyCtorOf :: CtorName -> CtorName -> Bool
subTyCtorOf ctor1 ctor2 = ctor1 == ctor2 || ctor2 == "Any"

subLtOf :: Lt -> Lt -> Bool
subLtOf = curry \case
  (LtVar name1, LtVar name2) -> name1 == name2
  (LtVar name, LtMin lts) -> name `Set.member` lts
  (LtMin lts1, LtMin lts2) -> lts1 `Set.isSubsetOf` lts2
  (lt1, lt2) -> lt1 == LtFree || lt2 == LtLocal || lt1 == LtMin Set.empty -- todo

paramsToTyCtxEntry :: Bool -> Param -> TyCtxEntry
paramsToTyCtxEntry contextual MkParam { name, ty }
  | contextual = TyCtxCap MkTyCtxCap { name, monoTy = ty }
  | otherwise = TyCtxVar MkTyCtxVar { name, tySchema = emptyTySchema ty }

ensureMonoTy :: (HasCallStack, MonadError String m) => TySchema -> m MonoTy
ensureMonoTy = \case
  MkTySchema { ltParams = [], tyParams = [], ty } -> pure ty
  schema -> throwError $ "Expected mono type, got " <> show schema <> " at\n" <> prettyCallStack callStack

freeVars :: HasCallStack => Expr -> Set VarName
freeVars = \case
  Const _ -> Set.empty
  Plus lhs rhs -> freeVars lhs <> freeVars rhs
  Var name -> Set.singleton name
  TLam MkTLam { body } -> freeVars body
  TApp MkTApp { lhs } -> freeVars lhs
  CapCtor MkCapCtor { } -> error "Unused"
  Lam MkLam { ctxParams, params, body } ->
    freeVars body \\ toSetOf (folded % #name) (ctxParams <> params)
  App MkApp { callee, ctxArgs, args } ->
    freeVars callee <> foldMap freeVars ctxArgs <> foldMap freeVars args
  Match MkMatch { scrutinee, branches } ->
    freeVars scrutinee <> flip foldMap branches \MkBranch{ varPatterns, body } ->
      freeVars body \\ Set.fromList varPatterns
  Handle MkHandle { capName, handler, body } ->
    capName `Set.delete` freeVars body <>
    flip foldMap handler \MkHandlerEntry { paramNames, body } ->
      freeVars body \\ Set.fromList ("resume" : paramNames)
  other -> error $ "unsupported " <> show other

data PositionSign = PositivePos | NegativePos deriving Eq

changeSign :: PositionSign -> PositionSign
changeSign = \case PositivePos -> NegativePos; NegativePos -> PositivePos

freeLtVarsOn
  :: (HasCallStack, ?tyCtx :: TyCtx)
  => MonoTy -> PositionSign -> Set LtName
freeLtVarsOn ty expectedSign = ty
  & emptyTySchema
  & (`lifetimesOn` expectedSign)
  & foldMap extractVars
  where
    extractVars = \case
      LtVar name -> Set.singleton name
      LtMin names -> names
      _ -> Set.empty

lifetimesOn
  :: (HasCallStack, ?tyCtx :: TyCtx)
  => TySchema -> PositionSign -> Set Lt
lifetimesOn MkTySchema{ ltParams, tyParams, ty } expectedSign =
  let ?tyCtx = filterVars (each % #name `toSetOf` tyParams) ?tyCtx in
  let allLts = execWriter $ ty `goOn` PositivePos in
  let ltSet = Set.fromList ltParams in
  flip foldMap allLts \case
    LtVar name | name `Set.member` ltSet -> Set.empty
    LtMin names -> Set.singleton $ LtMin (names \\ ltSet) `lub` LtFree -- to normalize
    lt -> Set.singleton lt
  where
    goOn :: MonadWriter (Set Lt) m => MonoTy -> PositionSign -> m ()
    goOn ty currSign = case ty of
      TyVar name ->
        unless (tyParams & each % #name `elemOf` name) $
          runExceptT (?tyCtx `lookupBound` name) >>= \case
            Left _ -> pure () -- ignore bounds of unknown type parameters
            Right bound -> bound `goOn` currSign
      TyCtor MkTyCtor { lt, args } -> do
        when (currSign == expectedSign) $
          tell $ Set.singleton lt
        -- Type parameters are invariant, include them in both ways.
        forM_ args (`goOn` currSign)
        forM_ args (`goOn` changeSign currSign)
      TyFun MkTyFun { ctx, lt, args, res } -> do
        forM_ (ctx <> args) (`goOn` changeSign currSign)
        when (currSign == expectedSign) $
          tell $ Set.singleton lt
        res `goOn` currSign

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
