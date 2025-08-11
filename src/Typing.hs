module Typing where

import Common
import Types
import TypingCtx
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
  TApp MkTApp { lhs = Var varName, ltArgs, tyArgs } -> do
    MkTySchema { ltParams, tyParams, ty } <- ?tyCtx `lookup` varName
    ltSubst <- mkSubst ltParams ltArgs
    tySubst <- mkSubst (toListOf (each % #name) tyParams) tyArgs
    let bounds = ltSubst @ toListOf (each % #bound) tyParams
    checkBounds tyArgs bounds
    pure $ emptyTySchema $ ltSubst @ (tySubst @ ty)
  Lam MkLam { ctxParams, params, body } -> do
    let ?tyCtx =
          map (paramsToTyCtxEntry True) ctxParams ++
          map (paramsToTyCtxEntry False) params ++
          ?tyCtx
    res <- inferExpr body >>= ensureMonoTy
    let boundVars = foldMapOf (folded % #name) Set.singleton (ctxParams <> params)
    let freeVarNames = freeVars body \\ boundVars
    freeVarsSchemas <- mapM (?tyCtx `lookup`) (Set.toList freeVarNames)
    freeLts <- mconcat <$> mapM (`lifetimesOn` PositivePos) freeVarsSchemas
    let capturingLt = LtIntersect $ Set.toList freeLts
    resLts <- emptyTySchema res `lifetimesOn` PositivePos
    when (LtLocal `Set.member` resLts) $
      throwError "Tracked value escapes via return value"
    pure $ emptyTySchema $ TyFun MkTyFun
      { ctx = toListOf (each % #ty) ctxParams, lt = capturingLt
      , args = toListOf (each % #ty) params, res }
  TLam MkTLam { ltParams, tyParams, body } -> do
    let ?tyCtx = fmap TyCtxTy tyParams ++ ?tyCtx
    ty <- inferExpr body >>= ensureMonoTy
    pure MkTySchema { ltParams, tyParams, ty }
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
        throwError $ "Type mismatch: " <> show actual <> " is not a subtype of " <> show expected
    pure $ emptyTySchema res
  TApp MkTApp { lhs, ltArgs, tyArgs } -> do
    MkTySchema { ltParams, tyParams, ty } <- inferExpr lhs
    unless (length tyParams == length tyArgs) $
      throwError "Type arguments number mismatch"
    ltSubst <- mkSubst ltParams ltArgs
    tyParamNames <- forM (zip tyParams tyArgs) \(MkTyParam { name, bound }, arg) -> do
      unless (arg `subTyOf` ltSubst @ bound) $
        throwError $ "Type argument " <> show arg <> " is not a subtype of bound " <> show bound
      pure name
    tySubst <- mkSubst tyParamNames tyArgs
    pure $ emptyTySchema $ ltSubst @ (tySubst @ ty)
  Match MkMatch { scrutinee, branches } -> do
    MkTyCtor { name = tyCtor, lt, args = tyArgs } <-
      inferExpr scrutinee >>= ensureMonoTy >>= \case
        TyCtor ctor -> pure ctor -- todo check optics for this
        other -> throwError $ "Expected type ctor, got " <> show other
    let ctorCandidates = ?tyCtx `lookup` tyCtor
    resTys <- forM branches \MkBranch{ ctorName, varPatterns, body } -> do
      MkTyCtxCtor { tyParams, params } <-
        case List.find @[] (\MkTyCtxCtor { name } -> name == ctorName) ctorCandidates of
          Nothing -> throwError $ "Ctor " <> ctorName <> " do not have expected type"
          Just ctor -> pure ctor
      unless (length varPatterns == length params) $
        throwError "Number of patterns mismatch"
      tySubst <- mkSubst (toListOf (each % #name) tyParams) tyArgs
      params' <- forM (fmap (tySubst @) params) \param -> do
        posLts <- Set.toList <$> param `freeLtVarsOn` PositivePos
        negLts <- Set.toList <$> param `freeLtVarsOn` NegativePos
        posSubst <- mkSubst posLts (replicate (length posLts) lt)
        negSubst <- mkSubst negLts (replicate (length negLts) LtFree)
        pure $ posSubst @ (negSubst @ param) -- todo check order
      let mkCtxVar name param = TyCtxVar MkTyCtxVar { name, tySchema = emptyTySchema param }
      let ?tyCtx = zipWith mkCtxVar varPatterns params' ++ ?tyCtx
      inferExpr body
    unless (and $ zipWith (==) resTys (drop 1 resTys)) $
      throwError $ "Branch result types shouls be equal, but they are\n" <> unlines (map show resTys) -- todo LUB
    pure $ head resTys
  Perform MkPerform { opName, cap, tyArgs = opTyArgs, args } -> do
    MkTyCtor { name = effName, args = tyArgs } <-
      inferExpr cap >>= ensureMonoTy >>= \case
        TyCtor ctor -> pure ctor
        other -> throwError $ "Expected type constructor, got " <> show other
    argTys <- mapM (ensureMonoTy <=< inferExpr) args
    MkEffCtxEntry { tyParams, ops } <- ?effCtx `lookup` effName
    MkOpSig { tyParams = opTyParams, params, res } <- case ops !? opName of
      Nothing -> throwError $ "Effect " <> effName <> " do not include operation " <> opName
      Just op -> pure op
    -- todo check order
    -- todo join two substitutions
    subst <- mkSubst (tyParams ++ opTyParams) (tyArgs ++ opTyArgs)
    unless (length params == length argTys) $
      throwError "Operation arguments number mismatch"
    forM_ (zip params argTys) \(param, arg) ->
      unless (arg `subTyOf` subst @ param) $
        throwError "Operation argument type mismatch"
    pure $ emptyTySchema $ subst @ res
  Handle MkHandle { capName, effTy, handler, body } -> do
    unless (#lt % only LtLocal `has` effTy) $
      throwError "Capabilities can only have local lifetime"
    let capCtx = TyCtxCap MkTyCtxCap { name = capName, monoTy = TyCtor effTy }
    resTy <- let ?tyCtx = capCtx : ?tyCtx in inferExpr body >>= ensureMonoTy
    resLts <- emptyTySchema resTy `lifetimesOn` PositivePos
    resFv <- (<>) <$> resTy `freeLtVarsOn` PositivePos <*> resTy `freeLtVarsOn` NegativePos
    when (LtLocal `Set.member` resLts) $
      throwError "Tracked value is escaping out of the handler body"
    let MkTyCtor { name = effName, args = effTyArgs } = effTy
    MkEffCtxEntry { tyParams = effTyParams, ops } <- ?effCtx `lookup` effName
    subst <- mkSubst effTyParams effTyArgs
    unless (length handler == Map.size ops) $
      throwError "Wrong number of implemented operations"
    forM_ handler \MkHandlerEntry { opName, paramNames, body } -> do
      -- todo make substitution carefully
      MkOpSig { tyParams = opTyParams, params = fmap (subst @) -> params, res = (subst @) -> opResTy } <-
        case ops !? opName of
          Nothing -> throwError $ "Operation " <> opName <> " is not specified for effect " <> effName
          Just sig -> pure sig
      unless (Set.fromList opTyParams `Set.disjoint` resFv) $
        throwError "Operation generics should not leak" -- todo
      unless (length paramNames == length params) $
        throwError "Operation parameter number mismatch"
      let opParamCtx = TyCtxVar <$> zipWith MkTyCtxVar paramNames (emptyTySchema <$> params)
      let resumeCtx = TyCtxVar MkTyCtxVar
            { name = "resume", tySchema = emptyTySchema $ TyFun MkTyFun
                { ctx = [], lt = LtFree, args = [opResTy], res = resTy }
            }
      opRetTy <- let ?tyCtx = opParamCtx ++ resumeCtx : ?tyCtx in
        inferExpr (subst @ body) >>= ensureMonoTy
      unless (opRetTy == resTy) $
        throwError $ "Operation " <> opName <> " return type " <> show opRetTy <> " mismatch"
    pure $ emptyTySchema resTy
  unsupported -> error $ "Unsupported construct: " <> show unsupported

-- todo generalize and use everywhere
checkBounds :: (MonadError String m, ?tyCtx :: TyCtx) => [MonoTy] -> [MonoTy] -> m ()
checkBounds args bounds = do
  unless (length args == length bounds) $
    throwError "Arguments and parameters number mismatch"
  forM_ (zip args bounds) \(arg, bound) ->
    unless (arg `subTyOf` bound) $
      throwError $ "Type argument " <> show arg <> " do not satisfy bound " <> show bound

infix 2 `subTyOf`
subTyOf :: (?tyCtx :: TyCtx) => MonoTy -> MonoTy -> Bool
subTyOf = curry \case
  (TyVar name1, TyVar name2) -> name1 == name2;
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

subTyCtorOf :: CtorName -> CtorName -> Bool
subTyCtorOf ctor1 ctor2 = ctor1 == ctor2 || ctor2 == "Any"

subLtOf :: Lt -> Lt -> Bool
subLtOf = curry \case
  (LtVar name1, LtVar name2) -> name1 == name2
  (LtIntersect lts, lt) -> all (`subLtOf` lt) lts
  (lt, LtIntersect lts) -> any (lt `subLtOf`) lts
  (lt1, lt2) -> lt1 == LtFree || lt2 == LtLocal

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
  Set.fromList <$> mapM (tyCtx `lookup`) vars

data PositionSign = PositivePos | NegativePos deriving Eq

changeSign :: PositionSign -> PositionSign
changeSign = \case PositivePos -> NegativePos; NegativePos -> PositivePos

freeLtVarsOn
  :: (HasCallStack, MonadError String m, ?tyCtx :: TyCtx)
  => MonoTy -> PositionSign -> m (Set LtName)
freeLtVarsOn ty expectedSign = ty
  & emptyTySchema
  & (`lifetimesOn` expectedSign)
  & fmap (foldMap extractVars)
  where
    extractVars = toSetOf (subTrees % folded % _LtVar)

lifetimesOn
  :: (HasCallStack, MonadError String m, ?tyCtx :: TyCtx)
  => TySchema -> PositionSign -> m (Set Lt)
lifetimesOn MkTySchema{ ltParams, tyParams, ty } expectedSign = do
  allLts <- execWriterT $ ty `goOn` PositivePos
  pure $ foldr (Set.delete . LtVar) allLts ltParams
  where
    goOn
      :: (MonadWriter (Set Lt) m, MonadError String m)
      => MonoTy -> PositionSign -> m ()
    goOn ty currSign = case ty of
      TyVar name ->
        unless (tyParams & each % #name `elemOf` name) do
          bound <- ?tyCtx `lookupBound` name
          bound `goOn` currSign
      TyCtor MkTyCtor { lt, args } -> do
        -- Type parameters are invariant, include them in both ways.
        forM_ args (`goOn` currSign)
        forM_ args (`goOn` changeSign currSign)
        when (currSign == expectedSign) $
          tell $ Set.singleton lt
      TyFun MkTyFun { ctx, lt, args, res } -> do
        forM_ (ctx <> args) (`goOn` changeSign currSign)
        when (currSign == expectedSign) $
          tell $ Set.singleton lt
        res `goOn` currSign
