module Core where

import Common
import Types
import Data.Bool
import Data.Functor
import Data.Functor.Identity
import Data.Foldable
import Data.Map (Map, (!), (!?))
import Data.Map qualified as Map
import Syntax
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Fix
import Control.Monad.Except
import Control.Monad.Writer
import Text.PrettyPrint.GenericPretty
import Data.Generics.Uniplate.Data qualified as Uniplate
import GHC.Stack
import Optics

data TyContextEntry = VarEntry VarName TySchema | CtxEntry VarName MonoTy
type TyContext = [TyContextEntry]

inferExpr
  :: (MonadError String m, MonadReader TyContext m, MonadWriter TySubst m)
  => Expr -> m (Expr, EffTy)
inferExpr source = case source of
  Const _ -> pure (source, int)
  Plus l r -> do
    (l', lTy) <- inferExpr l
    (r', rTy) <- inferExpr r
    unify lTy int >>= tell
    unify rTy int >>= tell
    pure (Plus l' r', int)
  App f arg -> do
    (f', fTy) <- inferExpr f
    (effs, expectedTy, resTy) <- case fTy of
      NoEff monoTy -> throwError $ "Expected function, got " <> show monoTy
      EffTy { effs, from, to } -> (effs, from, to)
    (arg', argTy) <- inferExpr arg
    unify expectedTy argTy >>= tell -- TODO subeffecting
    implicits <- extractCtx effs -- TODO в контексте могут быть унификационные переменные?
    pure (CtxApp implicits f' arg', resTy)
  other -> error $ "Unsupported " <> show other
  where
    int = effTyFromName "Int"

infer :: MonadError String m => Expr -> m (Expr, EffTy)
infer expr = do
  (res, _) <-
    runWriterT $
    flip runReaderT [] $
    inferExpr expr
  pure res

extractCtxFirst :: MonadReader TyContext m => VarName -> m EffTy
extractCtxFirst = undefined

extractCtx :: MonadReader TyContext m => Effs -> [VarName]
extractCtx = undefined

-- data Constraint
--   = CtxConstraint { witness :: VarName, opTy :: MonoTy }
--   | MonoTy := MonoTy

-- infix 6 :=

-- type Constraints = [Constraint]



-- infer :: (MonadError [String] m) => Syntax.Expr -> m (Core, Ty)
-- infer expr = do
--   ((core, resTy), constraints) <-
--     runFreshT @MonoTy $ runFreshT @VarName $
--     flip runReaderT Map.empty $ runWriterT $ modifyError (:[]) $
--     elaborate expr
--   ((subst, residuals), errors) <- runWriterT $ solve constraints
--   reportErrors residuals errors
--   let monoTy = subst `appTySubst` resTy
--   zonked <- zonk core subst
--   generalize zonked monoTy

-- elaborate
--   :: ( HasCallStack, MonadFresh MonoTy m, MonadFresh VarName m
--      , MonadWriter Constraints m, MonadReader TyContext m, MonadError String m
--      )
--   => Syntax.Expr -> m (Core, MonoTy)
-- elaborate = \case
--   Syntax.Const n -> pure (Const n, int)
--   Syntax.Plus l r -> do
--     (l', lTy) <- elaborate l
--     (r', rTy) <- elaborate r
--     tell [lTy := int, rTy := int]
--     pure (Plus l' r', int)
--   Syntax.Var name -> asks (!? name) >>= \case
--     Nothing -> throwError $ "Unknown variable " <> name
--     Just Ty{ tyParams, effs, monoTy } -> do
--       tySubst <- forM tyParams \param ->
--         (param,) <$> fresh @MonoTy
--       let withTApp = Var name `TApp` map snd tySubst
--       let instantiatedTy = mkTySubst tySubst `appTySubst` monoTy
--       let withWeakCtxApp = WeakCtxApp withTApp (Map.elems effs)
--       pure (withWeakCtxApp, instantiatedTy)
--   f Syntax.@ arg -> do
--     (f', fTy) <- elaborate f
--     (arg', argTy) <- case fTy of
--       MonoTy{ tyArgs = Ty{ effs } : _ } -> do
--         result <- local (<> fmap tyFromMono effs) $ elaborate arg
--         pure $ over _1 (CtxLam (Map.keys effs)) result
--       _ -> elaborate arg
--     f' <- case f' of
--       WeakCtxApp f' tys -> do
--         ctxVars <- forM tys \ty -> do
--           ctxName <- fresh @VarName
--           tell [CtxConstraint{ witness = ctxName, opTy = ty }]
--           pure $ Var ctxName
--         pure $ CtxApp f' ctxVars
--       f' -> pure f'
--     resTy <- fresh @MonoTy
--     tell [fTy := tyFromMono argTy --> tyFromMono resTy]
--     pure (App f' arg', resTy)
--   Syntax.Ascription name ty expr ->
--     local (Map.insert name ty) $ elaborate expr
--   other -> error $ "Unexpected expression: " <> show other
--   where
--     int = monoFromName "Int"
--     l --> r = MonoTy { tyCtor = "->", tyArgs = [l, r] }

-- whileM :: Monad m => m Bool -> m a -> m ()
-- whileM cond body = cond >>= bool (pure ()) (void body)

-- solve :: MonadWriter [String] m => Constraints -> m (TySubst, Constraints)
-- solve intialConstraints =
--   (\(_, subst, residual) -> (subst, residual)) <$>
--   flip execStateT (True, mempty @TySubst, intialConstraints) do
--     whileM (zoom _1 $ get @Bool) do
--       zoom _1 $ put False
--       constraints <- zoom _3 $ get <* put []
--       for_ constraints \case
--         t1 := t2 -> runExceptT (unify t1 t2) >>= \case
--           Left error -> tell [error]
--           Right subst -> do
--             zoom _1 $ put True
--             zoom _2 $ modify (subst <>)
--         CtxConstraint{} -> zoom _1 $ put True -- TODO
--         -- other ->
--         --   zoom _3 $ modify (other :)

unify :: MonadError String m => EffTy -> EffTy -> m TySubst
unify = curry \case
  (NoEff ty1, NoEff ty2) -> unifyMono ty1 ty2
  (EffTy{ from = from1, to = to1 }, EffTy{ from = from2, to = to2 }) ->
    (<>) <$> unify from1 from2 <*> unify to1 to2 -- TODO unify effects
  (t1, t2) -> throwError $ "Cannot unify " <> show t1 <> " and " <> show t2

unifyMono :: MonadError String m => MonoTy -> MonoTy -> m TySubst
unifyMono = curry \case
  (MonoTy{ ctor, args = [] }, ty) |
    isVarCtor ctor, noneOf (universe % each % #ctor) (== ctor) ty ->
    pure $ singletonTySubst ctor ty
  (ty, var@MonoTy{ ctor, args = [] }) | isVarCtor ctor ->
    unifyMono var ty
  (MonoTy{ ctor = ctor1, args = args1 }, MonoTy{ ctor = ctor2, args = args2 }) |
    not (isVarCtor ctor1), not (isVarCtor ctor2),
    ctor1 == ctor2, length args1 == length args2 ->
    mconcat <$> zipWithM unifyMono args1 args2
  (t1, t2) -> throwError $ "Cannot unify " <> show t1 <> " and " <> show t2

-- reportErrors :: MonadError [String] m => Constraints -> [String] -> m ()
-- reportErrors residual errors
--   | null residual && null errors = pure ()
--   | otherwise = throwError $ errors <> map (("Cannot solve: " <>) . show) residual

-- zonk :: (HasCallStack, Monad m) => Core -> TySubst -> m Core
-- zonk core subst = case core of
--   c@(Const _) -> pure c
--   Plus l r -> Plus <$> zonk l subst <*> zonk r subst
--   v@(Var _) -> pure v
--   App f arg -> do
--     f' <- zonk f subst
--     arg' <- zonk arg subst
--     pure $ App f' arg'
--   TApp f monoTy -> do
--     f' <- zonk f subst
--     pure $ TApp f' (fmap (subst `appTySubst`) monoTy)
--   WeakCtxApp f tys -> do
--     f' <- zonk f subst
--     pure $ WeakCtxApp f' (map (subst `appTySubst`) tys)
--   CtxApp f vars -> CtxApp <$> zonk f subst <*> mapM (`zonk` subst) vars -- TODO
--   CtxLam names body -> CtxLam names <$> zonk body subst
--   -- other -> error $ "Unsupported node: " <> show other

-- generalize :: Monad m => Core -> MonoTy -> m (Core, Ty)
-- generalize core monoTy = pure (core, tyFromMono monoTy)


newtype TySubst = TySubst (Map TyName MonoTy)
  deriving stock (Eq, Show)

instance Semigroup TySubst where
  second@(TySubst secondMap) <> first@(TySubst firstMap) = TySubst $
    let keys = Map.keys firstMap <> Map.keys secondMap in
    let substituted = (second `appTySubst`) . (first `appTySubst`) . mkVar <$> keys in
    Map.fromList $ zip keys substituted

instance Monoid TySubst where
  mempty = TySubst Map.empty

singletonTySubst :: TyName -> MonoTy -> TySubst
singletonTySubst = curry $ mkTySubst . (:[])

mkTySubst :: [(TyName, MonoTy)] -> TySubst
mkTySubst = TySubst . Map.fromList

appTySubst :: HasCallStack => TySubst -> MonoTy -> MonoTy
appTySubst (TySubst subst) = Uniplate.transform \ty ->
  case subst !? view #ctor ty of
    Nothing -> ty
    Just ty' -> over #args (++ view #args ty) ty'


newtype FreshT (label :: k) m a = FreshT (StateT Int m a)
  deriving newtype (Functor, Applicative, Monad, MonadError e, MonadTrans)

runFreshT :: forall l m a . Monad m => FreshT l m a -> m a
runFreshT (FreshT comp) = evalStateT comp 0

runFresh :: forall l a . FreshT l Identity a -> a
runFresh = runIdentity . runFreshT

class Monad m => MonadFresh l m where
  fresh :: m l

instance {-# OVERLAPPING #-} Monad m => MonadFresh Int (FreshT l m) where
  fresh = FreshT $ get <* modify' (+ 1)

instance {-# OVERLAPPING #-} Monad m => MonadFresh MonoTy (FreshT MonoTy m) where
  fresh = mkVar . ("t" <>) . show <$> fresh @Int

instance {-# OVERLAPPING #-} Monad m => MonadFresh VarName (FreshT VarName m) where
  fresh = ("$" <>) . show <$> fresh @Int

instance {-# OVERLAPPING #-} (MonadTrans t, MonadFresh l m) => MonadFresh l (t m) where
  fresh = lift $ fresh @l


-- deriving stock instance Generic Core
-- instance Out Core
-- instance Show Core where
--   show = pretty
-- deriving stock instance Generic Constraint
-- instance Out Constraint
-- instance Show Constraint where
--   show = pretty
