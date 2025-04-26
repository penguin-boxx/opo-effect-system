module Core where

import Common
import Types
import Data.Bool
import Data.Functor
import Data.Functor.Identity
import Data.Foldable
import Data.Map (Map, (!), (!?))
import Data.Map qualified as Map
import Data.Generics.Uniplate.Data qualified as Uniplate
import Syntax qualified
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Fix
import Control.Monad.Except
import Control.Monad.Writer
import Text.PrettyPrint.GenericPretty
import GHC.Stack
import Optics

data Core
  = Const Int
  | Plus Core Core
  | Var VarName
  | App Core Core
  | TApp Core [MonoTy]
  -- ^ Apply to all arguments at once to avoid capturing.
  | WeakCtxApp Core [Ty]
  -- ^ To replace with CtxApp or to generalize.
  | CtxApp Core [Core] -- todo make list?
  -- | Lam VarName Core
  -- | TLam [TyName] Core
  -- | LetIn VarName Core Core

data Constraint
  = CtxConstraint { witness :: VarName, opTy :: Ty }
  | MonoTy := MonoTy
  | Constraints :=> Constraints

infix 6 :=

type Constraints = [Constraint]

type TyContext = Map VarName Ty

infer :: (MonadError [String] m) => Syntax.Expr -> m (Core, Ty)
infer expr = do
  ((core, resTy), constraints) <-
    runFreshT @MonoTy $ runFreshT @VarName $
    flip runReaderT Map.empty $ runWriterT $
    elaborate expr
  ((subst, residuals), errors) <- runWriterT $ solve constraints
  reportErrors residuals errors
  let monoTy = subst `appTySubst` resTy
  zonked <- zonk core subst
  generalize zonked monoTy

elaborate
  :: ( HasCallStack, MonadFresh MonoTy m, MonadFresh VarName m
     , MonadWriter Constraints m, MonadReader TyContext m
     )
  => Syntax.Expr -> m (Core, MonoTy)
elaborate = \case
  Syntax.Const n -> pure (Const n, int)
  Syntax.Plus l r -> do
    (l', lTy) <- elaborate l
    (r', rTy) <- elaborate r
    tell [lTy := int, rTy := int]
    pure (Plus l' r', int)
  Syntax.Var name -> asks (!? name) >>= \case
    Nothing -> (Var name,) <$> fresh @MonoTy
    Just Ty{ tyParams, effs, monoTy } -> do
      tySubst <- forM tyParams \param ->
        (param,) <$> fresh @MonoTy
      let withTApp = Var name `TApp` map snd tySubst
      let instantiatedTy = mkTySubst tySubst `appTySubst` monoTy
      let withWeakCtxApp = WeakCtxApp withTApp (Map.elems effs)
      pure (withWeakCtxApp, instantiatedTy)
  f Syntax.:@ arg -> do
    (f', fTy) <- elaborate f
    f' <- case f' of
      WeakCtxApp f' tys -> do
        ctxVars <- forM tys \ty -> do
          ctxName <- fresh @VarName
          tell [CtxConstraint{ witness = ctxName, opTy = ty }]
          pure $ Var ctxName
        pure $ CtxApp f' ctxVars
      f' -> pure f'
    (arg', argTy) <- elaborate arg
    resTy <- fresh @MonoTy
    tell [fTy := argTy --> resTy]
    pure (App f' arg', resTy)
  Syntax.Ascription name ty expr ->
    local (Map.insert name ty) $ elaborate expr
  other -> error $ "Unexpected expression: " <> show other
  where
    int = monoFromName "Int"
    l --> r = MonoTy { tyCtor = "->", tyArgs = [l, r] }

whileM :: Monad m => m Bool -> m a -> m ()
whileM cond body = cond >>= bool (pure ()) (void body)

solve :: MonadWriter [String] m => Constraints -> m (TySubst, Constraints)
solve intialConstraints =
  (\(_, subst, residual) -> (subst, residual)) <$>
  flip execStateT (True, mempty @TySubst, intialConstraints) do
    whileM (zoom _1 $ get @Bool) do
      zoom _1 $ put False
      constraints <- zoom _3 $ get <* put []
      for_ constraints \case
        t1 := t2 -> runExceptT (unify t1 t2) >>= \case
          Left error -> tell [error]
          Right subst -> do
            zoom _1 $ put True
            zoom _2 $ modify (subst <>)
        CtxConstraint{} -> zoom _1 $ put True -- TODO
        other ->
          zoom _3 $ modify (other :)

unify :: MonadError String m => MonoTy -> MonoTy -> m TySubst
unify = curry \case
  (MonoTy{ tyCtor, tyArgs = [] }, ty) |
    isVarCtor tyCtor, tyCtor `notElem` map (view #tyCtor) (Uniplate.universe ty) -> -- TODO uniplate
    pure $ singletonTySubst tyCtor ty
  (ty, var@MonoTy{ tyCtor, tyArgs = [] }) | isVarCtor tyCtor ->
    unify var ty
  (MonoTy{ tyCtor = ctor1, tyArgs = args1 }, MonoTy{ tyCtor = ctor2, tyArgs = args2 }) |
    not (isVarCtor ctor1), not (isVarCtor ctor2),
    ctor1 == ctor2, length args1 == length args2 ->
    mconcat <$> zipWithM unify args1 args2
  (t1, t2) -> throwError $ "Cannot unify " <> show t1 <> " and " <> show t2

reportErrors :: MonadError [String] m => Constraints -> [String] -> m ()
reportErrors residual errors
  | null residual && null errors = pure ()
  | otherwise = throwError $ errors <> map (("Cannot solve: " <>) . show) residual

zonk :: (HasCallStack, Monad m) => Core -> TySubst -> m Core
zonk core subst = case core of
  c@(Const _) -> pure c
  Plus l r -> Plus <$> zonk l subst <*> zonk r subst
  v@(Var _) -> pure v
  App f arg -> do
    f' <- zonk f subst
    arg' <- zonk arg subst
    pure $ App f' arg'
  TApp f monoTy -> do
    f' <- zonk f subst
    pure $ TApp f' (fmap (subst `appTySubst`) monoTy)
  WeakCtxApp f tys -> do
    f' <- zonk f subst
    pure $ WeakCtxApp f' (over (each % #monoTy) (subst `appTySubst`) tys) -- TODO no poly supported
  CtxApp f vars -> CtxApp <$> zonk f subst <*> mapM (`zonk` subst) vars -- TODO
  -- other -> error $ "Unsupported node: " <> show other

generalize :: Monad m => Core -> MonoTy -> m (Core, Ty)
generalize core monoTy = pure (core, tyFromMono monoTy)


newtype TySubst = TySubst (Map TyName MonoTy)
  deriving (Eq, Show)

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
  case subst !? view #tyCtor ty of
    Nothing -> ty
    Just ty' -> over #tyArgs (++ view #tyArgs ty) ty'


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


deriving stock instance Generic Core
instance Out Core
instance Show Core where
  show = pretty
deriving stock instance Generic Constraint
instance Out Constraint
instance Show Constraint where
  show = pretty
