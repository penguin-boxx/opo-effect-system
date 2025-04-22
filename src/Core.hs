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
  | CtxApp Core Core
  | Lam VarName Core
  | TLam [TyName] Core
  | LetIn VarName Core Core

data Constraint
  = CxtConstraint { witness :: VarName, opName :: OpName, opTy :: Ty }
  | MonoTy := MonoTy
  | Constraints :=> Constraints

type Constraints = [Constraint]

type TyContext = Map VarName Ty

infer :: (MonadError [String] m) => Syntax.Expr -> m (Core, Ty)
infer expr = do
  ((core, resTy), constraints) <-
    runFreshT @MonoTy $ flip runReaderT Map.empty $ runWriterT $
    elaborate expr
  (subst, residuals) <- solve constraints
  reportErrors residuals
  let monoTy = subst `appTySubst` resTy
  zonked <- zonk core subst
  generalize zonked monoTy

elaborate
  :: (HasCallStack, MonadFresh MonoTy m, MonadWriter Constraints m, MonadReader TyContext m)
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
    Just Ty{ tyParams, monoTy } -> do
      subst <- forM tyParams \param -> (param,) <$> fresh @MonoTy
      let core = Var name `TApp` map snd subst
      let ty = mkTySubst subst `appTySubst` monoTy
      pure (core, ty)
  Syntax.Ascription name ty expr ->
    local (Map.insert name ty) $ elaborate expr
  other -> error $ "Unexpected expression: " <> show other
  where
    int = monoFromName "Int"

whileM :: Monad m => m Bool -> m a -> m ()
whileM cond body = cond >>= bool (pure ()) (void body)

solve :: (HasCallStack, Monad m) => Constraints -> m (TySubst, Constraints)
solve intialConstraints =
  (\(_, subst, residual) -> (subst, residual)) <$>
  flip execStateT (True, mempty @TySubst, intialConstraints) do
    whileM (zoom _1 $ get @Bool) do
      zoom _1 $ put False
      constraints <- zoom _3 get
      zoom _3 $ put []
      for_ constraints \case
        MonoTy{ tyCtor = ctor1, tyArgs = [] } := MonoTy{ tyCtor = ctor2, tyArgs = [] } | ctor1 == ctor2 ->
          zoom _1 $ put True
        MonoTy{ tyCtor, tyArgs = [] } := ty |
          tyCtor `notElem` map (view #tyCtor) (Uniplate.universe ty) -> do
          zoom _1 $ put True
          zoom _2 $ modify (singletonTySubst tyCtor ty <>)
        ty := var@MonoTy{ tyArgs = [] } -> do
          zoom _1 $ put True
          zoom _3 $ modify (var := ty :)
        other ->
          zoom _3 $ modify (other :)

reportErrors :: MonadError [String] m => Constraints -> m ()
reportErrors residuals
  | null residuals = pure ()
  | otherwise = throwError $ map show residuals

zonk :: (HasCallStack, Monad m) => Core -> TySubst -> m Core
zonk core subst = case core of
  c@(Const _) -> pure c
  Plus l r -> Plus <$> zonk l subst <*> zonk r subst
  v@(Var _) -> pure v
  TApp f monoTy -> do
    f' <- zonk f subst
    pure $ TApp f' (fmap (subst `appTySubst`) monoTy)
  other -> error $ "Unsupported node: " <> show other

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
  deriving newtype (Functor, Applicative, Monad, MonadError e)

runFreshT :: forall l m a . Monad m => FreshT l m a -> m a
runFreshT (FreshT comp) = evalStateT comp 0

runFresh :: forall l a . FreshT l Identity a -> a
runFresh = runIdentity . runFreshT

class Monad m => MonadFresh l m where
  fresh :: forall l . m MonoTy

instance {-# OVERLAPPING #-} Monad m => MonadFresh l (FreshT l m) where
  fresh = FreshT $ gets (mkVar . ('t' :) . show) <* modify' (+ 1)

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
