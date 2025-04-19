module Core where

import Common
import Types
import Data.Map (Map)
import Data.Map qualified as Map
import Syntax qualified
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Fix
import Text.PrettyPrint.GenericPretty

data Core
  = Const Int
  | Plus Core Core
  | Var VarName
  | App Core Core
  | TApp Core MonoTy
  | CApp Core Core
  | Lam VarName Core
  | TLam TyName TyIdx Core
  | LetIn VarName Core Core

type TyContext = Map VarName Ty

newtype TySubst = TySubst { getTySubst :: Map TyName MonoTy }
  deriving (Eq, Show)

data FlatConstraint
  = ContextConstraint VarName Eff
  | MonoTy := MonoTy

data Constraints
  = Flats [FlatConstraint]
  | Implication [TyIdx] FlatConstraint Constraints

-- tySubstOf :: [(Name, Type)] -> TySubst
-- tySubstOf = TySubst . Map.fromList

-- infer :: MonadReader TyContext m => Core -> m (Core, Ty)
-- infer expr = do
--   (core, constraints) <- elaborate expr
--   subst <- solve constraints
--   let core' = core `appSubst` subst
--   let monoTy = core `appSubst` subst


-- elaborate :: Syntax.Expr -> (Core, Constraint)
-- elaborate =
--   flip runReader Map.empty .
--   flip execState Map.empty .
--   mfix \rec -> \case
--   Syntax.LetIn name expr body ->
--     let (coreExpr, constraints) = rec expr in
--     let subst = undefined in
--     local (Map.insert name )

-- solve :: TyContext -> Constraint -> TySubst
-- solve = undefined

-- newtype SubstTy = SubstTy { getSubstTy :: Map Name Type }
--   deriving (Eq, Show)

-- substTyOf :: [(Name, Type)] -> SubstTy
-- substTyOf = SubstTy . Map.fromList


-- type FreshSeed = Integer

-- -- Реальная монада, предоставляющая свежие переменные.
-- newtype FreshT m a = FreshT (StateT FreshSeed m a)
--   deriving newtype (Functor, Applicative, Monad, MonadError e)

-- runFreshT :: Monad m => FreshT m a -> m a
-- runFreshT (FreshT comp) = evalStateT comp 0

-- -- Интерфейс свежести.
-- class Monad m => MonadFresh m where
--   fresh :: m Type

-- instance Monad m => MonadFresh (FreshT m) where
--   fresh = FreshT $ gets (TVar . ('t' :) . show) <* modify' (+ 1)

deriving stock instance Generic Core
instance Out Core
instance Show Core where
  show = pretty
