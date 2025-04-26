{-# OPTIONS_GHC "-Wno-orphans" #-}

module Embedding where

import Common
import Syntax
import Data.Proxy
import Data.Map qualified as Map
import Types
import Optics
import GHC.OverloadedLabels
import GHC.TypeLits

infixr 0 =.
infixr 0 -->
infixr 0 $$
infixl 6 +.
infixr 0 .:

(=.) :: VarName -> Expr -> Expr -> Expr
(=.) = LetIn

($$) :: Expr -> Expr -> Expr
($$) stmt = "_" =. stmt

c :: Int -> Expr
c = Const

(+.) :: Expr -> Expr -> Expr
(+.) = Plus

v :: VarName -> Expr
v = Var

(.:) :: VarName -> Ty -> Expr -> Expr
(.:) = Ascription

f :: TyName -> Ty -> Ty
f name = over #tyParams (name :)

ctx :: [(OpName, MonoTy)] -> Ty -> Ty
ctx (Map.fromList -> ctx) = over #effs (ctx <>)

withHandler :: (VarName, Expr) -> [OpHandler] -> Expr -> Expr
withHandler (pureName, pureBody) hOps hScope =
  Handle{ hPure = PureHandler{ pureName, pureBody }, hOps, hScope }


class LongArrow a b c | c -> a b where
  (-->) :: a -> b -> c

instance LongArrow (OpName, VarName, VarName) Expr OpHandler where
  (opName, paramName, kName) --> opBody =
    OpHandler{ opName, paramName, kName, opBody }

instance LongArrow VarName Expr (VarName, Expr) where
  (-->) = (,)

instance LongArrow Ty Ty MonoTy where
  l --> r = MonoTy { tyCtor = "->", tyArgs = [l, r] }

instance LongArrow Ty Ty Ty where
  l --> r = tyFromMono $ MonoTy { tyCtor = "->", tyArgs = [l, r] }


instance KnownSymbol name => IsLabel name String where
  fromLabel = symbolVal $ Proxy @name

instance KnownSymbol name => IsLabel name Expr where
  fromLabel = v $ symbolVal $ Proxy @name

instance KnownSymbol name => IsLabel name MonoTy where
  fromLabel = mkVar $ symbolVal $ Proxy @name

instance KnownSymbol name => IsLabel name Ty where
  fromLabel = tyFromMono $ fromLabel @name
