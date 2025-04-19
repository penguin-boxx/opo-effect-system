{-# OPTIONS_GHC "-Wno-orphans" #-}

module Embedding where

import Common
import Syntax
import Data.Proxy
import GHC.OverloadedLabels
import GHC.TypeLits

infixr 0 =.
infixr 0 -->
infixr 0 $$
infixl 6 +.

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

class LongArrow a b c | c -> a b where
  (-->) :: a -> b -> c

instance LongArrow (OpName, VarName, VarName) Expr OpHandler where
  (opName, paramName, kName) --> opBody =
    OpHandler{ opName, paramName, kName, opBody }

instance LongArrow VarName Expr (VarName, Expr) where
  (-->) = (,)

withHandler :: (VarName, Expr) -> [OpHandler] -> Expr -> Expr
withHandler (pureName, pureBody) hOps hScope =
  Handle{ hPure = PureHandler{ pureName, pureBody }, hOps, hScope }

instance KnownSymbol name => IsLabel name String where
  fromLabel = symbolVal $ Proxy @name

instance KnownSymbol name => IsLabel name Expr where
  fromLabel = v $ symbolVal $ Proxy @name
