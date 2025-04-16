{-# OPTIONS_GHC "-Wno-orphans" #-}

module Embedding where

import Syntax
import Data.Proxy
import GHC.OverloadedLabels
import GHC.TypeLits

infixr 0 =.
infixr 0 -->
infixr 0 $$
infixl 6 +.

(=.) :: VarName -> Expr -> Expr -> Expr
(name =. expr) body = Lam name body :@ expr

($$) :: Expr -> Expr -> Expr
($$) stmt = "_" =. stmt

c :: Int -> Expr
c = Const

(+.) :: Expr -> Expr -> Expr
(+.) = Plus

v :: VarName -> Expr
v = Var

thunk :: Expr -> Expr
thunk = Lam "_"

force :: Expr -> Expr
force = (:@ c 0)

class LongArrow a b c | c -> a b where
  (-->) :: a -> b -> c

instance LongArrow (OpName, VarName, VarName) Expr OpHandler where
  (opName, paramName, kName) --> opBody = OpHandler{..}

instance LongArrow VarName Expr (VarName, Expr) where
  (-->) = (,)

withHandler :: (VarName, Expr) -> [OpHandler] -> Expr -> Expr
withHandler (pureName, pureBody) hOps hScope = Handle{ hPure = PureHandler{..}, .. }

instance KnownSymbol name => IsLabel name String where
  fromLabel = symbolVal $ Proxy @name

instance KnownSymbol name => IsLabel name Expr where
  fromLabel = v $ symbolVal $ Proxy @name
