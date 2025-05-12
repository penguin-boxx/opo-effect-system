{-# OPTIONS_GHC "-Wno-orphans" #-}

module Embedding where

import Common
import Syntax
import Data.Proxy
import Data.Set qualified as Set
import Types
import Optics
import GHC.OverloadedLabels
import GHC.TypeLits

infixr 0 =.
-- infixr 0 -->
-- infixr 0 $$
-- infixl 6 +.
infixl 9 @

(=.) :: VarName -> Expr -> Expr -> Expr
(=.) = LetIn

($$) :: Expr -> Expr -> Expr
($$) stmt = "_" =. stmt

c :: Int -> Expr
c = Const

(+.) :: Expr -> Expr -> Expr
(+.) = Plus

(@) :: Expr -> Expr -> Expr
(@) = App

-- \forall
-- f :: [TyName] -> TySchema -> TySchema
-- f name = over #params (name :)

-- -- ctx :: [MonoTy] -> TySchema -> TySchema
-- -- ctx (Set.fromList -> ctx) = over #effs (ctx <>)

lam :: [VarName] -> Expr -> Expr
lam names body = foldr (Lam []) body names

clam :: [VarName] -> [VarName] -> Expr -> Expr
clam ctx names body = set (_Lam % _1) ctx $ lam names body

withHandler :: (VarName, Expr) -> [OpHandler] -> Expr -> Expr
withHandler (returnName, returnBody) ops scope =
  Handle{ return = Lam [] returnName returnBody, ops, scope }


class LongArrow a b c | c -> a b where
  (-->) :: a -> b -> c

instance LongArrow (OpName, VarName, VarName) Expr OpHandler where
  (name, paramName, kName) --> opBody =
    OpHandler { name, body = Lam [] paramName $ Lam [] kName opBody }

instance LongArrow EffTy EffTy EffTy where
  l --> r = EffTy { effs = [], from = l, to = r }

instance LongArrow EffTy EffTy TySchema where
  l --> r = tySchemaFromEff $ EffTy { effs = [], from = l, to = r }


instance KnownSymbol name => IsLabel name String where
  fromLabel = symbolVal $ Proxy @name

instance KnownSymbol name => IsLabel name Expr where
  fromLabel = Var $ symbolVal $ Proxy @name

instance KnownSymbol name => IsLabel name MonoTy where
  fromLabel = mkVar $ symbolVal $ Proxy @name

instance KnownSymbol name => IsLabel name TySchema where
  fromLabel = tySchemaFromMono $ fromLabel @name
