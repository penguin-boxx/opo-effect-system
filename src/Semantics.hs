module Semantics where

import Control.Monad.Cont
import Syntax

eval :: [Expr -> r] -> Expr -> Cont r Expr
eval kStack = \case
  Var name -> Var name
  Lam name body :@ arg -> (name =. arg) body
  Lam name body -> undefinend

-- Substitution.
(=.) :: VarName -> Expr -> Expr -> Expr
(=.) name arg body = undefined
