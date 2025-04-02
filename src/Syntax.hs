module Syntax where

type VarName = String
type OpName = String

data Expr
  = Var VarName
  | Expr :@ Expr
  | Lam VarName Expr
  | Do OpName [Expr]
  | Handle Expr [Handler]

data Handler = Handler OpName Expr
