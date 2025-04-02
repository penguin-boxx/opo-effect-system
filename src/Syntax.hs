module Syntax where

type VarName = String
type OpName = String

data Expr
  = Const Int
  | Plus Expr Expr
  | Var VarName
  | Expr :@ Expr
  | Lam VarName Expr
  | Do OpName Expr
  | Handle Expr OpName VarName VarName Expr
  deriving Show

infix 1 =.
infixl 6 +.
infixl 9 :@

(=.) :: VarName -> Expr -> Expr -> Expr
(name =. expr) body = Lam name body :@ expr

c :: Int -> Expr
c = Const

(+.) :: Expr -> Expr -> Expr
(+.) = Plus

v :: VarName -> Expr
v = Var

withHandler :: OpName -> Expr -> Expr -> Expr
withHandler opName body scope = Handle scope opName "" "" body
