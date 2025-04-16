module Syntax where

import Text.PrettyPrint.GenericPretty

type VarName = String
type OpName = String

data Expr
  = Const Int
  | Plus Expr Expr
  | Var VarName
  | Expr :@ Expr
  | Lam VarName Expr
  | Pair Expr Expr
  | Fst Expr
  | Snd Expr
  | Do OpName Expr
  | Handle
    { hPure :: PureHandler
    , hOps :: [OpHandler]
    , hScope :: Expr
    }
  deriving Eq

infixl 9 :@

data PureHandler = PureHandler
  { pureName :: VarName
  , pureBody :: Expr
  }
  deriving Eq

data OpHandler = OpHandler
  { opName :: OpName
  , paramName :: VarName
  , kName :: VarName
  , opBody :: Expr
  }
  deriving Eq

deriving stock instance Generic Expr
instance Out Expr
instance Show Expr where
  show = pretty
deriving stock instance Generic PureHandler
instance Out PureHandler
instance Show PureHandler where
  show = pretty
deriving stock instance Generic OpHandler
instance Out OpHandler
instance Show OpHandler where
  show = pretty
