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

data PureHandler = PureHandler
  { pureName :: VarName
  , pureBody :: Expr
  }

data OpHandler = OpHandler
  { opName :: OpName
  , paramName :: VarName
  , kName :: VarName
  , opBody :: Expr
  }

infixr 0 =.
infixr 0 -->
infixr 0 $$
infixl 6 +.
infixl 9 :@

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

class LongArrow a b c where
  (-->) :: a -> b -> c

instance LongArrow (OpName, VarName, VarName) Expr OpHandler where
  (opName, paramName, kName) --> opBody = OpHandler{..}

instance LongArrow VarName Expr (VarName, Expr) where
  (-->) = (,)

withHandler :: (VarName, Expr) -> [OpHandler] -> Expr -> Expr
withHandler (pureName, pureBody) hOps hScope = Handle{ hPure = PureHandler{..}, .. }

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
