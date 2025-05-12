module Syntax where

import Common
import Types
import Text.PrettyPrint.GenericPretty
import Optics

data Expr
  = Const Int | Plus Expr Expr -- just to debug semantics
  | Var VarName
  | App Expr Expr
  | CtxApp Expr [Expr] -- CORE
  | Lam { ctxParams :: [VarName], param :: VarName, body :: Expr }
  | Ctor VarName
  | Match { scrutinee :: Expr, cases :: [Branch] }
  | LetIn { name :: VarName, expr :: Expr, body :: Expr }
  | Handle { name :: VarName, ty :: MonoTy, return :: Expr, ops :: [OpHandler], scope :: Expr }
  deriving stock Eq

data Branch = Branch { ctor :: VarName, patterns :: [VarName], body :: Expr }
  deriving stock Eq

data OpHandler = OpHandler
  { name :: OpName
  , body :: Expr
  }
  deriving stock Eq

data DataCtor = DataCtor { name :: VarName, params :: [TySchema] }

data Decl
  = DataDecl { tyName :: TyName, tyParams :: [TyName], dataCtors :: [DataCtor] }
  | EffDecl { effName :: TyName, tyParams :: [TyName], ops :: [TySchema] }
  | VarDecl { name :: VarName, ty :: TySchema, body :: Expr }

type Prog = [Decl]

deriving stock instance Generic Expr
instance Out Expr
instance Show Expr where
  show = pretty
deriving stock instance Generic Branch
instance Out Branch
instance Show Branch where
  show = pretty
deriving stock instance Generic OpHandler
instance Out OpHandler
instance Show OpHandler where
  show = pretty
deriving stock instance Generic DataCtor
instance Out DataCtor
instance Show DataCtor where
  show = pretty
deriving stock instance Generic Decl
instance Out Decl
instance Show Decl where
  show = pretty

makePrisms ''Expr
