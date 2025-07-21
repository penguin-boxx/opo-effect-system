module Syntax where

import Common
import Types
import Text.PrettyPrint.GenericPretty
import Optics

-- Delimited continuation marker.
type Marker = Int

data HandlerEntry = HandlerEntry
  { opName :: OpName
  , body :: Expr
  }

type Handler = [HandlerEntry]

data Branch = Branch { ctorName :: CtorName, varPatterns :: [VarName], body :: Expr }

data Param = Param { name :: VarName, ty :: MonoTy }

data Expr
  = Const Int | Plus Expr Expr -- just to debug semantics
  | Var VarName
  | TLam { ltParams :: [LtName], tyParams :: [TyParam], body :: Expr }
  | TApp { lhs :: Expr, ltArgs :: [Lt], tyArgs :: [MonoTy] }
  | Ctor { name :: CtorName, ltArgs :: [Lt], tyArgs :: [MonoTy], args :: [Expr] }
  | CapCtor
    { name :: CtorName
    , tyArgs :: [MonoTy]
    , marker :: Marker
    , handler :: Handler
    }
  | Lam { ctxParams :: [Param], params :: [Param], body :: Expr }
  | App { callee :: Expr, ctxArgs :: [Expr], args :: [Expr] }
  | Match { scrutinee :: Expr, branches :: [Branch] }
  | Perform { opName :: OpName, cap :: Expr, tyArgs :: [MonoTy], args :: [Expr] }
  | Handle { capName :: VarName, effTy :: MonoTy, handler :: Handler, body :: Expr }
  | Handler { marker :: Marker, body :: Expr }

data DataCtor = DataCtor { name :: CtorName, params :: [MonoTy] }

data Decl
  = DataDecl
    { tyName :: TyName
    , ltParams :: [LtName]
    , tyParams :: [TyName]
    , dataCtors :: [DataCtor]
    }
  | EffDecl { effName :: TyName, tyParams :: [TyName], ops :: [TySchema] }
  | VarDecl { name :: VarName, ty :: TySchema, body :: Expr }

type Prog = [Decl]


deriving stock instance Generic HandlerEntry
instance Out HandlerEntry
instance Show HandlerEntry where
  show = pretty
deriving stock instance Generic Branch
instance Out Branch
instance Show Branch where
  show = pretty
deriving stock instance Generic Param
instance Out Param
instance Show Param where
  show = pretty
deriving stock instance Generic Expr
instance Out Expr
instance Show Expr where
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
