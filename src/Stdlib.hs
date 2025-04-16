module Stdlib where

import Syntax
import Embedding

withStdLib :: Expr -> Expr
withStdLib =
  (#unit =. c 0) .
  (#nil =. Lam "s" $ Lam "z" $ v "z") .
  (#cons =. Lam "x" $ Lam "xs" $ Lam "s" $ Lam "z" $ v "s" :@ v "x" :@ (v "xs" :@ v "s" :@ v "z")) .
  ("++" =. Lam "xs" $ Lam "ys" $ Lam "s" $ Lam "z" $ v "xs" :@ v "s" :@ (v "ys" :@ v "s" :@ v "z")) .
  (#plus =. Lam "x" $ Lam "y" $ v "x" +. v "y") .
  (#sum =. Lam "xs" $ v "xs" :@ v "plus" :@ c 0) .
  (#inl =. Lam "x" $ Lam "f" $ Lam "g" $ v "f" :@ v "x") .
  (#inr =. Lam "y" $ Lam "f" $ Lam "g" $ v "g" :@ v "x") .
  (#case =. Lam "variant" $ Lam "f" $ Lam "g" $ v "variant" :@ v "f" :@ v "g") .
  (#true =. Lam "x" $ Lam "y" $ v "x") .
  (#false =. Lam "x" $ Lam "y" $ v "y") .
  (#if =. Lam "x" $ v "x") .
  (#fix =. Lam #f $ Lam #x (#f :@ (#x :@ #x)) :@ Lam #x (#f :@ (#x :@ #x)))

withState :: OpName -> Expr -> Expr -> Expr
withState name ini scope =
  withHandler
    ("x" --> Lam "s" $ v "x")
    [ ("get(" <> name <> ")", "_", "k") --> Lam "s" $ v "k" :@ v "s" :@ v "s"
    , ("put(" <> name <> ")", "s'", "k") --> Lam "s" $ v "k" :@ v "s" :@ v "s'"
    ]
    scope
  :@ ini
