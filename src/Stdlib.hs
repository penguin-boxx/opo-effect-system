module Stdlib where

import Common
import Syntax
import Embedding

withStdLib :: Expr -> Expr
withStdLib =
  (#nil =. Lam #s $ Lam #z #z) .
  (#cons =. Lam #x $ Lam #xs $ Lam #s $ Lam #z $ #s :@ #x :@ (#xs :@ #s :@ #z)) .
  (#concat =. Lam #xs $ Lam #ys $ Lam #s $ Lam #z $ #xs :@ #s :@ (#ys :@ #s :@ #z)) .
  (#plus =. Lam #x $ Lam #y $ #x +. #y) .
  (#sum =. Lam #xs $ #xs :@ #plus :@ c 0) .
  (#inl =. Lam #x $ Lam #f $ Lam #g $ #f :@ #x) .
  (#inr =. Lam #y $ Lam #f $ Lam #g $ #g :@ #y) .
  (#case =. Lam #variant $ Lam #f $ Lam #g $ #variant :@ #f :@ #g) .
  (#true =. Lam #x $ Lam #y #x) .
  (#false =. Lam #x $ Lam #y #y) .
  (#if =. Lam #x $ #x) .
  (#zero =. Lam #s $ Lam #z #z) .
  (#suc =. Lam #n $ Lam #s $ Lam #z $ #s :@ (#n :@ #s :@ #z)) .
  (#iszero =. Lam #n $ #n :@ Lam #_ #false :@ #true) .
  (#fix =. Lam #f $ Lam #x (#f :@ Lam #z (#x :@ #x :@ #z)) :@ Lam #x (#f :@ Lam #z (#x :@ #x :@ #z)))

withState :: OpName -> Expr -> Expr -> Expr
withState name ini scope =
  withHandler
    (#x --> Lam #s $ #x)
    [ ("get(" <> name <> ")", #_, #k) --> Lam #s $ #k :@ #s :@ #s
    , ("put(" <> name <> ")", #s', #k) --> Lam #s $ #k :@ #s :@ #s'
    ]
    scope
  :@ ini

get :: String -> Expr
get name = Do ("get(" <> name <> ")") (c 0)

put :: String -> Expr -> Expr
put name = Do ("put(" <> name <> ")")

thunk :: Expr -> Expr
thunk = Lam "_"

force :: Expr -> Expr
force = (:@ unit)

do' :: OpName -> Expr
do' name = Do name unit

unit :: Expr
unit = c 0
