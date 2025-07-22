module Main where

import Syntax
import Semantics
import Types
import Typing
import Stdlib
import Control.Monad
import Control.Monad.Except

main :: IO ()
main = do
  tapp
  lam
  app

tapp :: IO ()
tapp = do
  let effCtx = []
  let tyCtx = [TyCtxVar "id" $ TySchema ["l"]
        [TyParam "a" $ TyCtor "Any" LtLocal []] $
        TyFun [] (LtVar "l") [TyVar "a"] (TyVar "a")]
  let expr = TApp (Var "id") [LtLocal] [TyCtor "File" LtLocal []]
  print $ runExcept $ inferExpr effCtx tyCtx expr

lam :: IO ()
lam = do
  let effCtx = []
  let tyCtx = [TyCtxVar "id" $ TySchema ["l"]
        [TyParam "a" $ TyCtor "Any" LtLocal []] $
        TyFun [] (LtVar "l") [TyVar "a"] (TyVar "a")]
  let expr = Lam
        [Param "io" $ TyCtor "IO" LtLocal []]
        [Param "x" $ TyCtor "Int" LtFree []] $
        TApp (Var "x") [] []
  print $ runExcept $ inferExpr effCtx tyCtx expr

app :: IO ()
app = do
  let effCtx = []
  let tyCtx = [TyCtxVar "id" $ TySchema ["l"]
        [TyParam "a" $ TyCtor "Any" LtLocal []] $
        TyFun [] (LtVar "l") [TyVar "a"] (TyVar "a")]
  let expr = Lam
        [Param "io" $ TyCtor "IO" LtLocal []]
        [Param "x" $ TyCtor "Int" LtFree []] $
        App (TApp (Var "id") [LtFree] [TyCtor "Int" LtFree []]) [] [TApp (Var "x") [] []]
  print $ runExcept $ inferExpr effCtx tyCtx expr
