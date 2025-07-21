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
  let expr = TApp (Var "x") [LtLocal] [TyCtor "File" LtLocal []]
  let effCtx = []
  let tyCtx = [TyCtxVar "x" $ TySchema ["l"]
        [TyParam "a" $ TyCtor "Any" LtLocal []] $
        TyFun [] (LtVar "l") [TyVar "a"] (TyVar "a")]
  print $ runExcept $ inferExpr effCtx tyCtx expr
