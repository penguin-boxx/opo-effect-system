module Main where

import Syntax
import Embedding
import Semantics
import Stdlib
import Core (infer)
import Control.Monad.Except

main :: IO ()
main = do
  print $ runExcept $ infer $
    (#f .: f #a (#a --> #a)) $
    #f -- :@ Plus (Const 42) (Const 1)
