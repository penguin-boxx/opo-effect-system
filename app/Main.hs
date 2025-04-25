module Main where

import Syntax
import Embedding
import Semantics
import Stdlib
import Control.Monad
import Core (infer)
import Control.Monad.Except

main :: IO ()
main = do
  let res = runExcept $ infer $
        (#f .: f #a (#a --> #a)) $
        #f :@ Plus (Const 42) (Const 1)
  case res of
    Left errors ->
      forM_ errors putStrLn
    Right result ->
      print result
