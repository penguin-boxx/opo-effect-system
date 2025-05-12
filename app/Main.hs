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
  let res = runExcept $ infer $ Plus (Const 1) (Const 2)
        -- (#g .: ctx [(#op, #int --> #int)] (#int --> #int) --> #int) $
        -- (#f .: f #a $ ctx [(#op, #int --> #int)] $ #a --> #a) $
        -- #g @ (#f @ Plus (Const 42) (Const 1))
  case res of
    Left error -> putStrLn error
    Right result -> print result
