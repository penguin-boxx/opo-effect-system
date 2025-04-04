module Main where

import Syntax
import Semantics.Defun

main :: IO ()
main = do
  putStrLn "Hello!"
  -- print $ eval' example1
  -- print $ eval' example2
  -- print $ eval' example3
  -- print $ eval' example4
  -- print $ eval' example5
  -- print $ eval' example6
  -- print $ eval' testPureWorks
  -- print $ eval' testPureWorksDepth
  print $ eval' exampleGet

example1 :: Expr
example1 =
  ("x" =. c 1 +. c 2) $
  ("y" =. v "x" +. c 3) $
  v "y"

example2 :: Expr
example2 = Lam "x" (Lam "y" $ v "x") :@ c 1 :@ c 2

example3 :: Expr
example3 =
  withHandler
    ("x" --> v "x")
    [("throw", "p", "_") --> v "p"] $
  ("x" =. c 1) $
  ("y" =. c 41) $
  Do "throw" (v "x" +. v "y")

example4 :: Expr
example4 =
  withHandler
    ("x" --> v "x")
    [("ask", "_", "k") --> v "k" :@ c 10] $
  Do "ask" (c 0) +. Do "ask" (c 1)

example5 :: Expr
example5 =
  withHandler
    ("x" --> v "x")
    [("ask1", "_", "k") --> v "k" :@ c 3] $
  withHandler
    ("x" --> v "x")
    [("ask2", "_", "k") --> v "k" :@ c 30] $
  Do "ask1" (c 0) +. Do "ask2" (c 1)

example6 :: Expr
example6 =
  withHandler
    ("x" --> v "x")
    [("ask1", "_", "k") --> v "k" :@ c 3] $
  withHandler
    ("x" --> v "x")
    [("ask2", "_", "k") --> v "k" :@ (Do "ask1" (c 1) +. c 10)] $
  Do "ask2" (c 0)

-- expect 21
testPureWorks :: Expr
testPureWorks =
  withHandler
    ("x" --> v "x" +. c 1)
    [("ask", "_", "k") --> v "k" :@ c 10] $
  Do "ask" (c 0) +. Do "ask" (c 1)

-- expect 1111
testPureWorksDepth :: Expr
testPureWorksDepth =
  withHandler
    ("x" --> v "x" +. c 1)
    [("ask1", "_", "k") --> v "k" :@ c 10] $
  withHandler
    ("x" --> v "x" +. c 1000)
    [("ask2", "_", "k") --> v "k" :@ c 100] $
  Do "ask1" (c 0) +. Do "ask2" (c 1)

withState :: Expr -> Expr -> Expr
withState ini scope = 
  withHandler
    ("x" --> Lam "s" $ v "x")
    [ ("get", "_", "k") --> Lam "s" $ v "k" :@ v "s" :@ v "s"
    , ("put", "s'", "k") --> Lam "s" $ v "k" :@ v "s" :@ v "s'"
    ] 
    scope 
  :@ ini

-- expected 1
exampleGet :: Expr
exampleGet =
  withState (c 1) $
  Do "put" (c 10) $$ 
  Do "get" (c 0)

-- example3 :: Expr
-- example3 =
--   withHandler "throw" (v "p") $


-- example4 :: Expr
-- example4 =
--   withHandler "ask'" (v "k" :@ c 41) $
--   withHandler "ask" (v "k" :@ c 1) $
--     Do "ask" (c 0) +. Do "ask'" (c 0)

-- example5 :: Expr
-- example5 =
--   ("f" =. Lam "_" (Do "ask" (c 0))) $
--   withHandler "ask'" (v "k" :@ c 41) $
--   withHandler "ask" (v "k" :@ Do "ask'" (c 0)) $
--     Do "ask" (c 0) +. Do "ask'" (c 0) +. v "f" :@ c 0
