module Main where

import Syntax
import Semantics.Defun

main :: IO ()
main = do
  putStrLn "Hello!"
  putStr "example1: "; print $ eval' example1
  putStr "example2: "; print $ eval' example2
  putStr "example3: "; print $ eval' example3
  putStr "example4: "; print $ eval' example4
  putStr "example5: "; print $ eval' example5
  putStr "example6: "; print $ eval' example6
  putStr "testPureWorks: "; print $ eval' testPureWorks
  putStr "testPureWorksDepth: "; print $ eval' testPureWorksDepth
  putStr "exampleGet: "; print $ eval' exampleGet

-- expected 6
example1 :: Expr
example1 =
  ("x" =. c 1 +. c 2) $
  ("y" =. v "x" +. c 3) $
  v "y"

-- expected 1
example2 :: Expr
example2 = Lam "x" (Lam "y" $ v "x") :@ c 1 :@ c 2

-- expected 42
example3 :: Expr
example3 =
  withHandler
    ("x" --> v "x")
    [("throw", "p", "_") --> v "p"] $
  ("x" =. c 1) $
  ("y" =. c 41) $
  Do "throw" (v "x" +. v "y")

-- expected 20
example4 :: Expr
example4 =
  withHandler
    ("x" --> v "x")
    [("ask", "_", "k") --> v "k" :@ c 10] $
  Do "ask" (c 0) +. Do "ask" (c 1)

-- expected 33
example5 :: Expr
example5 =
  withHandler
    ("x" --> v "x")
    [("ask1", "_", "k") --> v "k" :@ c 3] $
  withHandler
    ("x" --> v "x")
    [("ask2", "_", "k") --> v "k" :@ c 30] $
  Do "ask1" (c 0) +. Do "ask2" (c 1)

-- expected 13
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

withState :: OpName -> Expr -> Expr -> Expr
withState name ini scope =
  withHandler
    ("x" --> Lam "s" $ v "x")
    [ ("get(" <> name <> ")", "_", "k") --> Lam "s" $ v "k" :@ v "s" :@ v "s"
    , ("put(" <> name <> ")", "s'", "k") --> Lam "s" $ v "k" :@ v "s" :@ v "s'"
    ]
    scope
  :@ ini

-- expected 42
exampleGet :: Expr
exampleGet =
  withState "y" (c 1000) $
  withState "x" (c 1) $
  Do "put(x)" (c 10) $$
  ("z" =. Do "get(x)" (c 0)) $
  Do "put(x)" (v "z" +. c 32) $$
  Do "put(y)" (c (-100)) $$
  Do "get(x)" (c 0) +. Do "get(y)" (c 0)
