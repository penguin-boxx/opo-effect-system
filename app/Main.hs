module Main where

import Syntax
import Semantics.Simple

main :: IO ()
main = do
  putStrLn "Hello!"
  -- putStr "example1: "; print $ eval example1
  -- putStr "example2: "; print $ eval example2
  -- putStr "example3: "; print $ eval example3
  -- putStr "example4: "; print $ eval example4
  -- putStr "example5: "; print $ eval example5
  -- putStr "example6: "; print $ eval example6
  -- putStr "testPureWorks: "; print $ eval testPureWorks
  -- putStr "testPureWorksDepth: "; print $ eval testPureWorksDepth
  -- putStr "exampleGetPut: "; print $ eval exampleGetPut
  -- putStr "exampleGetXGetY: "; print $ eval exampleGetXGetY
  -- putStr "exampleGetYGetX: "; print $ eval exampleGetXGetY
  -- putStr "exampleXYGetPut: "; print $ eval exampleXYGetPut
  -- putStr "exampleNonDet: "; print $ eval exampleNonDet
  putStr "examplePair: "; print $ eval examplePair

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
exampleGetPut :: Expr
exampleGetPut =
  withState "x" (c 10) $
  ("tmp" =. Do "get(x)" (c 0)) $
  Do "put(x)" (v "tmp" +. c 32) $$
  Do "get(x)" (c 0)

exampleGetXGetY :: Expr
exampleGetXGetY =
  withState "x" (c 10) $
  withState "y" (c 32) $
  Do "get(x)" (c 0) +. Do "get(y)" (c 0)

exampleGetYGet :: Expr
exampleGetYGet =
  withState "x" (c 10) $
  withState "y" (c 32) $
  Do "get(x)" (c 0) +. Do "get(y)" (c 0)

-- expected 32
exampleXYGetPut :: Expr
exampleXYGetPut =
  withState "x" (c 10) $
  withState "y" (c 11) $
  ("z" =. Do "get(x)" (c 0) +. Do "get(y)" (c 0)) $
  Do "put(x)" (v "z") $$
  Do "get(x)" (c 0) +. Do "get(y)" (c 0)

exampleNonDet :: Expr
exampleNonDet =
  withStdLib $
  ("xs" =. v "cons" :@ c 1 :@ (v "cons" :@ c 2 :@ v "nil")) $
  v "sum" :@
    withHandler
      ("x" --> v "cons" :@ v "x" :@ v "nil")
      [("choice", "_", "k") --> v "++" :@ (v "k" :@ c 1) :@ (v "k" :@ c 10)]
    (Do "choice" (c 0) +. Do "choice" (c 0))

examplePair :: Expr
examplePair =
  ("tmp" =. Pair (c 1) (c 10)) $
  Fst (v "tmp") +. Snd (v "tmp")


-- exampleHigherOrder :: Expr
-- exampleHigherOrder =
--   withStdLib $
--   withState #x (c 0) $
--   (#withExn =. #fix :@ Lam #rec (Lam #body $ withHandler
--     (#x --> #x)
--     [ (#throw, #p, #_) --> #inl :@ #p
--     , (#catch, #p, #k) -->
--         (#try =. Fst #p) $
--         (#handler =. Snd #p) $
--         #case :@
--           :@ (Lam "e" $ )
--     , (#abort, #p, #_) --> #p
--     ]
--     (force #body))) $
--   #withExn :@ thunk (
--     c 0
--   )

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

thunk :: Expr -> Expr
thunk = Lam "_"

force :: Expr -> Expr
force = (:@ v "unit")
