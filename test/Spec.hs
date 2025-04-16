import Test.HUnit

import Syntax
import Embedding
import Stdlib
import Semantics

main :: IO ()
main = runTestTTAndExit $ TestList
  [ TestCase $ assertEqual "substitution" (Number 6) $ eval $
      (#x =. c 1 +. c 2) $
      (#y =. #x +. c 3)
      #y
  , TestCase $ assertEqual "lexical binding" (Number 1) $ eval $
      Lam #x (Lam #y #x) :@ c 1 :@ c 2
  , TestCase $ assertEqual "abortive effect" (Number 42) $ eval $
      withHandler
        (#x --> #x)
        [(#throw, #p, #_) --> #p] $
      (#x =. c 1) $
      (#y =. c 41) $
      Do #throw (#x +. #y)
  , TestCase $ assertEqual "many perform" (Number 20) $ eval $
      withHandler
        (#x --> #x)
        [(#ask, #_, #k) --> #k :@ c 10] $
      Do #ask (c 0) +. Do #ask (c 1)
  , TestCase $ assertEqual "many effects in stack" (Number 33) $ eval $
      withHandler
        (#x --> #x)
        [(#ask1, #_, #k) --> #k :@ c 3] $
      withHandler
        (#x --> #x)
        [(#ask2, #_, #k) --> #k :@ c 30] $
      Do #ask1 (c 0) +. Do #ask2 (c 1)
  , TestCase $ assertEqual "handler performs" (Number 13) $ eval $
      withHandler
        (#x --> #x)
        [(#ask1, #_, #k) --> #k :@ c 3] $
      withHandler
        (#x --> #x)
        [(#ask2, #_, #k) --> #k :@ (Do #ask1 (c 1) +. c 10)] $
      Do #ask2 (c 0)
  , TestCase $ assertEqual "pure works" (Number 21) $ eval $
      withHandler
        (#x --> #x +. c 1)
        [(#ask, #_, #k) --> #k :@ c 10] $
      Do #ask (c 0) +. Do #ask (c 1)
  , TestCase $ assertEqual "pure with many handlers" (Number 1111) $ eval $
      withHandler
        (#x --> #x +. c 1)
        [(#ask1, #_, #k) --> #k :@ c 10] $
      withHandler
        (#x --> #x +. c 1000)
        [(#ask2, #_, #k) --> #k :@ c 100] $
      Do #ask1 (c 0) +. Do #ask2 (c 1)
  , TestCase $ assertEqual "get + put" (Number 42) $ eval $
      withState #x (c 10) $
      (#tmp =. get #x) $
      put #x (#tmp +. c 32) $$
      get #x
  , TestCase $ assertEqual "get(x) + get(y)" (Number 42) $ eval $
      withState #x (c 10) $
      withState #y (c 32) $
      get #x +. get #y
  , TestCase $ assertEqual "many get and put" (Number 32) $ eval $
      withState #x (c 10) $
      withState #y (c 11) $
      (#z =. get #x +. get #y) $
      put #x #z $$
      get #x +. get #y
  , TestCase $ assertEqual "nondet" (Number 44) $ eval $
      withStdLib $
      #sum :@
        withHandler
          (#x --> #cons :@ #x :@ v #nil)
          [(#choice, #_, #k) --> v "++" :@ (#k :@ c 1) :@ (#k :@ c 10)]
        (Do #choice (c 0) +. Do #choice (c 0))
  , TestCase $ assertEqual "pair" (Number 11) $ eval $
      (#tmp =. Pair (c 1) (c 10)) $
      Fst #tmp +. Snd #tmp
  ]
