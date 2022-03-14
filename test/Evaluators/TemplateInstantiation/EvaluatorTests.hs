module Evaluators.TemplateInstantiation.EvaluatorTests
  ( tests
  ) where

import           Test.HUnit

import           Evaluators.TemplateInstantiation.Evaluator

progSKK3, progLet, progLetSc, progLetRec, progInd :: String
progNeg1, progNeg2, progNeg3, progArith1, progArith2 :: String
progChurch, progCons, progInfinite1, progInfinite2, progInfinite3 :: String

-- Exercise 2.4
progSKK3 = "main = S K K 3"

progLet = "main = let x = 4 in x"

progLetSc = "main = let s = K in s 5 6"

progLetRec =
  "pair x y f = f x y ;\
  \fst p = p K ;\
  \snd p = p K1 ;\
  \f x y = letrec\
  \          a = pair x b ;\
  \          b = pair y a\
  \        in fst (snd (snd (snd a))) ;\
  \main = f 5 6"

progInd = "main = twice twice I 7"

progNeg1 = "main = negate 3"
progNeg2 = "main = twice negate 3"
progNeg3 = "main = negate (I 3)"

progArith1 = "main = 2 + (7 / 3) * 52 - 5"
progArith2 = "main = 2 + 7 / 3 * 52 - 5" -- messing with order of operations

progChurch =
  "Z f x = x ;\
  \S n f x = f (n f x) ;\
  \inc x = x + 1 ;\
  \main = let one = S Z in\
  \       let three = S (S one) in\
  \         compose (three inc) (one inc) 0"

-- `cons` library (a Church encoding)
consDef :: String
consDef =
  "cons x y z = z x y ;\
  \car x = x K ;\
  \cdr x = x K1 ;\
  \caar = compose car car ;\
  \cadr = compose car cdr ;\
  \cdar = compose cdr car ;\
  \cddr = compose cdr cdr ;\
  \caadr = compose car cadr ;\
  \caaar = compose car caar ;\
  \cdadr = compose cdr cadr ;\
  \cdaar = compose cdr caar ;\
  \caddr = compose car cddr ;\
  \cadar = compose car cdar ;\
  \cdddr = compose cdr cddr ;\
  \cddar = compose cdr cdar ;\
  \getTenth = compose car (compose cdddr (compose cdddr cdddr)) ;"

-- Also a Church encoding
progCons = consDef ++ "main = let lst = cons 1 (cons 2 3) in cadr lst"

-- Infinite list/stream!
progInfinite1 = consDef ++ "main = letrec ones = cons 1 ones in getTenth ones"
progInfinite2 =
  consDef
    ++ "numsFrom x = cons x (numsFrom (x + 1)) ;\
       \nats = numsFrom 0 ;\
       \main = getTenth nats"
progInfinite3 =
  consDef
    ++ "fibsRec a b = cons a (fibsRec b (a + b)) ;\
       \fibs = fibsRec 0 1 ;\
       \main = getTenth fibs"

tests :: Test
tests = test
  [ "exercise 2.4" ~: runGetResult progSKK3 ~=? 3
  , "let example 1" ~: runGetResult progLet ~=? 4
  , "let example 2" ~: runGetResult progLetSc ~=? 5
  , "letrec" ~: runGetResult progLetRec ~=? 6
  , "indirections" ~: runGetResult progInd ~=? 7
  , "unary primitive (`negate`) w/o dump" ~: runGetResult progNeg1 ~=? (-3)
  , "unary primitive (`negate`) w/ dump" ~: runGetResult progNeg2 ~=? 3
  , "unary primitive (`negate`) w/ indirection"
  ~:  runGetResult progNeg3
  ~=? (-3)
  , "binary primitive" ~: runGetResult progArith1 ~=? 101
  , "binary primitive 2" ~: runGetResult progArith2 ~=? (-3)
  , "church" ~: runGetResult progChurch ~=? 4
  , "cons" ~: runGetResult progCons ~=? 2
  , "infinite streams (ones)" ~: runGetResult progInfinite1 ~=? 1
  , "infinite streams (nats)" ~: runGetResult progInfinite2 ~=? 9
  , "infinite streams (fibs)" ~: runGetResult progInfinite3 ~=? 34
  ]
