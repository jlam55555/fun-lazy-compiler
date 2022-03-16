module Evaluators.TemplateInstantiation.EvaluatorTests
  ( tests
  ) where

import           Test.HUnit

import           Evaluators.TemplateInstantiation

progSKK3, progLet, progLetSc, progLetRec, progInd :: String
progNeg1, progNeg2, progNeg3, progArith1, progArith2 :: String
progChurch, progCons, progInfinite1, progInfinite2, progInfinite3 :: String
progAnd1, progAnd2, progAnd3, progAnd4 :: String
progOr1, progOr2, progOr3, progOr4 :: String
progXor1, progXor2, progXor3, progXor4 :: String
progFac, progFib, progPair, progList :: String

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

-- Conditionals; checking truth tables
progAnd1 = "main = and False False"
progAnd2 = "main = and False True"
progAnd3 = "main = and True False"
progAnd4 = "main = and True True"

progOr1 = "main = or False False"
progOr2 = "main = or False True"
progOr3 = "main = or True False"
progOr4 = "main = or True True"

progXor1 = "main = xor False False"
progXor2 = "main = xor False True"
progXor3 = "main = xor True False"
progXor4 = "main = xor True True"

-- Factorial
progFac = "fac n = if (n == 0) 1 (n * fac (n - 1)) ;\
  \main = fac 3"

-- Naive fibonacci
progFib = "fib n = if (n < 2) n (fib (n - 1) + fib (n - 2)) ;\
  \main = fib 6"

-- Exercise 2.22: Pair
progPair = "main = fst (snd (fst (Pair (Pair 1 (Pair 2 3)) 4)))"

-- Lists
progList = "main = length (Cons (Cons 2 3) (Cons 2 (Cons 3 Nil)))"

tests :: Test
tests = test
  [ "exercise 2.4" ~: runGetNumOutput progSKK3 ~=? 3
  , "let example 1" ~: runGetNumOutput progLet ~=? 4
  , "let example 2" ~: runGetNumOutput progLetSc ~=? 5
  , "letrec" ~: runGetNumOutput progLetRec ~=? 6
  , "indirections" ~: runGetNumOutput progInd ~=? 7
  , "unary primitive (`negate`) w/o dump" ~: runGetNumOutput progNeg1 ~=? (-3)
  , "unary primitive (`negate`) w/ dump" ~: runGetNumOutput progNeg2 ~=? 3
  , "unary primitive (`negate`) w/ indirection"
  ~:  runGetNumOutput progNeg3
  ~=? (-3)
  , "binary primitive" ~: runGetNumOutput progArith1 ~=? 101
  , "binary primitive 2" ~: runGetNumOutput progArith2 ~=? (-3)
  , "church" ~: runGetNumOutput progChurch ~=? 4
  , "cons" ~: runGetNumOutput progCons ~=? 2
  , "infinite streams (ones)" ~: runGetNumOutput progInfinite1 ~=? 1
  , "infinite streams (nats)" ~: runGetNumOutput progInfinite2 ~=? 9
  , "infinite streams (fibs)" ~: runGetNumOutput progInfinite3 ~=? 34
  , "and F F" ~: not (runGetBoolOutput progAnd1) ~? ""
  , "and F T" ~: not (runGetBoolOutput progAnd2) ~? ""
  , "and T F" ~: not (runGetBoolOutput progAnd3) ~? ""
  , "and T T" ~: runGetBoolOutput progAnd4 ~? ""
  , "or F F" ~: not (runGetBoolOutput progOr1) ~? ""
  , "or F T" ~: runGetBoolOutput progOr2 ~? ""
  , "or T F" ~: runGetBoolOutput progOr3 ~? ""
  , "or T T" ~: runGetBoolOutput progOr4 ~? ""
  , "xor F F" ~: not (runGetBoolOutput progXor1) ~? ""
  , "xor F T" ~: runGetBoolOutput progXor2 ~? ""
  , "xor T F" ~: runGetBoolOutput progXor3 ~? ""
  , "xor T T" ~: not (runGetBoolOutput progXor4) ~? ""
  , "factorial" ~: runGetNumOutput progFac ~=? 6
  , "naive fib" ~: runGetNumOutput progFib ~=? 8
  , "pairs" ~: runGetNumOutput progPair ~=? 2
  , "list" ~: runGetNumOutput progList ~=? 3
  ]
