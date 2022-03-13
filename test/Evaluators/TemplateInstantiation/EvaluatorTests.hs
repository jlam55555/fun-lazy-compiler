module Evaluators.TemplateInstantiation.EvaluatorTests
  ( tests
  ) where

import           Test.HUnit

import           Evaluators.TemplateInstantiation.Evaluator
import           Evaluators.TemplateInstantiation.Node

progSKK3, progLet, progLetSc, progLetRec :: String

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

tests :: Test
tests = test
  [ "exercise 2.4" ~: getResult progSKK3 ~=? NNum 3
  , "let example 1" ~: getResult progLet ~=? NNum 4
  , "let example 2" ~: getResult progLetSc ~=? NNum 5
  , "letrec" ~: getResult progLetRec ~=? NNum 6
  ]
