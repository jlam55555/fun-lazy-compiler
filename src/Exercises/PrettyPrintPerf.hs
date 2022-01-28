module Exercises.PrettyPrintPerf
  ( exercise1_1
  , exercise1_4
  ) where

import           Control.Exception
import           Iseq
import           Language
import qualified PrettyPrint
import           Utils

-- Demonstrate the performance issue with ++ and left-associativity.
-- Page 22 in the tutorial, exercise 1.1.

-- (Naive) pretty print expression
pprExpr :: CoreExpr -> String
pprExpr (ENum n   ) = show n
pprExpr (EVar v   ) = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2
pprExpr _           = throw NotImplementedError

-- (Naive) pretty print atomic (place parens around expression
-- unless it is already an atom)
pprAExpr :: CoreExpr -> String
pprAExpr e | isAtomicExpr e = pprExpr e
           | otherwise      = "(" ++ pprExpr e ++ ")"

-- Returns e1 e2 e2 e2 e2 ... = (... (((e1 e2) e2) e2) ...)
-- with n function applications.
mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s) where e2s = e2 : e2s

-- Roughly quadratic in number of steps wrt n.
-- (Starts slowing down around n=1000 to 10000.)
exercise1_1 :: Int -> Int
exercise1_1 n = length $ pprExpr $ mkMultiAp n (EVar "f") (EVar "x")

-- We can get linear performance if we used a Stringbuilder-like
-- accumulator and built the result string RTL.

-- Exercises 1.2-1.3: see PrettyPrint module for implementations
-- of iConcat, iInterleave, pprExpr, pprAExpr, pprScDefn, pprProgram

-- Runs in linear number of steps wrt n.
exercise1_4 :: Int -> Int
exercise1_4 n =
  length $ iDisplay $ PrettyPrint.pprExpr $ mkMultiAp n (EVar "f") (EVar "x")
