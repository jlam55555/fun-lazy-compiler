module PrettyPrintInfix
  ( expr
  ) where

import           Iseq
import           Language
import           PrettyPrint

expr :: CoreExpr
expr =
  (EAp (EAp (EVar "<") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y")))
       (EAp (EAp (EVar "*") (EVar "p")) (EAp (EVar "length") (EVar "xs")))
  )

exercise1_8 :: IO ()
exercise1_8 = putStrLn $ iDisplay $ pprExpr expr
