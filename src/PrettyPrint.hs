module PrettyPrint
  ( pprint
  , pprProgram
  , pprScDefn
  , pprExpr
  , pprDefns
  , pprDefn
  ) where

import           Iseq
import           Language

-- Pretty print entire program
pprint :: CoreProgram -> String
pprint prog = iDisplay $ pprProgram prog

-- Program to iseq
pprProgram :: CoreProgram -> Iseq
pprProgram prog = iInterleave iNewline (map pprScDefn prog)

-- Supercombinator to iseq
pprScDefn :: CoreScDefn -> Iseq
pprScDefn (name, args, body) = iConcat
  [lhs, iStr "=", pprExpr body, iStr ";", iNewline]
  where lhs = iInterleave (iStr " ") (map iStr (name : args))

-- Expression to iseq
pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n               ) = iStr (show n)
pprExpr (EVar v               ) = iStr v
pprExpr (EAp e1 e2            ) = iConcat [pprExpr e1, iStr " ", pprAExpr e2]
pprExpr (ELet isrec defns expr) = iConcat
  [ iStr keyword
  , iNewline
  , iStr "  "
  , iIndent (pprDefns defns)
  , iNewline
  , iStr "in "
  , pprExpr expr
  ]
 where
  keyword | not isrec = "let"
          | isrec     = "letrec"

-- Definition list to iseq
pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where sep = iConcat [iStr ";", iNewline]

-- Definition to iseq
pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]

-- Atomic expression to iseq (place parens around it if
-- not already an atom)
pprAExpr :: CoreExpr -> Iseq
pprAExpr e | isAtomicExpr e = pprExpr e
           | otherwise      = iConcat [iStr "(", pprExpr e, iStr ")"]
