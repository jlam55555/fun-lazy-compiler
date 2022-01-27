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
pprProgram prog = iInterleave sep (map pprScDefn prog)
  where sep = iConcat [iStr " ;", iNewline]

-- Supercombinator to iseq
-- Body is automatically put on a new line in case the args are long,
-- still looks okay even if there are no args
pprScDefn :: CoreScDefn -> Iseq
pprScDefn (name, args, body) = iConcat
  [lhs, iStr " = ", iNewline, iStr "  ", iIndent $ pprExpr body]
  where lhs = iInterleave (iStr " ") (map iStr (name : args))

-- Expression to iseq
-- Exercise 1.3: Write remaining rules.
pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n               ) = iStr (show n)
pprExpr (EVar v               ) = iStr v
pprExpr (EAp e1 e2            ) = iConcat [pprExpr e1, iStr " ", pprAExpr e2]
pprExpr (ELet isrec defns expr) = iConcat
  [ iStr keyword
  , iStr " "
  , iIndent (pprDefns defns)
  , iNewline
  , iStr "in "
  , pprExpr expr
  ]
 where
  keyword | not isrec = "let"
          | otherwise = "letrec"
pprExpr (ECase scrut alters) =
  iConcat [iStr "case ", pprExpr scrut, iStr " of ", pprAlters alters]
pprExpr (EConstr tag arity) = iConcat
  [iStr "Pack{ ", iStr (show tag), iStr ", ", iStr (show arity), iStr " }"]
pprExpr (ELam args body) = iConcat [iStr "\\ ", lhs, iStr " . ", pprExpr body]
  where lhs = iConcat (map iStr args)

-- Alter list to iseq
pprAlters :: [CoreAlter] -> Iseq
pprAlters alters = iInterleave sep (map pprAlter alters)
  where sep = iConcat [iStr " ;", iNewline]

-- Alter to iseq
pprAlter :: CoreAlter -> Iseq
pprAlter (tag, args, body) = iConcat [lhs, iStr " -> ", pprExpr body]
 where
  tagSeq = iConcat [iStr "<", iStr (show tag), iStr ">"]
  lhs    = iInterleave (iStr " ") (tagSeq : map iStr args)

-- Definition list to iseq
pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where sep = iConcat [iStr " ;", iNewline]

-- Definition to iseq
pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]

-- Atomic expression to iseq (place parens around it if
-- not already an atom)
pprAExpr :: CoreExpr -> Iseq
pprAExpr e | isAtomicExpr e = pprExpr e
           | otherwise      = iConcat [iStr "( ", pprExpr e, iStr " )"]
