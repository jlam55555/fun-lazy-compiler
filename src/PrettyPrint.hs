module PrettyPrint where

import           CorePrelude
import           Language
import           Utils

-- Iseq has two advantages over strings:
-- - Linear time append characteristic (basically defer all concatenation
--   operations until printing)
-- - Has operations for controlling newlines and indentation
data Iseq = INil
  | IStr String
  | IAppend Iseq Iseq

-- Empty iseq
iNil :: Iseq
iNil = INil

-- Turn a string into an iseq
iStr :: String -> Iseq
iStr s = IStr s

-- Append two iseqs
iAppend :: Iseq -> Iseq -> Iseq
iAppend INil seq2 = seq2
iAppend seq1 INil = seq1
iAppend seq1 seq2 = IAppend seq1 seq2

-- New line with indentation
iNewline :: Iseq
iNewline = iStr "\n"

-- Indent an iseq
-- (Currently ignoring indentation)
iIndent :: Iseq -> Iseq
iIndent seq = seq

-- Turn an iseq into a string
iDisplay :: Iseq -> String
iDisplay seq = flatten [seq]

-- Helper for iDisplay: concatenate in roughly linear time
-- (Reduces right-associatively.)
flatten :: [Iseq] -> String
flatten []                         = ""
flatten (INil              : seqs) = flatten seqs
flatten (IStr s            : seqs) = s ++ flatten seqs
flatten (IAppend seq1 seq2 : seqs) = flatten (seq1 : seq2 : seqs)

-- Alternative implementation of flatten using foldr
-- and an explicit string accumulator
-- flatten :: [Iseq] -> String
-- flatten = flatten' ""
--  where
--   flatten' acc = foldr
--     (\seq acc ->
--       (case seq of
--         INil              -> acc
--         IStr s            -> s ++ acc
--         IAppend seq1 seq2 -> flatten' acc [seq1, seq2]
--       )
--     )
--     acc

-- Concatenate a list of iseqs
iConcat :: [Iseq] -> Iseq
iConcat iseqList = foldl iAppend iNil iseqList

-- Interleave an iseq between every element in a
-- list of iseqs
iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave delim [] = iNil
iInterleave delim (seqListHd : seqListTl) =
  foldl (\acc term -> iConcat [acc, delim, term]) seqListHd seqListTl

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
           | otherwise      = iConcat [IStr "(", pprExpr e, IStr ")"]
