module PrettyPrint
  ( pprint
  , pprProgram
  , pprScDefn
  , pprExpr
  , pprAlter
  , pprAlters
  , pprDefns
  , pprDefn
  , ppp
  ) where

import           Iseq
import           Language

-- Pretty print entire program
pprint :: CoreProgram -> String
pprint prog = iDisplay $ pprProgram prog

-- Program to iseq
pprProgram :: CoreProgram -> Iseq
pprProgram prog = iInterleave sep $ pprScDefn <$> prog
  where sep = iConcat [iStr " ;", iNewline]

-- Supercombinator to iseq
-- Body is automatically put on a new line in case the args are long,
-- still looks okay even if there are no args
pprScDefn :: CoreScDefn -> Iseq
pprScDefn (name, args, body) = iConcat
  [lhs, iStr " =", iNewline, iStr "  ", iIndent $ pprExpr body]
  where lhs = iInterleave (iStr " ") $ iStr <$> (name : args)

data Associativity = AscLeft | AscRight | AscNone
  deriving Eq

-- Associativity of an infix operator
opAsc :: String -> Associativity
opAsc op | op `elem` ["*", "+", "&", "|"] = AscRight
         | op `elem` ["/", "-", "==", "~=", ">", ">=", "<", "<="] = AscNone
         | otherwise                      = AscLeft

-- Ordered enum describing precedence levels for infix operators
data PrecLevel = PrecAtom
  | PrecAp
  | PrecMulDiv
  | PrecAddSub
  | PrecRel
  | PrecConj
  | PrecDisj
  | PrecBase
  deriving (Eq, Show)

intOfPrecLevel :: PrecLevel -> Int
intOfPrecLevel PrecAtom   = 7
intOfPrecLevel PrecAp     = 6
intOfPrecLevel PrecMulDiv = 5
intOfPrecLevel PrecAddSub = 4
intOfPrecLevel PrecRel    = 3
intOfPrecLevel PrecConj   = 2
intOfPrecLevel PrecDisj   = 1
intOfPrecLevel PrecBase   = 0

instance Ord PrecLevel where
  (<=) a b = intOfPrecLevel a <= intOfPrecLevel b

opPrec :: String -> PrecLevel
opPrec "*"  = PrecMulDiv
opPrec "/"  = PrecMulDiv
opPrec "+"  = PrecAddSub
opPrec "-"  = PrecAddSub
opPrec "==" = PrecRel
opPrec "~=" = PrecRel
opPrec "<"  = PrecRel
opPrec "<=" = PrecRel
opPrec ">"  = PrecRel
opPrec ">=" = PrecRel
opPrec "&"  = PrecConj
opPrec "|"  = PrecDisj
opPrec _    = PrecAp

-- Determine the infix precedence of a Core expression.
exprPrec :: CoreExpr -> PrecLevel
exprPrec (ENum _) = PrecAtom
exprPrec (EVar x) | -- Operators as atoms should be wrapped in parens
                    prec /= PrecAp = PrecBase
                  | otherwise      = PrecAtom
  where prec = opPrec x
exprPrec (EConstr _                 _) = PrecAtom
exprPrec (EAp     (EAp (EVar op) _) _) = opPrec op
exprPrec (EAp     _                 _) = PrecAp
exprPrec (ELet _ _ _                 ) = PrecBase
exprPrec (ECase _ _                  ) = PrecBase
exprPrec (ELam  _ _                  ) = PrecBase

-- Expression to iseq
-- Exercise 1.3: Write remaining rules.
pprExpr :: CoreExpr -> Iseq
pprExpr = pprExpr' PrecBase

-- Helper for pprExpr. Mutually recursive with pprExpr''
pprExpr' :: PrecLevel -> CoreExpr -> Iseq
pprExpr' _ (ENum n) = iStr (show n)
pprExpr' _ (EVar v) = iStr v
pprExpr' _ e@(EAp e'@(EAp (EVar op) e1) e2)
  |
  -- Infix operator
    prec /= PrecAp
  = let asc = opAsc op
    in  iConcat
          [ pprExpr'' prec (asc /= AscLeft) e1
          , iStr " "
          , iStr op
          , iStr " "
          , pprExpr'' prec (asc /= AscRight) e2
          ]
  |
  -- Regular prefix function applied to two arguments. Treat
  -- function application (" ") as a left-associative infix operator
  -- with precedence PrecAp
    otherwise
  = iConcat [pprExpr'' PrecAp False e', iStr " ", pprExpr'' PrecAp True e2]
  where prec = exprPrec e
pprExpr' _ (EAp e1 e2) =
  -- Regular function application
  iConcat [pprExpr'' PrecAp False e1, iStr " ", pprExpr'' PrecAp True e2]
pprExpr' _ (ELet isrec defns expr) = iConcat
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
pprExpr' _ (ECase scrut alters) = iConcat
  [ iStr "case "
  , pprExpr scrut
  , iStr " of"
  , iNewline
  , iStr "  "
  , iIndent $ pprAlters alters
  ]
pprExpr' _ (EConstr tag arity) = iConcat
  [iStr "Pack{ ", iStr (show tag), iStr ", ", iStr (show arity), iStr " }"]
pprExpr' _ (ELam args body) = iConcat
  [iStr "\\ ", lhs, iStr " . ", pprExpr body]
  where lhs = iInterleave (iStr " ") $ iStr <$> args

-- Mutually-recursive helper to pprExpr' that wraps the expression in
-- parentheses if the context precedence is greater than the expression's
-- precedence. The second parameter denotes whether the associativity
-- indicates to wrap expressions of equal precedence:
-- - A left-associative operator needs to wrap expressions on the right
--   with the same precedence.
-- - A right-associative operator needs to wrap expressions on the left
--   with the same precedence.
-- - A non-associative operator needs to wrap expressions on both sides
--   with the same precedence.
pprExpr'' :: PrecLevel -> Bool -> CoreExpr -> Iseq
pprExpr'' prec wrapEqPrec e =
  if (if wrapEqPrec then (>=) else (>)) prec (exprPrec e)
    then iConcat [iStr "( ", iIndent $ pprExpr' prec e, iStr " )"]
    else pprExpr e

-- Alter list to iseq
pprAlters :: [CoreAlter] -> Iseq
pprAlters alters = iInterleave sep $ pprAlter <$> alters
  where sep = iConcat [iStr " ;", iNewline]

-- Alter to iseq
pprAlter :: CoreAlter -> Iseq
pprAlter (tag, args, body) = iConcat [lhs, iStr " -> ", pprExpr body]
 where
  tagSeq = iConcat [iStr "<", iStr (show tag), iStr ">"]
  lhs    = iInterleave (iStr " ") $ tagSeq : (iStr <$> args)

-- Definition list to iseq
pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep $ pprDefn <$> defns
  where sep = iConcat [iStr " ;", iNewline]

-- Definition to iseq
pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]

-- Useful helper function for Pretty-Printing Program
-- (to stdout). Can be used in conjunction with Parser.parse.
ppp :: CoreProgram -> IO ()
ppp = putStrLn . iDisplay . pprProgram
