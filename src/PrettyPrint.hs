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

-- Helper to determine whether a core expression is an infix operator
isInfix :: CoreExpr -> Bool
isInfix (EVar op) =
  elem op ["*", "/", "+", "-", "==", "~=", ">", ">=", "<", "<=", "&", "|"]
isInfix _ = False

data Associativity = AscLeft | AscRight | AscNone
  deriving Eq

-- Associativity of an operator/function in the function position
-- of a EAp. If it is not one of the infix operators, it is an
-- ordinary prefix function (in which space is left-associative).
opAsc :: CoreExpr -> Associativity
opAsc (EVar op)
  | op `elem` ["*", "+", "&", "|"] = AscRight
  | op `elem` ["/", "-", "==", "~=", ">", ">=", "<", "<="] = AscNone
  | otherwise                      = AscLeft
opAsc _ = AscLeft

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

-- Determine the infix precedence of a Core expression.
opPrec :: CoreExpr -> PrecLevel
opPrec (EVar "*"   ) = PrecMulDiv
opPrec (EVar "/"   ) = PrecMulDiv
opPrec (EVar "+"   ) = PrecAddSub
opPrec (EVar "-"   ) = PrecAddSub
opPrec (EVar "=="  ) = PrecRel
opPrec (EVar "~="  ) = PrecRel
opPrec (EVar "<"   ) = PrecRel
opPrec (EVar "<="  ) = PrecRel
opPrec (EVar ">"   ) = PrecRel
opPrec (EVar ">="  ) = PrecRel
opPrec (EVar "&"   ) = PrecConj
opPrec (EVar "|"   ) = PrecDisj
opPrec (ENum _     ) = PrecAtom
opPrec (EVar _     ) = PrecAtom
opPrec (EConstr _ _) = PrecAtom
opPrec (EAp op _) =
  -- Returns infix operator precedence or PrecAp
  let prec = opPrec op
  in  if prec >= PrecDisj && prec <= PrecMulDiv then prec else PrecAp
opPrec (ELet _ _ _) = PrecBase
opPrec (ECase _ _ ) = PrecBase
opPrec (ELam  _ _ ) = PrecBase

-- Expression to iseq
-- Exercise 1.3: Write remaining rules.
pprExpr :: CoreExpr -> Iseq
pprExpr = pprExpr' PrecBase

-- Helper for pprExpr. Mutually recursive with pprExpr''
pprExpr' :: PrecLevel -> CoreExpr -> Iseq
pprExpr' _ (ENum n) = iStr (show n)
pprExpr' _ (EVar v) = iStr v
pprExpr' _ (EAp (EAp op e1) e2)
  |
  -- Infix operator
    isInfix op
  = let asc  = opAsc op
        prec = opPrec op
    in  iConcat
          [ pprExpr'' prec (asc /= AscLeft) e1
          , iStr " "
          , pprExpr op
          , iStr " "
          , pprExpr'' prec (asc /= AscRight) e2
          ]
  |
  -- Regular function composition; almost same as above, treat
  -- function application as a left-associative infix operator " "
  -- with precedence PrecAp
    otherwise
  = iConcat
    [pprExpr'' PrecAp False (EAp op e1), iStr " ", pprExpr'' PrecAp True e2]
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
pprExpr' _ (ECase scrut alters) =
  iConcat [iStr "case ", pprExpr scrut, iStr " of ", pprAlters alters]
pprExpr' _ (EConstr tag arity) = iConcat
  [iStr "Pack{ ", iStr (show tag), iStr ", ", iStr (show arity), iStr " }"]
pprExpr' _ (ELam args body) = iConcat
  [iStr "\\ ", lhs, iStr " . ", pprExpr body]
  where lhs = iConcat (map iStr args)

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
  if (if wrapEqPrec then (>=) else (>)) prec (opPrec e)
    then iConcat [iStr "( ", iIndent $ pprExpr' prec e, iStr " )"]
    else pprExpr e

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
