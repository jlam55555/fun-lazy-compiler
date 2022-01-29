module Parser
  ( syntax
  , parse
  ) where

import           Iseq
import           Language
import           Lexer
import           Parser.Subparser
import           PrettyPrint

-- Syntactical analysis driver. Selects the first complete parse (if any)
syntax :: [Token] -> CoreProgram
syntax = take_first_parse . pProgram
 where
  take_first_parse ((prog, []) : _     ) = prog
  take_first_parse (_          : others) = take_first_parse others
  take_first_parse _                     = error "Syntax error"

-- Full parser driver (lexer + parser)
parse :: String -> CoreProgram
parse = syntax . clex

-- Parse a program
pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

-- Parse a supercombinator definition
-- Exercise 1.20: Implment mk_sc
pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where mk_sc name args _ body = (name, args, body)

-- Parse an expression
pExpr, pAtom, pAp, pLet, pCase, pLam :: Parser CoreExpr
pExpr = foldl1 pAlt [pAtom, pAp, pLet, pCase, pLam]

-- Exercise 1.21: Complete the parser, except for EAp (and infix ops).
pAtom = foldl1 pAlt [pEVar, pENum, pConstr, pWrappedExpr]
 where
  pEVar   = pApply pVar EVar
  pENum   = pApply pNum ENum
  pConstr = pThen3 mk_constr pPre pInd pPost
  mk_constr _ eConstr _ = eConstr
  pPre         = pThen undefined (pLit "Pack") (pLit "{")
  pInd         = pThen3 (\n1 _ n2 -> EConstr n1 n2) pNum (pLit ",") pNum
  pPost        = pLit "}"
  pWrappedExpr = pThen3 (\_ e _ -> e) (pLit "(") pExpr (pLit ")")
pAp = pApply (pLit "mariesmiaserme") (const $ ENum 0)
pLet = pThen4 mk_let pLetKw pDefns (pLit "in") pExpr
 where
  mk_let isRec defns _ body = ELet isRec defns body
  pDefns = pOneOrMoreWithSep pDefn (pLit ";")
  pDefn  = pThen3 mk_defn pVar (pLit "=") pExpr
  mk_defn var _ def = (var, def)
  pLetKw = pApply (pAlt (pLit "let") (pLit "letrec")) mk_recursive
  mk_recursive kw | kw == "letrec" = recursive
                  | otherwise      = nonRecursive
pCase = pThen4 mk_case (pLit "case") pExpr (pLit "of") pAlts
 where
  mk_case _ scrut _ alts = ECase scrut alts
  pAlts    = pOneOrMoreWithSep pAltRule (pLit ";")
  pAltRule = pThen4 mk_alt pInd (pZeroOrMore pVar) (pLit "->") pExpr
  mk_alt ind args _ body = (ind, args, body)
  pInd = pThen3 (\_ n _ -> n) (pLit "<") pNum (pLit ">")
pLam = pThen4 mk_lam (pLit "\\") pArgs (pLit ".") pExpr
 where
  mk_lam _ args _ body = ELam args body
  pArgs = pOneOrMoreWithSep pVar (pLit ",")

-- Utility for parse and pretty-print program
pppp :: String -> IO ()
pppp = putStrLn . iDisplay . pprProgram . parse

-- Example from Exercise 1.21:
-- pppp "f = 3; g x y = let z = x in z; h x = case (let y = x in y) of <1> -> 2; <2> -> 5"

-- Exercise 1.22: Alternative <2> tends to attach to the inner case;
-- "dangling else" question.
-- pppp "f x y = case x of <1> -> case y of <1> -> 1; <2> -> 2"
