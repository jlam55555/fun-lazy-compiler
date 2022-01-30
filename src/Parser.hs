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
pProgram = pOneOrMoreWithSep pSc pSem

-- Symbols
pLPar, pRPar, pLBkt, pRBkt, pLAng, pRAng, pCom, pSem, pDot, pEq, pArr, pBSl
  :: Parser String
[pLPar, pRPar, pLBkt, pRBkt, pLAng, pRAng, pCom, pSem, pDot, pEq, pArr, pBSl] =
  map pLit ["(", ")", "{", "}", "<", ">", ",", ";", ".", "=", "->", "\\"]

-- Keywords
pKwCase, pKwLet, pKwLetrec, pKwPack, pKwOf, pKwIn :: Parser String
[pKwCase, pKwLet, pKwLetrec, pKwPack, pKwOf, pKwIn] =
  map pLit ["case", "let", "letrec", "Pack", "of", "in"]

-- Parse a supercombinator definition
-- Exercise 1.20: Implment mk_sc
pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) pEq pExpr
  where mk_sc name args _ body = (name, args, body)

-- Parse an expression
pExpr, pAtom, pAp, pLet, pCase, pLam :: Parser CoreExpr
pExpr = foldl1 pAlt [pAtom, pAp, pLet, pCase, pLam]

-- Exercise 1.21: Complete the parser, except for EAp (and infix ops).
-- Exercise 1.23: Implement mk_ap_chain for function application.
pAtom = foldl1 pAlt [pEVar, pENum, pConstr, pWrappedExpr]
 where
  mk_constr _ eConstr _ = eConstr
  pEVar        = pApply pVar EVar
  pENum        = pApply pNum ENum
  pConstr      = pThen3 mk_constr pPre pInd pRBkt
  pPre         = pThen undefined pKwPack pLBkt
  pInd         = pThen3 (\n1 _ n2 -> EConstr n1 n2) pNum pCom pNum
  pWrappedExpr = pThen3 (\_ e _ -> e) pLPar pExpr pRPar

pAp = pApply (pOneOrMore pAtom) mk_ap_chain where mk_ap_chain = foldl1 EAp

pLet = pThen4 mk_let pLetKw pDefns pKwIn pExpr
 where
  mk_let isRec defns _ body = ELet isRec defns body
  mk_defn var _ def = (var, def)
  pDefns = pOneOrMoreWithSep pDefn pSem
  pDefn  = pThen3 mk_defn pVar pEq pExpr
  pLetKw = pApply (pAlt pKwLet pKwLetrec) $ (==) "letrec"

pCase = pThen4 mk_case pKwCase pExpr pKwOf pAlts
 where
  mk_case _ scrut _ alts = ECase scrut alts
  mk_alt ind args _ body = (ind, args, body)
  pAlts    = pOneOrMoreWithSep pAltRule pSem
  pAltRule = pThen4 mk_alt pInd (pZeroOrMore pVar) pArr pExpr
  pInd     = pThen3 (\_ n _ -> n) pLAng pNum pRAng

pLam = pThen4 mk_lam pBSl pArgs pDot pExpr
 where
  mk_lam _ args _ body = ELam args body
  pArgs = pOneOrMoreWithSep pVar pCom

-- Utility for parse and pretty-print program
pppp :: String -> IO ()
pppp = putStrLn . iDisplay . pprProgram . parse

-- Example from Exercise 1.21:
-- pppp "f = 3; g x y = let z = x in z; h x = case (let y = x in y) of <1> -> 2; <2> -> 5"

-- Exercise 1.22: Alternative <2> tends to attach to the inner case;
-- "dangling else" question.
-- pppp "f x y = case x of <1> -> case y of <1> -> 1; <2> -> 2"
