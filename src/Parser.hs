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
pExpr = foldl1 pAlt [pLet, pCase, pLam, pExpr1]

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

-- Helper data structure for implementing grammar with epsilon
-- (no-op) for efficiency reasons
data PartialExpr = NoOp | FoundOp Name CoreExpr

-- Helper function for converting PartialExpr to CoreExpr
-- for efficient infix operator parsing
assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp            = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

-- Infix operator precedence levels
pExpr1, pExpr2, pExpr3, pExpr4, pExpr5, pExpr6 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c
pExpr2 = pThen assembleOp pExpr3 pExpr2c
pExpr3 = pThen assembleOp pExpr4 pExpr3c
pExpr4 = pThen assembleOp pExpr5 pExpr4c
pExpr5 = pThen assembleOp pExpr6 pExpr5c
pExpr6 = pAp

-- Infix operator precedence levels with epsilons
pExpr1c, pExpr2c, pExpr3c, pExpr4c, pExpr5c :: Parser PartialExpr
pExpr1c = pAlt pOr $ pEmpty NoOp where pOr = pThen FoundOp (pLit "|") pExpr1
pExpr2c = pAlt pAnd $ pEmpty NoOp where pAnd = pThen FoundOp (pLit "&") pExpr2
pExpr3c = pAlt pRelOp $ pEmpty NoOp
 where
  pRelOp = pThen FoundOp (foldl1 pAlt $ map pLit relOps) pExpr3
  relOps = ["==", "~=", "<", "<=", ">", ">="]
pExpr4c = pAlt pAddSub $ pEmpty NoOp
  where pAddSub = pThen FoundOp (pAlt (pLit "+") (pLit "-")) pExpr4
pExpr5c = pAlt pMulDiv $ pEmpty NoOp
  where pMulDiv = pThen FoundOp (pAlt (pLit "*") (pLit "/")) pExpr5

-- Utility for parse and pretty-print program
pppp :: String -> IO ()
pppp = putStrLn . iDisplay . pprProgram . parse

-- Example from Exercise 1.21:
-- pppp "f = 3; g x y = let z = x in z; h x = case (let y = x in y) of <1> -> 2; <2> -> 5"

-- Exercise 1.22: Alternative <2> tends to attach to the inner case;
-- "dangling else" question.
-- pppp "f x y = case x of <1> -> case y of <1> -> 1; <2> -> 2"
