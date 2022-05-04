module Evaluators.GMachine.Compiler
  ( compile
  , compileSc
  , buildInitialHeap
  , initialCode
  , GmCompiledSC
  ) where

import           Evaluators.GMachine.State

import           Alloc
import           CorePrelude
import           Data.AssocList
import           Language
import           Utils

type GmCompiledSC = (Name, Int, GmCode)

type GmCompiler = CoreExpr -> GmEnv Int -> GmCode

compile :: CoreProgram -> GmState
compile prog = GmState initialCode [] [] h e statInitial
  where (h, e) = buildInitialHeap prog

buildInitialHeap :: CoreProgram -> (GmHeap, GmEnv Addr)
buildInitialHeap prog = mapAccuml allocSc hInitial compiled
  where compiled = (compileSc <$> preludeDefs ++ prog) ++ compiledPrimitives

compiledPrimitives :: [GmCompiledSC]
compiledPrimitives =
  [ compile2 "+" Add
  , compile2 "-" Sub
  , compile2 "*" Mul
  , compile2 "/" Div
  , compile1 "negate" Neg
  , compile2 "==" Eq
  , compile2 "~=" Ne
  , compile2 "<"  Lt
  , compile2 "<=" Le
  , compile2 ">"  Gt
  , compile2 ">=" Ge
  , compiledIf
  ]
 where
  compile2 op opcode =
    (op, 2, [Push 1, Eval, Push 1, Eval, opcode, Update 2, Pop 2, Unwind])
  compile1 op opcode = (op, 1, [Push 0, Eval, opcode, Update 1, Pop 1, Unwind])
  compiledIf =
    ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])

-- Introduced in Mark 5 with strict evaluation contexts
builtInBinary :: AssocList Name Instruction
builtInBinary =
  [ ("+" , Add)
  , ("-" , Sub)
  , ("*" , Mul)
  , ("/" , Div)
  , ("==", Eq)
  , ("~=", Ne)
  , (">=", Ge)
  , (">" , Gt)
  , ("<=", Le)
  , ("<" , Lt)
  ]

allocSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocSc h (f, nargs, is) = (h', (f, a))
  where (h', a) = hAlloc h $ NGlobal nargs is

compileSc :: CoreScDefn -> GmCompiledSC
compileSc (f, args, body) = (f, length args, compileR body $ zip args [0 ..])

-- R compilation scheme is driver for supercombinator compilation.
-- I've found that this is called the F compilation scheme in another
-- document, "Efficient compilation of lazy evaluation"
compileR :: GmCompiler
compileR e env = compileE e env ++ [Update d, Pop d, Unwind]
  where d = length env

-- E compilation scheme compiles in a strict context (to WHNF)
-- (introduced in Mark 5)
compileE :: GmCompiler
-- Numbers are already in WHNF, return them
compileE (ENum n) _ = [Pushint n]
-- For let bindings, we can evaluate the body in a strict context
compileE (ELet isRec defs e) env | isRec     = compileLetrec compileE defs e env
                                 | otherwise = compileLet compileE defs e env
-- Arithmetic in a strict evaluation context is simplified
compileE (EAp (EAp (EVar binOp) e0) e1) env | hasEntry binOp builtInBinary =
  compileE e1 env
    ++ compileE e0 (argOffset 1 env)
    ++ [lookupDef Add binOp builtInBinary]
compileE (EAp (EVar "negate") e) env = compileE e env ++ [Neg]
-- Conditionals may be compiled in a strict context
compileE (EAp (EAp (EAp (EVar "if") e0) e1) e2) env =
  compileE e0 env ++ [Cond (compileE e1 env) (compileE e2 env)]
-- Fallback case: evaluate other expressions in a non-strict context
compileE e env = compileC e env ++ [Eval]

-- C compilation scheme compiles in a lazy context
compileC :: GmCompiler
compileC (EVar x) env | n >= 0    = [Push n]
                      | otherwise = [Pushglobal x]
  where n = lookupDef (-1) x env
compileC (ENum n) _ = [Pushint n]
compileC (EAp e1 e2) env =
  compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]
compileC (ELet isRec defs e) env | isRec     = compileLetrec compileC defs e env
                                 | otherwise = compileLet compileC defs e env
compileC _ _ = error "compileC: unimplemented expression form"

-- Compile a letrec expression
compileLetrec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetrec comp defs body env =
  [Alloc n]
    ++ compileLetRec' defs env'
    ++ comp body env'
    ++ [Slide $ length defs]
 where
  n    = length defs
  env' = compileArgs defs env

-- Compile the binding of a letrec expression
compileLetRec' :: [(Name, CoreExpr)] -> GmEnv Int -> GmCode
compileLetRec' defs env = concat $ compileBinding <$> zip defs
                                                          [n - 1, n - 2 .. 0]
 where
  compileBinding ((_, definiens), n') = compileC definiens env ++ [Update n']
  n = length defs

-- Compile a non-recursive let expression
compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLet comp defs body env =
  compileLet' defs env ++ comp body env' ++ [Slide $ length defs]
  where env' = compileArgs defs env

-- Compiles the bindings of a let expression
compileLet' :: [(Name, CoreExpr)] -> GmEnv Int -> GmCode
compileLet' [] _ = []
compileLet' ((_, definiens) : defs) env =
  compileC definiens env ++ compileLet' defs (argOffset 1 env)

-- Provides updated environment for evaluating the body of a `let` expression
compileArgs :: [(Name, CoreExpr)] -> GmEnv Int -> GmEnv Int
compileArgs defs env =
  zip (fst <$> defs) [n - 1, n - 2 .. 0] ++ argOffset n env
  where n = length defs

-- Increase the argument stack offset by n
argOffset :: Int -> GmEnv Int -> GmEnv Int
argOffset n env = [ (v, n + m) | (v, m) <- env ]

initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]
