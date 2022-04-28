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
compiledPrimitives = []

allocSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocSc h (f, nargs, is) = (h', (f, a))
  where (h', a) = hAlloc h $ NGlobal nargs is

compileSc :: CoreScDefn -> GmCompiledSC
compileSc (f, args, body) = (f, length args, compileR body $ zip args [0 ..])

compileR :: GmCompiler
compileR e env = compileC e env ++ [Update d, Pop d, Unwind]
  where d = length env

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
