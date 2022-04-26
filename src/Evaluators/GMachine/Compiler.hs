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
compile prog = GmState initialCode [] h e statInitial
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
compileC _ _ = error "compileC: unimplemented expression form"

argOffset :: Int -> GmEnv Int -> GmEnv Int
argOffset n env = [ (v, n + m) | (v, m) <- env ]

initialCode :: GmCode
initialCode = [Pushglobal "main", Unwind]
