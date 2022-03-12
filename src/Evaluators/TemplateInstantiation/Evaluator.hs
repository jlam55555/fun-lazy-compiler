module Evaluators.TemplateInstantiation.Evaluator
  ( runProg
  , compile
  , eval
  , showResults
  ) where

import           Data.AssocList

import           Alloc
import           CorePrelude
import           Language
import           Parser
import           Utils

import           Evaluators.TemplateInstantiation.Node
import           Evaluators.TemplateInstantiation.PrintUtils
import           Evaluators.TemplateInstantiation.State
import           Evaluators.TemplateInstantiation.Statistics

-- Driver for the graph reduction implementation
runProg :: String -> String
runProg = showResults . eval . compile . parse

-- Translate the program into a form suitable for execution
compile :: CoreProgram -> TiState
compile program =
  (initialStack, initialTiDump, initialHeap, globalEnv, tiStatInitial)
 where
  scDefs                   = program ++ preludeDefs ++ extraPreludeDefs
  (initialHeap, globalEnv) = buildInitialHeap scDefs
  initialStack             = [addressOfMain]
  addressOfMain =
    lookupDef (error "compile: main is not defined") "main" globalEnv

-- Initial heap comprises an address for each supercombinator
buildInitialHeap :: CoreProgram -> (TiHeap, TiEnv)
buildInitialHeap scDefs = mapAccuml allocateSc hInitial scDefs

-- Add a supercombinator to the heap
allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc h (name, args, body) = (h', (name, a))
  where (h', a) = hAlloc h $ NSupercomb name args body

-- Execute the program by performing repeated state transitions
-- until a final state is reached. The result is the list of all
-- states passed through.
-- Exercise 2.9: This formulation is more useful than the suggested
-- one because it will still show the final state if there is an
-- error thrown in `tiFinal` (empty stack error)
eval :: TiState -> [TiState]
eval state = state : restStates
 where
  restStates | tiFinal state = []
             | otherwise     = eval nextState
  nextState = (doAdmin . step) state

-- Test if a state is final
tiFinal :: TiState -> Bool
tiFinal ([soleAddr], _, h, _, _) = isDataNode $ hLookup h soleAddr
tiFinal ([]        , _, _, _, _) = error "tiFinal: empty stack"
tiFinal _                        = False -- stack contains more than one element

-- Check if a Node is a data object rather than a redex
-- (supercombinator or application)
isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

-- Update statistics on TiState on an evaluation step
doAdmin :: TiState -> TiState
doAdmin state = applyToStats updateFn state
 where
  updateFn =
    tiStatIncSteps
      . tiStatUpdateReductions (hLookup h $ head s)
      . tiStatUpdateMaxStackDepth (length s)
  (s, _, h, _, _) = state

-- Maps a state to its successor following the stated evaluation rules
step :: TiState -> TiState
step state = dispatch $ hLookup h $ head s
 where
  (s, _, h, _, _) = state
  dispatch (NNum n                 ) = numStep state n
  dispatch (NAp a1 a2              ) = apStep state a1 a2
  dispatch (NSupercomb sc args body) = scStep state sc args body

-- Number should not be on the stack spine
numStep :: TiState -> Int -> TiState
numStep _ _ = error "numStep: number applied as function"

-- Step when an application node is reached: add the function to the spine
apStep :: TiState -> Addr -> Addr -> TiState
apStep (s, d, h, e, stats) a1 _ = (a1 : s, d, h, e, stats)

-- Step when a supercombinator node is reached: unwind the stack
-- and instantiate the sc with the environment (globals + args)
-- Exercise 2.6. If there are not enough stack elements, throw
-- a suitable error message.
scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (s, d, h, e, stats) _ argNames body = (s', d, h', e, stats)
 where
  -- stack update: remove sc and args from stack, replace node
  -- (currently no node update, only replacement)
  s'        = resultAddr : prevStack
  prevStack = if length s < argsToDrop
    then error "scStep: not enough arguments for application"
    else drop argsToDrop s
  argsToDrop       = length argNames + 1
  (h', resultAddr) = instantiate body h env
  -- Exercise 2.8: order matters here; if it were reversed, the
  -- outside environment would override the new bindings
  env              = argBindings ++ e
  argBindings      = zip argNames $ getArgs h s

-- Looks up all the arguments (names) for NAp nodes on the spine
getArgs :: TiHeap -> TiStack -> [Addr]
getArgs _ []      = error "getArgs: empty stack"
getArgs h (_ : s) = map getArg s
  where getArg a = arg where (NAp _ arg) = hLookup h a

-- Instantiate a supercombinator. Takes a sc, heap, and
-- environment (globals + args bindings).
type TiInst = TiHeap -> TiEnv -> (TiHeap, Addr)

instantiate :: CoreExpr -> TiInst
instantiate (ENum n   ) h _ = hAlloc h (NNum n)
instantiate (EAp e1 e2) h e = hAlloc h2 (NAp a1 a2)
 where
  (h1, a1) = instantiate e1 h e
  (h2, a2) = instantiate e2 h1 e
instantiate (EVar v) h e =
  (h, lookupDef (error $ "instantiate: unbound var " ++ show v) v e)
instantiate (EConstr tag arity) h e = instantiateConstr tag arity h e
instantiate (ELet isrec defs body) h e = instantiateLet isrec defs body h e
instantiate (ECase _ _) _ _ = error "instantiate: can't instantiate case exprs"
instantiate (ELam _ _) _ _ = error "instantiate: can't instantiate lambda fns"

instantiateConstr :: Int -> Int -> TiInst
instantiateConstr _ _ _ _ = error "instantiate: can't instantiate constructors"

instantiateLet :: IsRec -> [(Name, CoreExpr)] -> CoreExpr -> TiInst
instantiateLet _ _ _ _ _ = error "instantiate: can't instantiate let(rec)"
