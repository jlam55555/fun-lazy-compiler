module Evaluators.TemplateInstantiation.Evaluator
  ( runProg
  , getResult
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

-- Driver for the graph reduction implementation;
-- runs program and shows result
runProg :: String -> String
runProg = showResults . eval . compile . parse

-- Get result of running program
getResult :: String -> Node
getResult prog = hLookup h s
 where
  ([s], _, h, _, _) = last states
  states            = eval . compile . parse $ prog

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
  dispatch (NInd a                 ) = indStep state a

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
  root : _ = s'
  s'       = if length s < argsToDrop
    then error "scStep: not enough arguments for application"
    else drop (argsToDrop - 1) s
  argsToDrop  = length argNames + 1
  h'          = instantiateAndUpdate body root h e'
  -- Exercise 2.8: order matters here; if it were reversed, the
  -- outside environment would override the new bindings
  e'          = argBindings ++ e
  argBindings = zip argNames $ getArgs h s

-- Indirection node: equation 2.4; the indirection node gets
-- replaced with the address on the stack.
-- Exercise 2.13: Implement indirection nodes.
indStep :: TiState -> Addr -> TiState
indStep (s, d, h, e, stats) a = (a : tail s, d, h, e, stats)

-- Looks up all the arguments (names) for NAp nodes on the spine
getArgs :: TiHeap -> TiStack -> [Addr]
getArgs _ []      = error "getArgs: empty stack"
getArgs h (_ : s) = map getArg s
  where getArg a = arg where (NAp _ arg) = hLookup h a

-- Instantiate a supercombinator. Takes a sc, heap, and
-- environment (globals + args bindings).
-- (Instantiation is essentially substitution of the environment to
-- evaluate an expression, such as the body of a supercombinator or `let`.)
type TiInst = TiHeap -> TiEnv -> (TiHeap, Addr)

instantiate :: CoreExpr -> TiInst
instantiate (ENum n   ) h _ = hAlloc h (NNum n)
instantiate (EAp e1 e2) h e = hAlloc h'' (NAp a1 a2)
 where
  (h' , a1) = instantiate e1 h e
  (h'', a2) = instantiate e2 h' e
instantiate (EVar v) h e =
  (h, lookupDef (error $ "instantiate: unbound var " ++ show v) v e)
instantiate (EConstr tag arity) h e = instantiateConstr tag arity h e
instantiate (ELet isrec defs body) h e = instantiateLet isrec defs body h e
instantiate (ECase _ _) _ _ = error "instantiate: can't instantiate case exprs"
instantiate (ELam _ _) _ _ = error "instantiate: can't instantiate lambda fns"

instantiateConstr :: Int -> Int -> TiInst
instantiateConstr _ _ _ _ = error "instantiate: can't instantiate constructors"

-- Exercise 2.10, 2.11: Implement let(rec)
instantiateLet :: IsRec -> [(Name, CoreExpr)] -> CoreExpr -> TiInst
instantiateLet isRec bindings body h e = instantiate body h' e'
 where
  -- Augmented environment to evaluate body (and bindings in letrec)
  e'             = zip (bindersOf bindings) rhsAddrs ++ e
  (h', rhsAddrs) = mapAccuml instantiateBinding h $ rhssOf bindings
  instantiateBinding h'' body' = instantiate body' h'' e''
   where
    e'' | -- letrec: use augmented environment for bindings
          isRec == recursive = e'
        | -- let: use previous environment for bindings
          otherwise          = e

-- Exercise 2.12: The given program cannot exist in a strongly-typed language
-- since the binding `f = f x` is not well-typed (similar to how the Y-combinator
-- cannot be well-typed).

-- Exercise 2.14: Implement `instantiateAndUpdate`, used for the root of an
-- instantiation -- doesn't need to allocate a new node, since the application node
-- can be reused. (Recursive nodes do have to be allocated.) The second argument
-- is the address of the node to update
type TiUpdInst = Addr -> TiHeap -> TiEnv -> TiHeap

instantiateAndUpdate :: CoreExpr -> TiUpdInst
instantiateAndUpdate (ENum n   ) updAddr h _ = hUpdate h updAddr (NNum n)
instantiateAndUpdate (EAp e1 e2) updAddr h e = hUpdate h'' updAddr (NAp a1 a2)
 where
  (h' , a1) = instantiate e1 h e
  (h'', a2) = instantiate e2 h' e
-- Exercise 2.14: For `EVar`, we need to have an indirection because the
-- variable's address may be updated at some point.
instantiateAndUpdate (EVar v) updAddr h e = hUpdate h updAddr $ NInd a
  where a = lookupDef (error $ "instantiate: unbound var " ++ show v) v e
instantiateAndUpdate (EConstr tag arity) updAddr h e =
  instantiateAndUpdateConstr tag arity updAddr h e
instantiateAndUpdate (ELet isRec bindings body) updAddr h e =
  instantiateAndUpdateLet isRec bindings body updAddr h e
instantiateAndUpdate (ECase _ _) _ _ _ =
  error "instantiateAndUpdate: can't instantiate case"
instantiateAndUpdate (ELam _ _) _ _ _ =
  error "instantiateAndUpdate: can't instantiate lambda fns"

instantiateAndUpdateConstr :: Int -> Int -> TiUpdInst
instantiateAndUpdateConstr =
  error "instantiateAndUpdate: can't instantiate constructors"

-- Exercise 2.14: Note: we need to be careful to recursively call
-- `instantiateAndUpdate` on the body expression but `instantiate` on the
-- bindings expressions to avoid extra indirections
instantiateAndUpdateLet :: IsRec -> [(Name, CoreExpr)] -> CoreExpr -> TiUpdInst
instantiateAndUpdateLet isRec bindings body updAddr h e = instantiateAndUpdate
  body
  updAddr
  h'
  e'
 where
  -- Augmented environment to evaluate body (and bindings in letrec)
  e'             = zip (bindersOf bindings) rhsAddrs ++ e
  (h', rhsAddrs) = mapAccuml instantiateBinding h $ rhssOf bindings
  instantiateBinding h'' body' = instantiate body' h'' e''
   where
    e'' | -- letrec: use augmented environment for bindings
          isRec == recursive = e'
        | -- let: use previous environment for bindings
          otherwise          = e
