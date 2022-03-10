module Evaluators.TemplateInstantiation.Evaluator
  ( runProg
  , compile
  , eval
  , showResults
  ) where

import           Alloc
import           CorePrelude
import           Data.AssocList
import           Evaluators.TemplateInstantiation.State
import           Iseq
import           Language
import           Parser
import           Utils

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
  addressOfMain = lookupDef (error "main is not defined") "main" globalEnv

-- Initial heap comprises an address for each supercombinator
buildInitialHeap :: CoreProgram -> (TiHeap, TiEnv)
buildInitialHeap scDefs = mapAccuml allocateSc hInitial scDefs

-- Add a supercombinator to the heap
allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc h (name, args, body) = (h', (name, addr))
  where (h', addr) = hAlloc h $ NSupercomb name args body

-- Execute the program by performing repeated state transitions
-- until a final state is reached. The result is the list of all
-- states passed through.
eval :: TiState -> [TiState]
eval state = state : restStates
 where
  restStates | tiFinal state = []
             | otherwise     = eval nextState
  nextState = (doAdmin . step) state

-- Test if a state is final
tiFinal :: TiState -> Bool
tiFinal ([soleAddr], _, h, _, _) = isDataNode $ hLookup h soleAddr
tiFinal ([]        , _, _, _, _) = error "Empty stack!"
tiFinal _                        = False -- stack contains more than one element

-- Check if a Node is a data object rather than a redex
-- (supercombinator or application)
isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

-- Update statistics on TiState on an evaluation step
doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

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
numStep _ _ = error "Number applied as function!"

-- Step when an application node is reached: add the function to the spine
apStep :: TiState -> Addr -> Addr -> TiState
apStep (s, d, h, e, stats) a1 _ = (a1 : s, d, h, e, stats)

-- Step when a supercombinator node is reached: unwind the stack
-- and instantiate the sc with the environment (globals + args)
scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (s, d, h, e, stats) _ argNames body = (s', d, h', e, stats)
 where
  -- stack update: remove sc and args from stack, replace node
  -- (currently no node update, only replacement)
  s'               = resultAddr : drop (length argNames + 1) s
  (h', resultAddr) = instantiate body h env
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
  (h, lookupDef (error $ "Unbound var " ++ show v) v e)
instantiate (EConstr tag arity   ) h e = instantiateConstr tag arity h e
instantiate (ELet isrec defs body) h e = instantiateLet isrec defs body h e
instantiate (ECase _ _           ) _ _ = error "Can't instantiate case exprs"
instantiate (ELam _ _) _ _ = error "Not supporting lambda abstractions yet"

instantiateConstr :: Int -> Int -> TiInst
instantiateConstr _ _ _ _ = error "Can't instantiate constructors yet"

instantiateLet :: IsRec -> [(Name, CoreExpr)] -> CoreExpr -> TiInst
instantiateLet _ _ _ _ _ = error "Can't instantiate let(rec)s yet"

-- Format the result of `eval` for printing.
-- TODO: move this to a different module
showResults :: [TiState] -> String
showResults states =
  iDisplay $ iConcat [iLayn $ showState <$> states, showStats $ last states]

showState :: TiState -> Iseq
showState (s, _, h, _, _) = iConcat [showStack h s, iNewline]

showStack :: TiHeap -> TiStack -> Iseq
showStack h s = iConcat
  [ iStr "stack ["
  , iIndent $ iInterleave iNewline $ showStackItem <$> s
  , iStr " ]"
  ]
 where
  showStackItem a =
    iConcat [showFWAddr a, iStr ": ", showStkNode h $ hLookup h a]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode h (NAp fnAddr argAddr) = iConcat
  [ iStr "NAp "
  , showFWAddr fnAddr
  , iStr " "
  , showFWAddr argAddr
  , iStr " ("
  , showNode $ hLookup h argAddr
  , iStr ")"
  ]
showStkNode _ node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) =
  iConcat [iStr "NAp ", showAddr a1, iStr " ", showAddr a2]
showNode (NSupercomb name _ _) = iStr $ "NSupercomb " ++ name
showNode (NNum n             ) = iStr "NNum " `iAppend` iNum n

showAddr :: Addr -> Iseq
showAddr a = iStr $ show a

-- Show address in field of width 4
showFWAddr :: Addr -> Iseq
showFWAddr a = iStr $ space (4 - length str) ++ str where str = show a

showStats :: TiState -> Iseq
showStats (_, _, _, _, stats) = iConcat
  [ iNewline
  , iNewline
  , iStr "Total number of steps = "
  , iNum $ tiStatGetSteps stats
  ]
