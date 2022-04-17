module Evaluators.GMachine.Evaluator
  ( eval
  ) where

import           Alloc
import           Data.AssocList
import           Evaluators.GMachine.State
import           Language

eval :: GmState -> [GmState]
eval state = state : restStates
 where
  restStates | gmFinal state = []
             | otherwise     = eval nextState
  nextState = doAdmin $ step state
  doAdmin state' = state' { gmStats = statIncSteps $ gmStats state' }

-- Test if `GmState` is final
gmFinal :: GmState -> Bool
gmFinal = null . gmCode

-- Perform an evaluation step (perform the next instruction)
step :: GmState -> GmState
step state = dispatch i $ state { gmCode = is } where i : is = gmCode state

type GmStateT = GmState -> GmState

dispatch :: Instruction -> GmStateT
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint    n) = pushint n
dispatch (Push       n) = push n
dispatch (Slide      n) = slide n
dispatch Unwind         = unwind
dispatch Mkap           = mkap

-- Push global onto stack
pushglobal :: Name -> GmStateT
pushglobal f state = state { gmStack = a : gmStack state }
 where
  a =
    lookupDef (error $ "pushglobal: undeclared global: " ++ f) f $ gmEnv state

-- Allocate int and push onto stack
pushint :: Int -> GmStateT
pushint n s = s { gmStack = a : gmStack s, gmHeap = h' }
  where (h', a) = hAlloc (gmHeap s) $ NNum n

-- Push n-th arg on stack onto stack
push :: Int -> GmStateT
push n s = s { gmStack = a : as }
 where
  as = gmStack s
  a  = getArg $ hLookup (gmHeap s) $ as !! (n + 1)
  getArg (NAp _ a2) = a2
  getArg _          = error "push: attempt to retrieve arg of non-ap node"

-- Allocate an application node from the top two items on the stack and
-- push that onto the stack
mkap :: GmStateT
mkap s = s { gmStack = a : as', gmHeap = h' }
 where
  (h', a)       = hAlloc (gmHeap s) $ NAp a1 a2
  a1 : a2 : as' = gmStack s

-- Moves the top element of the stack down n elements, discarding the
-- other elements
slide :: Int -> GmStateT
slide n s = s { gmStack = a : drop n as } where a : as = gmStack s

-- Unwind continues evaluation after the construction of a supercombinator:
-- - If there is a number on top of the stack, then we're finished
-- - If there is an application node on top of the stack, then we continue
--   to unwind from the next node
-- - If there is a global node on top of the stack, then we reduce the
--   supercombinator. We check that there are enough nodes on the stack to
--   perform the reduction
unwind :: GmStateT
unwind s = newState $ hLookup h a
 where
  a : as = gmStack s
  h      = gmHeap s
  newState (NNum _  ) = s
  newState (NAp a1 _) = s { gmCode = [Unwind], gmStack = a1 : a : as }
  newState (NGlobal n c)
    | length as < n = error "unwind: too few arguments to sc"
    | otherwise     = s { gmCode = c }
