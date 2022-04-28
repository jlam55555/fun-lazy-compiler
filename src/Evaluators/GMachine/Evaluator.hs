module Evaluators.GMachine.Evaluator
  ( eval
  ) where

import           Evaluators.GMachine.State

import           Alloc
import           Data.AssocList
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
dispatch (Update     n) = update n
dispatch (Pop        n) = pop n
dispatch Unwind         = unwind
dispatch Mkap           = mkap

-- Push global onto stack
pushglobal :: Name -> GmStateT
pushglobal f state = state { gmStack = a : gmStack state }
 where
  a =
    lookupDef (error $ "pushglobal: undeclared global: " ++ f) f $ gmEnv state

-- Allocate int and push onto stack
-- Exercise 3.6: Reuse number nodes
pushint :: Int -> GmStateT
pushint n state
  | a' /= badAddr = state { gmStack = a' : gmStack state }
  | otherwise = state { gmStack = a : gmStack state
                      , gmHeap  = h'
                      , gmEnv   = (show n, a) : gmEnv state
                      }
 where
  a'      = lookupDef badAddr (show n) $ gmEnv state
  (h', a) = hAlloc (gmHeap state) $ NNum n

-- Push n-th stack item onto the stack
-- Mark 3: use updated addressing mode
push :: Int -> GmStateT
push n state = state { gmStack = (as !! n) : as } where as = gmStack state

-- Allocate an application node from the top two items on the stack and
-- push that onto the stack
mkap :: GmStateT
mkap state = state { gmStack = a : as', gmHeap = h' }
 where
  (h', a)       = hAlloc (gmHeap state) $ NAp a1 a2
  a1 : a2 : as' = gmStack state

-- Remove n elements from the stack
pop :: Int -> GmStateT
pop n state = state { gmStack = drop n $ gmStack state }

-- Overwrite the (n+1)th stack item with an indirection to the item on
-- the top of the stack, and remove the top element of the stack
update :: Int -> GmStateT
update n state = state { gmStack = as, gmHeap = h' }
 where
  a : as = gmStack state
  h'     = hUpdate (gmHeap state) (as !! n) $ NInd a

-- -- Moves the top element of the stack down n elements, discarding the
-- -- other elements
-- slide :: Int -> GmStateT
-- slide n s = s { gmStack = a : drop n as } where a : as = gmStack s

-- Unwind continues evaluation after the construction of a supercombinator:
-- - If there is a number on top of the stack, then we're finished
-- - If there is an application node on top of the stack, then we continue
--   to unwind from the next node
-- - If there is a global node on top of the stack, then we reduce the
--   supercombinator. We check that there are enough nodes on the stack to
--   perform the reduction
-- - If there is an indirection on top of the stack, then we add the
--   replace the top node with the indirection address.
unwind :: GmStateT
unwind state = newState $ hLookup h a
 where
  a : as = gmStack state
  h      = gmHeap state
  newState (NNum _  ) = state
  newState (NAp a1 _) = state { gmCode = [Unwind], gmStack = a1 : a : as }
  newState (NGlobal n c)
    | length as < n = error "unwind: too few arguments to sc"
    | otherwise     = state { gmCode = c, gmStack = rearrange n state }
  newState (NInd a') = state { gmCode = [Unwind], gmStack = a' : as }

-- Helper function to rearrange the stack for updated Unwind in Mark 3
-- Exercise 3.12
rearrange :: Int -> GmState -> GmStack
rearrange n state = take n as' ++ drop n as
 where
  h   = gmHeap state
  as  = gmStack state
  as' = getArg . hLookup h <$> tail as
  getArg (NAp _ arg) = arg
  getArg _           = error "rearrange: non-ap node on stack during unwind"
