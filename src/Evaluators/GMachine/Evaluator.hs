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
dispatch (Pushglobal f)   = pushglobal f
dispatch (Pushint    n)   = pushint n
dispatch (Push       n)   = push n
dispatch (Update     n)   = update n
dispatch (Pop        n)   = pop n
dispatch (Alloc      n)   = alloc n
dispatch (Slide      n)   = slide n
dispatch Unwind           = unwind
dispatch Mkap             = mkap
dispatch Eval             = evalI
dispatch Add              = arithmetic2 (+)
dispatch Sub              = arithmetic2 (-)
dispatch Mul              = arithmetic2 (*)
dispatch Div              = arithmetic2 div
dispatch Neg              = arithmetic1 negate
dispatch Eq               = comparison (==)
dispatch Ne               = comparison (/=)
dispatch Lt               = comparison (<)
dispatch Le               = comparison (<=)
dispatch Gt               = comparison (>)
dispatch Ge               = comparison (>=)
-- dispatch (Cond t f      ) = cond t f -- replaced in Mark 6
dispatch (Pack t n      ) = pack t n
dispatch (Casejump rules) = casejump rules
dispatch (Split    n    ) = split n
dispatch Print            = printI

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

-- Allocate n items on the stack (filled with arbitrary values)
-- Implemented in Mark 3
alloc :: Int -> GmStateT
alloc n state = state { gmStack = s' ++ gmStack state, gmHeap = h' }
  where (h', s') = allocNodes n $ gmHeap state

-- Helper for `alloc`
allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
allocNodes 0 h = (h, [])
allocNodes n h = (h'', a : as)
 where
  (h' , as) = allocNodes (n - 1) h
  (h'', a ) = hAlloc h' $ NInd hNull

-- Moves the top element of the stack down n elements, discarding the
-- other elements
slide :: Int -> GmStateT
slide n state = state { gmStack = a : drop n as } where a : as = gmStack state

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
  d      = gmDump state

  dataNodeState [] = state
  dataNodeState ((is, s) : d') =
    state { gmCode = is, gmStack = a : s, gmDump = d' }

  -- Mark 4: NNum is final if the dump is empty, otherwise we pop a dumpItem
  -- from the stack;
  -- Mark 6: NConstr is treated the same as NNum
  newState (NNum _      ) = dataNodeState d
  newState (NConstr _  _) = dataNodeState d
  newState (NAp     a1 _) = state { gmCode = [Unwind], gmStack = a1 : a : as }
  -- Mark 5: unwinding may be necessary if a function does not directly
  -- evaluate to a number
  newState (NGlobal n c)
    | length as < n = state { gmCode  = is
                            , gmStack = last (gmStack state) : s
                            , gmDump  = d'
                            }
    | otherwise = state { gmCode = c, gmStack = rearrange n state }
    where ((is, s) : d') = d
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

-- Eval opcode, introduced in Mark 4
evalI :: GmStateT
evalI state = state { gmCode = [Unwind], gmStack = [a], gmDump = d' }
 where
  d'    = (gmCode state, s) : gmDump state
  a : s = gmStack state

-- Cond opcode, introduced in Mark 4
-- Replaced in Mark 6 using the structured data representation
-- cond :: GmCode -> GmCode -> GmStateT
-- cond t f state = state { gmCode = branch ++ is, gmStack = as }
--  where
--   is     = gmCode state
--   a : as = gmStack state
--   branch = getBranch $ hLookup (gmHeap state) a
--   getBranch (NNum 1) = t
--   getBranch (NNum 0) = f
--   getBranch _        = error "cond: top of stack is not boolean"

-- Helper function for arithmetic operators: takes a number and an initial
-- state, and returns a new state in which the number has been placed into the
-- heap, and a pointer to this new node left on top of the stack.
boxInteger :: Int -> GmState -> GmState
boxInteger n state = state { gmStack = a : gmStack state, gmHeap = h' }
  where (h', a) = hAlloc (gmHeap state) $ NNum n

-- Helper function for arithmetic operators: takes an address and a state,
-- and returns the number at that address
unboxInteger :: Addr -> GmState -> Int
unboxInteger a state = ub $ hLookup (gmHeap state) a
 where
  ub (NNum i) = i
  ub _        = error "unboxInteger: unboxing a non-integer"

-- Similar to `boxInteger`, but takes booleans and stores them as integers
-- Updated in Mark 6: now returns structured data representation of booleans
boxBoolean :: Bool -> GmState -> GmState
boxBoolean b state = state { gmStack = a : gmStack state, gmHeap = h' }
 where
  (h', a) = hAlloc (gmHeap state) $ NConstr b' []
  b' | -- 2 is tag of True
       b         = 2
     | -- 1 is tag of False
       otherwise = 1

-- Generic unary operator, introduced in Mark 4
primitive1
  :: (b -> GmState -> GmState) -- boxing function
  -> (Addr -> GmState -> a)    -- unboxing function
  -> (a -> b)                  -- operator
  -> (GmState -> GmState)      -- state transition
primitive1 box unbox op state = box (op $ unbox a state)
  $ state { gmStack = as }
  where a : as = gmStack state

-- Generic binary operator, introduced in Mark 4
primitive2
  :: (b -> GmState -> GmState)
  -> (Addr -> GmState -> a)
  -> (a -> a -> b)
  -> (GmState -> GmState)
primitive2 box unbox op state =
  box (op (unbox a0 state) (unbox a1 state)) $ state { gmStack = as }
  where a0 : a1 : as = gmStack state

-- Generic unary arithmetic
arithmetic1 :: (Int -> Int) -> (GmState -> GmState)
arithmetic1 = primitive1 boxInteger unboxInteger

-- Generic binary arithmetic
arithmetic2 :: (Int -> Int -> Int) -> (GmState -> GmState)
arithmetic2 = primitive2 boxInteger unboxInteger

-- Generic comparison function
comparison :: (Int -> Int -> Bool) -> GmState -> GmState
comparison = primitive2 boxBoolean unboxInteger

-- Structured data constructor; assumes saturated constructor;
-- Introduced in Mark 6
pack :: Int -> Int -> GmStateT
pack t n state = state { gmStack = a : drop n s, gmHeap = h' }
 where
  s       = gmStack state
  (h', a) = hAlloc (gmHeap state) $ NConstr t $ take n s

-- Case instruction; introduced in Mark 6; assumes element on top
-- of stack is a structured data element
casejump :: [(Int, GmCode)] -> GmStateT
casejump rules state = state { gmCode = is' ++ gmCode state }
 where
  a :          _ = gmStack state
  ( NConstr t _) = hLookup (gmHeap state) a
  is'            = lookupDef (error "casejump: failed match") t rules

-- Split instruction; introduced in Mark 6; puts the arguments
-- of the sructured data onto the stack; don't actually need n
split :: Int -> GmStateT
split _ state = state { gmStack = args ++ as }
 where
  a :             as = gmStack state
  ( NConstr _ args)  = hLookup (gmHeap state) a

-- Print instruction; introduced in Mark 6; recursively evaluates
-- arguments of a structured data for printing
printI :: GmStateT
printI state = state { gmCode = is', gmOutput = o', gmStack = s' }
 where
  a : as        = gmStack state
  node          = hLookup (gmHeap state) a
  -- Updated stack and instructions depends on the data node
  (is', o', s') = printI' node
  printI' (NNum n) = (gmCode state, gmOutput state ++ " " ++ show n, as)
  printI' (NConstr _ args) =
    (printIs (length args) ++ gmCode state, gmOutput state, args ++ as)
    where printIs n = concat $ take n $ repeat [Eval, Print]
  printI' _ = error "print: non-data node"
