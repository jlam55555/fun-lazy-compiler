module Evaluators.GMachine.State where

import           Alloc
import           Data.AssocList
import           Language

-- Current instruction stream
type GmCode = [Instruction]

-- Stack-based instruction set
data Instruction
  = Unwind
  | Pushglobal Name
  | Pushint Int
  | Push Int
  | Mkap
  | Slide Int
  deriving (Show, Eq)

type GmStack = [Addr]
type GmHeap = Heap Node
type GmEnv a = AssocList Name a

data Node
  = NNum Int           -- Numbers
  | NAp Addr Addr      -- Fnaps
  | NGlobal Int GmCode -- Globals; replaces NSupercomb

-- Count evaluation steps
type GmStats = Int

statInitial :: GmStats
statInitial = 0

statIncSteps :: GmStats -> GmStats
statIncSteps = (+ 1)

statGetSteps :: GmStats -> Int
statGetSteps = id

-- G-Machine state
-- Deviating from the book to use record syntax
data GmState = GmState
  { gmCode  :: GmCode
  , gmStack :: GmStack
  , gmHeap  :: GmHeap
  , gmEnv   :: GmEnv Addr
  , gmStats :: GmStats
  }
  deriving Show
