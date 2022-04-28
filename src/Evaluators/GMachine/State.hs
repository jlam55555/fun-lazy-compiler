module Evaluators.GMachine.State
  ( Instruction(..)
  , GmCode
  , GmStack
  , GmHeap
  , GmEnv
  , GmState(..)
  , Node(..)
  , statInitial
  , statIncSteps
  , statGetSteps
  ) where

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
  | Update Int
  | Pop Int
  | Alloc Int
  | Slide Int
  deriving Eq

instance Show Instruction where
  show Unwind         = "unwind"
  show (Pushglobal n) = "pushglobal " ++ n
  show (Pushint    n) = "pushint " ++ show n
  show (Push       n) = "pusharg " ++ show n
  show Mkap           = "mkap"
  show (Update n)     = "update " ++ show n
  show (Pop    n)     = "pop " ++ show n
  show (Alloc  n)     = "alloc " ++ show n
  show (Slide  n)     = "slide " ++ show n

type GmStack = [Addr]
type GmHeap = Heap Node
type GmEnv a = AssocList Name a

data Node
  = NNum Int           -- Numbers
  | NAp Addr Addr      -- Fnaps
  | NGlobal Int GmCode -- Globals; replaces NSupercomb
  | NInd Addr          -- Indirections

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
