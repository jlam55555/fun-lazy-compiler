module Evaluators.GMachine.State
  ( Instruction(..)
  , GmCode
  , GmStack
  , GmDump
  , GmDumpItem
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
  | Eval
  | Add | Sub | Mul | Div | Neg
  | Eq | Ne | Lt | Le | Gt | Ge
  -- | Cond GmCode GmCode -- replaced in Mark 6
  | Pack Int Int
  | Casejump [(Int, GmCode)]
  | Split Int
  | Print
  deriving Eq

type GmOutput = String

type GmStack = [Addr]
type GmHeap = Heap Node
type GmEnv a = AssocList Name a

type GmDump = [GmDumpItem]
type GmDumpItem = (GmCode, GmStack)

data Node
  = NNum Int           -- Numbers
  | NAp Addr Addr      -- Fnaps
  | NGlobal Int GmCode -- Globals; replaces NSupercomb
  | NInd Addr          -- Indirections
  | NConstr Int [Addr] -- Structured data

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
  { gmOutput :: GmOutput
  , gmCode   :: GmCode
  , gmStack  :: GmStack
  , gmDump   :: GmDump
  , gmHeap   :: GmHeap
  , gmEnv    :: GmEnv Addr
  , gmStats  :: GmStats
  }
