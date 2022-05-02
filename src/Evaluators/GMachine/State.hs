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
  | Cond GmCode GmCode
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
  show Eval           = "eval"
  show Add            = "add"
  show Sub            = "sub"
  show Mul            = "mul"
  show Div            = "div"
  show Neg            = "neg"
  show Eq             = "eq"
  show Ne             = "ne"
  show Lt             = "lt"
  show Le             = "le"
  show Gt             = "gt"
  show Ge             = "ge"
  -- TODO: cond has gmCode subexpressions, need to print them out later
  show (Cond _ _)     = "cond"

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
  , gmDump  :: GmDump
  , gmHeap  :: GmHeap
  , gmEnv   :: GmEnv Addr
  , gmStats :: GmStats
  }
  deriving Show
