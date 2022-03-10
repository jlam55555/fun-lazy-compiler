module Evaluators.TemplateInstantiation.State
  ( TiState
  , TiHeap
  , TiStack
  , TiEnv
  , TiDump
  , Node(..)
  , initialTiDump
  , tiStatInitial
  , tiStatGetSteps
  , tiStatIncSteps
  , applyToStats
  ) where

import           Alloc
import           Data.AssocList
import           Language

-- Spine stack
type TiStack = [Addr]

-- Dump of stacks
data TiDump = DummyTiDump

initialTiDump :: TiDump
initialTiDump = DummyTiDump

-- Address space
type TiHeap = Heap Node

-- Elements on the heap
data Node = NAp Addr Addr                   -- Application
          | NSupercomb Name [Name] CoreExpr -- Supercombinator
          | NNum Int                        -- Number

-- Mapping from names to addresses
-- (called `TiGlobals` in the tutorial)
type TiEnv = AssocList Name Addr

-- Store statistics about evaluation
type TiStats = Int -- currently only store number of steps

-- Initial evaluation statistics
tiStatInitial :: TiStats
tiStatInitial = 0

-- Update statistics
tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps = (1 +)

-- Get number of steps from statistics
tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

-- Apply a function to the statistics component of the state
applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats statsFn (s, d, h, g, stats) = (s, d, h, g, statsFn stats)

-- Representation of the state of the template instantiation machine
type TiState = (TiStack, TiDump, TiHeap, TiEnv, TiStats)
