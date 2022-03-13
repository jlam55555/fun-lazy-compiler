module Evaluators.TemplateInstantiation.State
  ( TiState
  , TiHeap
  , TiStack
  , TiEnv
  , TiDump
  , initialTiDump
  , applyToStats
  ) where

import           Alloc
import           Data.AssocList
import           Evaluators.TemplateInstantiation.Node
import           Evaluators.TemplateInstantiation.Statistics
import           Language

-- Spine stack
type TiStack = [Addr]

-- Dump of stacks
type TiDump = [TiStack]

initialTiDump :: TiDump
initialTiDump = []

-- Address space
type TiHeap = Heap Node

-- Mapping from names to addresses
-- (called `TiGlobals` in the tutorial)
type TiEnv = AssocList Name Addr

-- Representation of the state of the template instantiation machine
type TiState = (TiStack, TiDump, TiHeap, TiEnv, TiStats)

-- Apply a function to the statistics component of the state
applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats statsFn (s, d, h, g, stats) = (s, d, h, g, statsFn stats)
