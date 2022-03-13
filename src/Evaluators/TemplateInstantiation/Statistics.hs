module Evaluators.TemplateInstantiation.Statistics
  ( TiStats
  , tiStatInitial
  , tiStatIncSteps
  , tiStatGetSteps
  , tiStatUpdateReductions
  , tiStatGetPrimitiveReductions
  , tiStatGetSupercombinatorReductions
  , tiStatUpdateMaxStackDepth
  , tiStatGetMaxStackDepth
  ) where

import           Evaluators.TemplateInstantiation.Node

-- Store statistics about evaluation
-- Exercise 2.7: Collect more execution statistics:
-- - `steps`: # of evaluation steps
-- - `pr`: # of primitive reductions
-- - `sr`: # of sc reductions
-- - `msd`: maximum stack depth
type TiStats = (Int, Int, Int, Int)

-- Initial evaluation statistics
tiStatInitial :: TiStats
tiStatInitial = (0, 0, 0, 0)

-- Helper update/getters for statistics
tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps (steps, pr, sr, msd) = (steps + 1, pr, sr, msd)

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps (steps, _, _, _) = steps

-- Node is the next node on the stack, which indicates the next
-- evaluation step (which may or may not be a reduction)
-- Exercise 2.16: updated to count primitives; note that primitive
-- reductions may be double-counted if the arguments are not
-- evaluated due to Equation 2.9
tiStatUpdateReductions :: Node -> TiStats -> TiStats
tiStatUpdateReductions (NPrim _ _) (steps, pr, sr, msd) =
  (steps, pr + 1, sr, msd)
tiStatUpdateReductions (NSupercomb _ _ _) (steps, pr, sr, msd) =
  (steps, pr, sr + 1, msd)
tiStatUpdateReductions _ stats = stats

tiStatGetPrimitiveReductions :: TiStats -> Int
tiStatGetPrimitiveReductions (_, pr, _, _) = pr

tiStatGetSupercombinatorReductions :: TiStats -> Int
tiStatGetSupercombinatorReductions (_, _, sr, _) = sr

tiStatUpdateMaxStackDepth :: Int -> TiStats -> TiStats
tiStatUpdateMaxStackDepth msd' (steps, pr, sr, msd) =
  (steps, pr, sr, max msd msd')

tiStatGetMaxStackDepth :: TiStats -> Int
tiStatGetMaxStackDepth (_, _, _, msd) = msd
