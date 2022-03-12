module Evaluators.TemplateInstantiation.Node
  ( Node(..)
  ) where

import           Alloc
import           Language

-- Elements on the heap/stack
data Node = NAp Addr Addr                   -- Application
          | NSupercomb Name [Name] CoreExpr -- Supercombinator
          | NNum Int                        -- Number
