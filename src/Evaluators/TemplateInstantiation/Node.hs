module Evaluators.TemplateInstantiation.Node
  ( Node(..)
  , Primitive(..)
  ) where

import           Alloc
import           Language

-- Elements on the heap/stack
data Node = NAp Addr Addr                   -- Application
          | NSupercomb Name [Name] CoreExpr -- Supercombinator
          | NNum Int                        -- Number
          | NInd Addr                       -- Indirection
          | NPrim Name Primitive            -- Primitive
  deriving (Eq, Show)

data Primitive = Neg | Add | Sub | Mul | Div
  deriving (Eq, Show)
