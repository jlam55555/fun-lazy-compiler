module Evaluators.TemplateInstantiation.Node
  ( Node(..)
  , Primitive(..)
  , trueNode
  , falseNode
  , boolToNode
  ) where

import           Alloc
import           Language

-- Elements on the heap/stack
data Node = NAp Addr Addr                   -- Application
          | NSupercomb Name [Name] CoreExpr -- Supercombinator
          | NNum Int                        -- Number
          | NInd Addr                       -- Indirection
          | NPrim Name Primitive            -- Primitive
          | NData Int [Addr]                -- Structured data
  deriving (Eq, Show)

data Primitive = Neg | Add | Sub | Mul | Div
  | Constr Int Int | If | CasePair
  | Greater | GreaterEq | Less | LessEq | Eq | NotEq
  deriving (Eq, Show)

-- Standard representations of booleans in Core
trueNode, falseNode :: Node
trueNode = NData 1 []
falseNode = NData 2 []

boolToNode :: Bool -> Node
boolToNode True  = trueNode
boolToNode False = falseNode
