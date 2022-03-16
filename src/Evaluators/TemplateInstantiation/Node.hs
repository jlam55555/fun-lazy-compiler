module Evaluators.TemplateInstantiation.Node
  ( Node(..)
  , trueNode
  , falseNode
  , boolToNode
  , tagTrue
  , tagFalse
  , tagNil
  , tagCons
  ) where

import           Alloc
import           Language

import           Evaluators.TemplateInstantiation.Primitives

-- Elements on the heap/stack
data Node = NAp Addr Addr                   -- Application
          | NSupercomb Name [Name] CoreExpr -- Supercombinator
          | NNum Int                        -- Number
          | NInd Addr                       -- Indirection
          | NPrim Name Primitive            -- Primitive
          | NData Int [Addr]                -- Structured data
  deriving (Eq, Show)

-- Standard representations of special structured datatypes in Core
-- Note: `tagCons` is used for both `Pair` and `Cons`, which are
-- functionally equal (aliases), and methods for one can be used
-- for the other.
tagTrue, tagFalse, tagNil, tagCons :: Int
tagTrue = 0
tagFalse = 1
tagNil = 3
tagCons = 4

trueNode, falseNode :: Node
trueNode = NData tagTrue []
falseNode = NData tagFalse []

boolToNode :: Bool -> Node
boolToNode True  = trueNode
boolToNode False = falseNode
