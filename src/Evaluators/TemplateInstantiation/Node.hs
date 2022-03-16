module Evaluators.TemplateInstantiation.Node
  ( Node(..)
  , Primitive(..)
  , trueNode
  , falseNode
  , boolToNode
  , tagTrue
  , tagFalse
  , tagPair
  , tagNil
  , tagCons
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
  | Constr Int Int | If | CasePair | CaseList
  | Greater | GreaterEq | Less | LessEq | Eq | NotEq
  | Abort
  deriving (Eq, Show)

-- Standard representations of special structured datatypes in Core
tagTrue, tagFalse, tagPair, tagNil, tagCons :: Int
tagTrue = 0
tagFalse = 1
tagPair = 2
tagNil = 3
tagCons = 4

trueNode, falseNode :: Node
trueNode = NData tagTrue []
falseNode = NData tagFalse []

boolToNode :: Bool -> Node
boolToNode True  = trueNode
boolToNode False = falseNode
