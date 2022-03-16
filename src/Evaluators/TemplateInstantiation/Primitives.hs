module Evaluators.TemplateInstantiation.Primitives
  ( Primitive(..)
  , primitives
  ) where

import           Data.AssocList
import           Language

data Primitive = Neg | Add | Sub | Mul | Div
  | Constr Int Int | If | CasePair | CaseList
  | Greater | GreaterEq | Less | LessEq | Eq | NotEq
  | Abort
  deriving (Eq, Show)

-- Add primitives to the heap
primitives :: AssocList Name Primitive
primitives =
  [ ("negate"  , Neg)
  , ("+"       , Add)
  , ("-"       , Sub)
  , ("*"       , Mul)
  , ("/"       , Div)
  , ("if"      , If)
  , (">"       , Greater)
  , (">="      , GreaterEq)
  , ("<"       , Less)
  , ("<="      , LessEq)
  , ("=="      , Eq)
  , ("~="      , NotEq)
  , ("casePair", CasePair)
  , ("caseList", CaseList)
  , ("abort"   , Abort)
  ]
