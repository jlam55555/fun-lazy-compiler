module Language
  ( CoreProgram
  , CoreScDefn
  , Expr(..)
  , CoreExpr
  , CoreAlter
  , Name
  , IsRec
  , recursive
  , nonRecursive
  , bindersOf
  , rhssOf
  , isAtomicExpr
  ) where

-- Datatypes for Core syntax

-- Toplevel programs
type Program a = [ScDefn a]
type CoreProgram = Program Name

-- Supercombinators (top-level expression bindings)
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

-- Expressions are parameterized over their binders
data Expr a =
  EVar Name                           -- Variables
  | ENum Int                          -- Numbers
  | EConstr Int Int                   -- Constructor tag arity
  | EAp (Expr a) (Expr a)             -- Applications
  | ELet IsRec [(a, Expr a)] (Expr a) -- Let(rec) expressions
  | ECase (Expr a) [Alter a]          -- Case expression
  | ELam [a] (Expr a)                 -- Lambda abstractions
  deriving (Eq, Show)
type CoreExpr = Expr Name

-- Binder type
type Name = String

type IsRec = Bool
recursive, nonRecursive :: Bool
recursive = True
nonRecursive = False

-- Case rules/alternatives
type Alter a = (Int, [a], Expr a)
type CoreAlter = Alter Name

-- Useful helper functions

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [ name | (name, _) <- defns ]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [ rhs | (_, rhs) <- defns ]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _     ) = True
isAtomicExpr (ENum _     ) = True
isAtomicExpr (EConstr _ _) = True
isAtomicExpr _             = False
