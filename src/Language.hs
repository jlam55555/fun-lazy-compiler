module Language where
import Utils

data Expr a =
  EVar Name -- Variables
  | ENum Int  -- Numbers
  | EConstr Int Int -- Constructor tag arity
  | EAp (Expr a) (Expr a) -- Applications
  | ELet IsRec [(a, Expr a)] (Expr a) -- Let(rec) expressions
  | ECase (Expr a) [Alter a] -- Case expression
  | ELam [a] (Expr a) -- Lambda abstractions

-- Placeholders so that this compiles for now
type Name = ()
type IsRec = ()
type Alter a = ()
