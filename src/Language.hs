module Language where
import           Utils

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
type CoreExpr = Expr Name

-- Binder type
type Name = String

type IsRec = Bool
recursive, nonRecursive :: Bool
recursive = True
nonRecursive = False

-- Case rules/alternatives
type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

-- Useful helper functions

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [ name | (name, _) <- defns ]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [ rhs | (_, rhs) <- defns ]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

-- Standard Prelude
-- I x = x ;
-- K x y = x ;
-- K1 x y = y ;
-- S f g x = f x (g x) ;
-- compose f g x = f (g x) ;
-- twice f = compose f f ;

preludeDefs :: CoreProgram
preludeDefs =
  [ ("I" , ["x"]     , EVar "x")
  , ("K" , ["x", "y"], EVar "x")
  , ("K1", ["x", "y"], EVar "y")
  , ( "S"
    , ["f", "g", "x"]
    , EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))
    )
  , ("compose", ["f", "g", "x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x")))
  , ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))
  ]
