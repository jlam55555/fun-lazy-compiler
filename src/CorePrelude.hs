module CorePrelude
  ( preludeDefs
  , extraPreludeDefs
  ) where

import           Language

-- Standard Prelude for Core
-- This Haskell module is not called Prelude to avoid shadowing our Prelude)

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

-- Standard definitions for true/false
-- Mirrors definitions for nodes in Evaluators.TemplateInstantiation.Node
trueExpr, falseExpr :: CoreExpr
trueExpr = EConstr 1 0
falseExpr = EConstr 2 0

-- Introduced in section 2.3.4.
-- Execise 2.20: definitions for conditionals added.
extraPreludeDefs :: CoreProgram
extraPreludeDefs =
  [
  -- Standard definitions for true/false
    ("True" , [], trueExpr)
  , ("False", [], falseExpr)
  , ( "and"
    , ["x", "y"]
    , EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "y")) falseExpr
    )
  , ( "or"
    , ["x", "y"]
    , EAp (EAp (EAp (EVar "if") (EVar "x")) trueExpr) (EVar "y")
    )
  , ( "xor"
    , ["x", "y"]
    , EAp (EAp (EAp (EVar "if") (EVar "x")) (EAp (EVar "not") (EVar "y")))
          (EVar "y")
    )
  , ("not", ["x"], EAp (EAp (EAp (EVar "if") (EVar "x")) falseExpr) trueExpr)
  ]
