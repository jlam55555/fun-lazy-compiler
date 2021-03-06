module CorePrelude
  ( preludeDefs
  , extraPreludeDefs
  , extraPreludeDefsGM
  ) where

import           Evaluators.TemplateInstantiation.Node
import           Language
import           Parser

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

-- Extra prelude defs (for TI)
-- Introduced in section 2.3.4.
-- Execise 2.20: Add definitions for conditionals.
-- Exercise 2.22: Add definitions for pair.
extraPreludeDefs :: CoreProgram
extraPreludeDefs =
  [
  -- Standard definitions for true/false
    ("True" , [], EConstr tagTrue 0)
  , ("False", [], EConstr tagFalse 0)
  -- Conditionals
  , ( "and"
    , ["x", "y"]
    , EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "y")) (EVar "False")
    )
  , ( "or"
    , ["x", "y"]
    , EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "True")) (EVar "y")
    )
  , ( "xor"
    , ["x", "y"]
    , EAp (EAp (EAp (EVar "if") (EVar "x")) (EAp (EVar "not") (EVar "y")))
          (EVar "y")
    )
  , ( "not"
    , ["x"]
    , EAp (EAp (EAp (EVar "if") (EVar "x")) (EVar "False")) (EVar "True")
    )
  , ("isZero", ["x"], EAp (EAp (EVar "==") (EVar "x")) (ENum 0))
  -- Pairs
  , ("Pair"  , []   , EConstr tagCons 2)
  , ("fst", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K"))
  , ("snd", ["p"], EAp (EAp (EVar "casePair") (EVar "p")) (EVar "K1"))
  -- Lists
  , ("Nil"   , []   , EConstr tagNil 0)
  , ("Cons"  , []   , EConstr tagCons 2)
  , ( "head"
    , ["l"]
    , EAp (EAp (EAp (EVar "caseList") (EVar "l")) (EVar "abort")) (EVar "K")
    )
  , ( "tail"
    , ["l"]
    , EAp (EAp (EAp (EVar "caseList") (EVar "l")) (EVar "abort")) (EVar "K1")
    )
  , ( "length"
    , ["xs"]
    , EAp (EAp (EAp (EVar "caseList") (EVar "xs")) (ENum 0)) (EVar "length2")
    )
  , ( "length2"
    , ["x", "xs"]
    , EAp (EAp (EVar "+") (ENum 1)) (EAp (EVar "length") (EVar "xs"))
    )
  , ( "isNil"
    , ["l"]
    , EAp (EAp (EVar "==") (EAp (EVar "length") (EVar "l"))) (ENum 0)
    )
  ]

-- Extra prelude defs (for GM)
extraPreludeDefsGM :: CoreProgram
extraPreludeDefsGM = parse $ concat
  [ "if c t f = case c of <1> -> f ; <2> -> t ;"
  , "False = Pack{1,0} ;"
  , "True = Pack{2,0} ;"
  , "Nil = Pack{3,0} ;"
  , "Cons = Pack{4,2}"
  ]
