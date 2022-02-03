module PrettyPrintTests
  ( tests
  ) where

import           CorePrelude
import           Iseq
import           Language
import           PrettyPrint
import           Test.HUnit

tests :: Test
tests = test
  [ -- pprint/pprProgram/pprScDefn
    "pprint preludeDefs"
  ~:  "I x =\n  x ;\n"
  ++  "K x y =\n  x ;\n"
  ++  "K1 x y =\n  y ;\n"
  ++  "S f g x =\n  f x ( g x ) ;\n"
  ++  "compose f g x =\n  f ( g x ) ;\n"
  ++  "twice f =\n  compose f f"
  ~=? pprint preludeDefs
  , -- pprExpr
    "pprExpr ENum" ~: "5" ~=? (iDisplay $ pprExpr $ ENum 5)
  , "pprExpr EVar" ~: "hello" ~=? (iDisplay $ pprExpr $ EVar "hello")
  , "pprExpr EAp" ~: "f g" ~=? (iDisplay $ pprExpr $ EAp (EVar "f") (EVar "g"))
  , "pprExpr ELet"
  ~:  "let x = 5 ;\n    y = 4\nin x + y"
  ~=? (iDisplay $ pprExpr $ ELet nonRecursive
                                 [("x", ENum 5), ("y", ENum 4)]
                                 (EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))
      )
  , "pprExpr case"
  ~:  "case Pack{ 5, 2 } of\n  <1> x y z -> x y z ;\n  <5> z abc -> abc - z"
  ~=? (iDisplay $ pprExpr $ ECase
        (EConstr 5 2)
        [ (1, ["x", "y", "z"], EAp (EAp (EVar "x") (EVar "y")) (EVar "z"))
        , (5, ["z", "abc"]   , EAp (EAp (EVar "-") (EVar "abc")) (EVar "z"))
        ]
      )
  , "pprExpr pack" ~: "Pack{ 0, 3 }" ~=? (iDisplay $ pprExpr $ EConstr 0 3)
  , "pprExpr lam"
  ~:  "\\ x y . y x"
  ~=? (iDisplay $ pprExpr $ ELam ["x", "y"] (EAp (EVar "y") (EVar "x")))
  , -- infix operators, optional parens (operator precedence and associativity)
    "hi" ~: (2 :: Integer) ~=? 2
  , "hi"
  ~:  "x + y < p * length xs"
  ~=? (iDisplay $ pprExpr $ EAp
        (EAp (EVar "<") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y")))
        (EAp (EAp (EVar "*") (EVar "p")) (EAp (EVar "length") (EVar "xs")))
      )
  , "hi"
  ~:  "( x + y ) * p * length xs"
  ~=? (iDisplay $ pprExpr $ EAp
        (EAp (EVar "*") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y")))
        (EAp (EAp (EVar "*") (EVar "p")) (EAp (EVar "length") (EVar "xs")))
      )
  , "hi"
  ~:  "( x * y ) * p * length xs"
  ~=? (iDisplay $ pprExpr $ EAp
        (EAp (EVar "*") (EAp (EAp (EVar "*") (EVar "x")) (EVar "y")))
        (EAp (EAp (EVar "*") (EVar "p")) (EAp (EVar "length") (EVar "xs")))
      )
  , "hi"
  ~:  "f ( x + y ) ( p * length xs )"
  ~=? (iDisplay $ pprExpr $ EAp
        (EAp (EVar "f") (EAp (EAp (EVar "+") (EVar "x")) (EVar "y")))
        (EAp (EAp (EVar "*") (EVar "p")) (EAp (EVar "length") (EVar "xs")))
      )
  , "hi"
  ~:  "h ( g ( f x ) )"
  ~=? (iDisplay $ pprExpr $ EAp (EVar "h")
                                (EAp (EVar "g") (EAp (EVar "f") (EVar "x")))
      )
  , "hi"
  ~:  "h g f x"
  ~=? (iDisplay $ pprExpr $ EAp (EAp (EAp (EVar "h") (EVar "g")) (EVar "f"))
                                (EVar "x")
      )
  , "hi"
  ~:  "h g ( f x )"
  ~=? (iDisplay $ pprExpr $ EAp (EAp (EVar "h") (EVar "g"))
                                (EAp (EVar "f") (EVar "x"))
      )
  , "hi"
  ~:  "( 1 - 2 ) - 3"
  ~=? (iDisplay $ pprExpr $ EAp
        (EAp (EVar "-") (EAp (EAp (EVar "-") (ENum 1)) (ENum 2)))
        (ENum 3)
      )
  , "hi"
  ~:  "1 - ( 2 - 3 )"
  ~=? (iDisplay $ pprExpr $ EAp (EAp (EVar "-") (ENum 1))
                                (EAp (EAp (EVar "-") (ENum 2)) (ENum 3))
      )
  , "hi"
  ~:  "1 + 2 + 3"
  ~=? (iDisplay $ pprExpr $ EAp (EAp (EVar "+") (ENum 1))
                                (EAp (EAp (EVar "+") (ENum 2)) (ENum 3))
      )
  , "hi"
  ~:  "( 1 + 2 ) + 3"
  ~=? (iDisplay $ pprExpr $ EAp
        (EAp (EVar "+") (EAp (EAp (EVar "+") (ENum 1)) (ENum 2)))
        (ENum 3)
      )
  , "hi"
  ~:  "( ( * ) + ( + ) ( + ) ) ( + )"
  ~=? (iDisplay $ pprExpr $ EAp
        (EAp (EAp (EVar "+") (EVar "*")) (EAp (EVar "+") (EVar "+")))
        (EVar "+")
      )
  -- , "hi" ~: "" ~=? (iDisplay $ pprExpr $)
  -- , "hi" ~: "" ~=? (iDisplay $ pprExpr $)
  -- , "hi" ~: "" ~=? (iDisplay $ pprExpr $)
  ]
