module Parser.CoreTests
  ( tests
  ) where

import           Language
import           Lexer
import           Parser.Core
import           Test.HUnit

tests :: Test
tests = test
  [ -- pProgram, pSc will be tested in ParserTests

    -- pAtom
    "pAtom var" ~: [(EVar "x", [])] ~=? (pAtom $ clex "x")
  , "pAtom num" ~: [(ENum 523, [])] ~=? (pAtom $ clex "523")
  , "pAtom pack" ~: [(EConstr 5 0, [])] ~=? (pAtom $ clex "Pack{ 5   ,0  }")
  , "pAtom wrap" ~: [(EAp (EVar "x") (ENum 2), [])] ~=? (pAtom $ clex "(x 2)")
  , "pAtom non-atom 1" ~: [] ~=? (pAtom $ clex "case 2 of <1> -> 3")
  , "pAtom non-atom 2" ~: [] ~=? (pAtom $ clex "let x = 2 in x + 3")
  , "pAtom non-atom 3" ~: [] ~=? (pAtom $ clex "\\ x . x")
  , -- pCase
    "pCase"
  ~:  [ ( ECase (EAp (EAp (EConstr 5 2) (ENum 4)) (ENum 5))
                [(5, ["x", "y"], (EVar "x"))]
        , []
        )
      ]
  ~=? (pCase $ clex "case (Pack{5,2} 4 5) of <5> x y -> x")

    -- pLam
  , "pLam"
  ~:  [(ELam ["x", "y"] (EAp (EVar "y") (EVar "x")), [])]
  ~=? (pLam $ clex "\\ x y . y x")

    -- pAp
  , "pLam"
  ~:  [(EAp (EAp (EVar "x") (ENum 5)) (EVar "y"), [])]
  ~=? (pAp $ clex "x 5 y")

    -- pLet
  , "pLet"
  ~:  [(ELet recursive [("y", ENum 2)] (EVar "y"), [])]
  ~=? (pLet $ clex "letrec y = 2 in y")

    -- infix operator precedence levels
  , "infix op asc right 1"
  ~:  [ ( EAp (EAp (EVar "+") (ENum 2)) (EAp (EAp (EVar "+") (ENum 3)) (ENum 4))
        , []
        )
      , (EAp (EAp (EVar "+") (ENum 2)) (ENum 3), [(0, "+"), (0, "4")])
      , (ENum 2, [(0, "+"), (0, "3"), (0, "+"), (0, "4")])
      ]
  ~=? (pExpr $ clex "2 + 3 + 4")
  , "infix op asc right 2"
  ~:  [ ( EAp (EAp (EVar "-") (ENum 2)) (EAp (EAp (EVar "-") (ENum 3)) (ENum 4))
        , []
        )
      , (EAp (EAp (EVar "-") (ENum 2)) (ENum 3), [(0, "-"), (0, "4")])
      , (ENum 2, [(0, "-"), (0, "3"), (0, "-"), (0, "4")])
      ]
  ~=? (pExpr $ clex "2 - 3 - 4")
  , "infix op asc left"
  ~:  [ ( EAp (EAp (EVar "-") (EAp (EAp (EVar "-") (ENum 2)) (ENum 3))) (ENum 4)
        , []
        )
      , (EAp (EAp (EVar "-") (ENum 2)) (ENum 3), [(0, "-"), (0, "4")])
      ]
  ~=? (pExpr $ clex "(2 - 3) - 4")
  ]
