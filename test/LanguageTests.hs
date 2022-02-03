module LanguageTests
  ( tests
  ) where

import           Language
import           Test.HUnit

bindings :: [(Name, CoreExpr)]
bindings = [("x", ENum 5), ("y", EVar "x")]

tests :: Test
tests = test
  [ -- bindersOf
    "bindersOf" ~: ["x", "y"] ~=? bindersOf bindings
  , -- rhssOf
    "rhssOf" ~: [ENum 5, EVar "x"] ~=? rhssOf bindings
  , -- isAtomicExpr
    "isAtomicExpr var" ~: isAtomicExpr (EVar "x") @? "var is atomic"
  , "isAtomicExpr num" ~: isAtomicExpr (ENum 5) @? "num is atomic"
  , "isAtomicExpr pack" ~: isAtomicExpr (EConstr 0 5) @? "pack is atomic"
  , "isAtomicExpr ap"
  ~: not (isAtomicExpr $ EAp (ENum 0) (ENum 1))
  @? "ap is not atomic"
  , "isAtomicExpr let"
  ~: not (isAtomicExpr $ ELet nonRecursive [("x", (ENum 0))] (EVar "x"))
  @? "let is not atomic"
  , "isAtomicExpr case"
  ~: not (isAtomicExpr $ ECase (ENum 5) [(0, [], (ENum 0))])
  @? "case is not atomic"
  , "isAtomicExpr lam"
  ~: not (isAtomicExpr $ ELam [] (ENum 0))
  @? "lam is not atomic"
  ]
