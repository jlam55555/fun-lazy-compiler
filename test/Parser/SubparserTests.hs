module Parser.SubparserTests
  ( tests
  ) where

import           Lexer
import           Parser.Subparser
import           Test.HUnit

tests :: Test
tests = test
  [ -- pLit
    "pLit on non-reserved identifier match"
  ~:  [("hello", [])]
  ~=? (pLit "hello" $ clex "hello")
  , "pLit on non-reserved identifier fail" ~: [] ~=? (pLit "hello" $ clex "hi")
  , -- pVar
    "pVar on non-reserved identifier"
  ~:  [("letre", [])]
  ~=? (pVar $ clex "letre")
  , "pVar on reserved identifier" ~: [] ~=? (pVar $ clex "letrec")
  , "pVar on non-identifier" ~: [] ~=? (pVar $ clex "523")
  , -- pNum
    "pNum on number" ~: [(523, [])] ~=? (pNum $ clex "523")
  , "pNum on non-number" ~: [] ~=? (pNum $ clex "hello")
  , -- pAlt
    "pAlt multiple matching parses"
  ~:  [("hi", [(0, "world")]), ("hi world", [])]
  ~=? (pAlt pVar (pThen (\x y -> x ++ " " ++ y) pVar pVar) $ clex "hi world")
  , -- pThen, pThen3, pThen4
    "pThen3"
  ~:  [("jon", [])]
  ~=? ( pThen3 (\_ name _ -> name) (pLit "hello") pVar (pLit "!")
      $ clex "hello jon!"
      )
  , -- pZeroOrMore, pOneOrMore
    "pZeroOrMore zero"
  ~:  [([], [(0, "abc")])]
  ~=? (pZeroOrMore pNum $ clex "abc")
  , "pZeroOrMore one"
  ~:  [([1], [(0, "abc")])]
  ~=? (pZeroOrMore pNum $ clex "1 abc")
  , "pZeroOrMore multiple"
  ~:  [([1, 2, 3], [(0, "abc")])]
  ~=? (pZeroOrMore pNum $ clex "1 2 3 abc")
  , "pOneOrMore zero" ~: [] ~=? (pOneOrMore pNum $ clex "abc")
  , "pOneOrMore one"
  ~:  [([1], [(0, "abc")])]
  ~=? (pOneOrMore pNum $ clex "1 abc")
  , "pOneOrMore multiple"
  ~:  [([1, 2, 3], [(0, "abc")])]
  ~=? (pOneOrMore pNum $ clex "1 2 3 abc")
  , -- pEmpty
    "pEmpty"
  ~:  [("hi", [(0, "hello"), (0, "world")])]
  ~=? (pEmpty "hi" $ clex "hello world")
  , -- pApply
    "pApply"
  ~:  [([5, 2, 3], [])]
  ~=? (pApply (pOneOrMore pNum) (map $ (+) 2) $ clex "3 0 1")
  , -- pOneOrMoreWithSep
    "pOneOrMoreWithSep"
  ~:  [([5, 2, 3], [])]
  ~=? (pOneOrMoreWithSep pNum (pLit ",") $ clex "5,2,3")
  ]
