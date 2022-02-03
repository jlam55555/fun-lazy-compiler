module LexerTests
  ( tests
  ) where

import           Lexer
import           Test.HUnit

tests :: Test
tests = test
  [
    -- empty
    "empty string" ~: [] ~=? clex ""
  , -- comments
    "ignore comments" ~: [(1, "a"), (1, "b"), (1, "c"), (3, "hello")] ~=? clex
    "-- Hello, world!\na b c\n-- another comment \nhello"
  , -- newlines
    "newlines advance lineno" ~: [(1, "a"), (2, "b"), (3, "c")] ~=? clex
    "\na\nb\nc"
  , -- numbers
    "numbers" ~: [(0, "1254"), (0, "31415")] ~=? clex "1254 31415"
  , -- identifiers (variables)
    "identifiers" ~: [(0, "a_bc"), (0, "c0123_54c")] ~=? clex " a_bc c0123_54c"
  , -- recognize two character operators
    "ops" ~: [(0, "=="), (0, "~="), (0, ">="), (0, "<="), (0, "->")] ~=? clex
    "== ~= >= <= ->"
  , -- recognize other tokens as single-character tokens
    "other"
  ~:  [ (0, "~")
      , (0, "!")
      , (0, "@")
      , (0, "#")
      , (0, "$")
      , (0, "%")
      , (0, "^")
      , (0, "&")
      , (0, "*")
      , (0, "(")
      , (0, ")")
      , (0, "-")
      , (0, "_")
      , (0, "=")
      , (0, "+")
      , (0, "[")
      , (0, "]")
      , (0, "->")
      ]
  ~=? clex "~!@#$%^&*()-_=+[]->"
  ]
