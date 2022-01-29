module Exercises.SubparserTests
  ( pHelloOrGoodbye
  , pGreeting
  , pSample1
  , pSample2
  , pSample3
  , pSample4
  , pGreetings0
  , pGreetings1
  , pEmptyTest
  , pGreetingsLength
  , pProgram
  , pSample5
  , pSample6
  , pSample7
  , pSample8
  , pSample9
  , pKleeneTime
  ) where

import           Lexer
import           Parser.Subparser

-- Example from tutorial
-- TODO: move these examples to a separate place, and move the parser
--   functions to some internal module
pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pGreeting :: Parser (String, String)
pGreeting = pThen3 mk_pair pHelloOrGoodbye pVar $ pLit "!"
  where mk_pair hg name _ = (hg, name)

pSample1, pSample2, pSample3, pSample4 :: ParseResult (String, String)
pSample1 = pGreeting $ clex "hello world!"   -- success
pSample2 = pGreeting $ clex "goodbye world!" -- success
pSample3 = pGreeting $ clex "hi world"       -- failure
pSample4 = pGreeting $ clex "hello world"    -- failure

-- Testing Kleene*, Kleene+, epsilon parsers, pApply
pGreetings0, pGreetings1 :: Parser [(String, String)]
pGreetings0 = pZeroOrMore pGreeting
pGreetings1 = pOneOrMore pGreeting

pEmptyTest :: Parser String
pEmptyTest = pEmpty "hi"

pGreetingsLength :: Parser Int
pGreetingsLength = pApply pGreetings0 length

-- Testing pOneOrMoreWithSep: sample "program" with variables
-- separated by semicolons
pProgram :: Parser [String]
pProgram = pOneOrMoreWithSep pVar $ pLit ";"

-- Testing that pVar doesn't match keywords
pSample5, pSample6, pSample7 :: ParseResult String
pSample5 = pVar $ clex "letre" -- success
pSample6 = pVar $ clex "let"   -- failure
pSample7 = pVar $ clex "_let"  -- failure

-- Testing pNum
pSample8, pSample9 :: ParseResult Int
pSample8 = pNum $ clex "42"    -- success
pSample9 = pNum $ clex "hello" -- failure

-- Testing performance of greedy quantifiers
-- Generate "x " repeated n times
nxs :: Int -> String
nxs n = take (2 * n) xs where xs = 'x' : ' ' : xs

-- These should all return 1, since it greedily consumes the pattern
-- Before Exercise 1.19, this is quadratic. Afterwards, it is linear.
pKleeneTime :: Int -> Int
pKleeneTime n = length $ pOneOrMore (pLit "x") $ clex $ nxs n
