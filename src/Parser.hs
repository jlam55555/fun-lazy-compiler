module Parser
  ( syntax
  , parse
  ) where

import           Data.Char
import           Language
import           Lexer

-- Parser returns a list of possible parses. Each parse
-- includes the parsed type and the remaining tokens
-- (assuming that the toplevel is a list of parsed items).
type ParseResult a = [(a, [Token])]
type Parser a = [Token] -> ParseResult a

-- Syntactical analysis driver
syntax :: [Token] -> CoreProgram
syntax = undefined

-- Full parser driver (lexer + parser)
parse :: String -> CoreProgram
parse = syntax . clex

-- Small parser for a grammar that only parses
-- tokens that match a literal. Examples:
-- Parses successfully:
-- pLit "Hello" $ clex "Hello world"
-- Doesn't parse:
-- pLit "hi" $ clex "Hello world"
-- Exercise 1.16: Write in terms of pSat
pLit :: String -> Parser String
pLit s = pSat $ (==) s

-- Parser for variables
-- Exercise 1.17: Don't parse keywords
pVar :: Parser String
pVar = pSat $ \tok -> (isAlpha . head $ tok) && (not $ elem tok keywords)
  where keywords = ["let", "letrec", "case", "in", "of", "Pack"]

-- Parser for number (integer) literals
pNum :: Parser Int
pNum = pApply pNumString read where pNumString = pSat $ isDigit . head

-- Combines all possible parses for multiple subparsers
-- on the same input. Corresponds to the vertical bar in a
-- (BNF) grammar.
-- Example:
-- pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")
pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

-- Combines two parsers into a new parser, which performs
-- sequential parses using the first parser followed by the
-- second parser, and combines their results using the
-- provided functions.
-- Example:
-- pGreeting = pThen mk_pair pHelloOrGoodbye pVar
--   where mk_pair hg name = (hg, name)
pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks =
  [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1 ]

-- Exercise 1.12: Extend pThen
pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks =
  [ (v1 v2, toks2)
  | (v1, toks1) <- pThen combine p1 p2 toks
  , (v2, toks2) <- p3 toks1
  ]

-- Exercise 1.12: Extend pThen
pThen4
  :: (a -> b -> c -> d -> e)
  -> Parser a
  -> Parser b
  -> Parser c
  -> Parser d
  -> Parser e
pThen4 combine p1 p2 p3 p4 toks =
  [ (v1 v2, toks2)
  | (v1, toks1) <- pThen3 combine p1 p2 p3 toks
  , (v2, toks2) <- p4 toks1
  ]

-- Exercise 1.13: Kleene star (repeat zero or more times)
-- Exercise 1.19: Make this greedy for performance reasons,
--   only return the first result. Turns quadratic -> linear
--   runtime for long repeated sequences in pZeroOrMore and
--   pOneOrMore.
pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (: []) . head . parses
  where parses = pOneOrMore p `pAlt` pEmpty []

-- Exercise 1.13: Kleene plus (repeat one or more times)
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p $ pZeroOrMore p

-- Exercise 1.13: Epsilon (always match)
pEmpty :: a -> Parser a
pEmpty matchVal toks = [(matchVal, toks)]

-- Exercise 1.14: Implement pApply (basically a functor)
pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [ (f v, toks1) | (v, toks1) <- p toks ]

-- Exercise 1.15: Implement pOneOrMoreWithSep
pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p pSep = pThen (:) p pTail
 where
  pTail     = pZeroOrMore pTailPart
  pTailPart = pThen (flip const) pSep p

-- Exercise 1.16: Generalization for pVar and pLit
pSat :: (String -> Bool) -> Parser String
pSat _ [] = []
pSat f ((_, tok) : toks) | f tok     = [(tok, toks)]
                         | otherwise = []

-- Example from tutorial
-- TODO: move these examples to a separate place, and move the parser
--   functions to some internal module
_pHelloOrGoodbye :: Parser String
_pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

_pGreeting :: Parser (String, String)
_pGreeting = pThen3 mk_pair _pHelloOrGoodbye pVar $ pLit "!"
  where mk_pair hg name _ = (hg, name)

_pSample1, _pSample2, _pSample3, _pSample4 :: ParseResult (String, String)
_pSample1 = _pGreeting $ clex "hello world!"   -- success
_pSample2 = _pGreeting $ clex "goodbye world!" -- success
_pSample3 = _pGreeting $ clex "hi world"       -- failure
_pSample4 = _pGreeting $ clex "hello world"    -- failure

-- Testing Kleene*, Kleene+, epsilon parsers, pApply
_pGreetings0, _pGreetings1 :: Parser [(String, String)]
_pGreetings0 = pZeroOrMore _pGreeting
_pGreetings1 = pOneOrMore _pGreeting

_pEmptyTest :: Parser String
_pEmptyTest = pEmpty "hi"

_pGreetingsLength :: Parser Int
_pGreetingsLength = pApply _pGreetings0 length

-- Testing pOneOrMoreWithSep: sample "program" with variables
-- separated by semicolons
_pProgram :: Parser [String]
_pProgram = pOneOrMoreWithSep pVar $ pLit ";"

-- Testing that pVar doesn't match keywords
_pSample5, _pSample6, _pSample7 :: ParseResult String
_pSample5 = pVar $ clex "letre" -- success
_pSample6 = pVar $ clex "let"   -- failure
_pSample7 = pVar $ clex "_let"  -- failure

-- Testing pNum
_pSample8, _pSample9 :: ParseResult Int
_pSample8 = pNum $ clex "42"    -- success
_pSample9 = pNum $ clex "hello" -- failure

-- Testing performance of greedy quantifiers
-- Generate "x " repeated n times
_nxs :: Int -> String
_nxs n = take (2 * n) xs where xs = 'x' : ' ' : xs

-- These should all return 1, since it greedily consumes the pattern
-- Before Exercise 1.19, this is quadratic. Afterwards, it is linear.
_pKleeneTime :: Int -> Int
_pKleeneTime n = length $ pOneOrMore (pLit "x") $ clex $ _nxs n
