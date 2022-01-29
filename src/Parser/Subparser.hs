module Parser.Subparser
  ( Parser
  , ParseResult
  , pLit
  , pVar
  , pNum
  , pAlt
  , pThen
  , pThen3
  , pThen4
  , pZeroOrMore
  , pOneOrMore
  , pEmpty
  , pApply
  , pOneOrMoreWithSep
  ) where

import           Data.Char
import           Lexer

-- Parser returns a list of possible parses. Each parse
-- includes the parsed type and the remaining tokens
-- (assuming that the toplevel is a list of parsed items).
type ParseResult a = [(a, [Token])]
type Parser a = [Token] -> ParseResult a

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
pAlt p1 p2 toks = p1 toks ++ p2 toks

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
