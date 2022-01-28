module Lexer
  ( Token
  , clex
  ) where

import           Data.Char

-- Note: a token is never empty
-- Exercise 1.11: Token also records line number of a token
type Token = (Int, String)

-- Lexical analysis driver
clex :: String -> [Token]
clex = clex' 0

-- First parameter is the line number
-- Exercise 1.9: Allow for (Haskell-style) comments
-- Exercise 1.10: Lex two-character operators
-- Exercise 1.11: Token also records line number of a token
clex' :: Int -> String -> [Token]
clex' _  ""               = []
clex' ln ('-' : '-' : cs) = -- Haskell comment syntax
                            clex' ln cs_skip_line
  where cs_skip_line = dropWhile (not . (==) '\n') cs
clex' ln (c : cs) | -- Newline needs to advance line num
                    c == '\n'   = clex' (ln + 1) cs
                  | -- Throw away whitespace
                    isSpace c   = clex' ln cs
                  | -- Recognize numbers as a single token
                    isDigit c   = (ln, num_token) : clex' ln num_rest_cs
                  | -- Recognize variables, which begin with alphabetic
                    -- letter and continue with letters, digits, and _
                    isAlpha c   = (ln, var_tok) : clex' ln var_rest_cs
                  | -- Recognize two-character operators
                    isTwoCharOp = (ln, cTwoCharOp) : (clex' ln $ tail cs)
                  | -- If no pattern applies, a token containing the
                    -- character is returned.
                    otherwise   = (ln, [c]) : clex' ln cs
 where
  num_token   = c : takeWhile isDigit cs
  num_rest_cs = dropWhile isDigit cs
  var_tok     = c : takeWhile isIdChar cs
  var_rest_cs = dropWhile isIdChar cs
  isIdChar c' = isAlpha c' || isDigit c' || c == '_'
  isTwoCharOp = cs /= [] && elem cTwoCharOp twoCharOps
  cTwoCharOp  = c : head cs : []
  twoCharOps  = ["==", "~=", ">=", "<=", "->"]
