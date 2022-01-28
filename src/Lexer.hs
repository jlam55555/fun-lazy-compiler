module Lexer
  ( Token
  , clex
  ) where

import           Data.Char

-- Note: a token is never empty
type Token = String

-- Lexical analysis driver
clex :: String -> [Token]
clex "" = []
clex (c : cs) | -- Throw away whitespace
                isSpace c = clex cs
              | -- Recognize numbers as a single token
                isDigit c = num_token : clex num_rest_cs
              | -- Recognize variables, which begin with alphabetic
                -- letter and continue with letters, digits, and _
                isAlpha c = var_tok : clex var_rest_cs
              | -- If no pattern applies, a token containing the
                -- character is returned.
                otherwise = [c] : clex cs
 where
  num_token   = c : takeWhile isDigit cs
  num_rest_cs = dropWhile isDigit cs
  var_tok     = c : takeWhile isIdChar cs
  var_rest_cs = dropWhile isIdChar cs
  isIdChar c' = isAlpha c' || isDigit c' || c' == '_'
