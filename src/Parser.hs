module Parser
  ( syntax
  , parse
  ) where

import           Language
import           Lexer

-- Syntactical analysis driver
syntax :: [Token] -> CoreProgram
syntax = undefined

-- Full parser driver (lexer + parser)
parse :: String -> CoreProgram
parse = syntax . clex
