module Parser
  ( syntax
  , parse
  ) where

import           Language
import           Lexer
import           Parser.Core

-- Syntactical analysis driver. Selects the first complete parse (if any),
-- otherwise give the very unuseful error message: "Syntax error"
syntax :: [Token] -> CoreProgram
syntax = take_first_parse . pProgram
 where
  take_first_parse ((prog, []) : _     ) = prog
  take_first_parse (_          : others) = take_first_parse others
  take_first_parse _                     = error "Syntax error"

-- Full parser driver (lexer + parser)
parse :: String -> CoreProgram
parse = syntax . clex
