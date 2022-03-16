module Evaluators.TemplateInstantiation
  ( eval
  , compile
  , run
  , showTrace
  , runShowTrace
  , getOutput
  , runGetOutput
  , runGetNumOutput
  , runGetBoolOutput
  , showOutput
  , runShowOutput
  ) where

import           Alloc
import           Parser

import           Evaluators.TemplateInstantiation.Evaluator
import           Evaluators.TemplateInstantiation.Node
import           Evaluators.TemplateInstantiation.PrintUtils
import           Evaluators.TemplateInstantiation.State

-- Driver functions for the template instantiation evaluator.

-- Simply run program, and return raw results from `eval`
run :: String -> [TiState]
run = eval . compile . parse

-- Driver for the graph reduction implementation;
-- runs program and shows result
runShowTrace :: String -> String
runShowTrace = showTrace . run

-- Get final (data) node from evaluation result
getOutput :: [TiState] -> Node
getOutput states = hLookup h s
  where -- Non-exhaustive pattern should not fail because `eval` should always
        -- return a singleton stack (the single element containing the result)
        ([s], _, h, _, _) = last states

-- Get result of running program
runGetOutput :: String -> Node
runGetOutput = getOutput . run

-- Get numeric result; useful for tests
runGetNumOutput :: String -> Int
runGetNumOutput = toNum . runGetOutput
 where
  toNum (NNum n) = n
  toNum _        = error "runGetNumOutput: not a numeric result"

-- Get boolean result; useful for tests
runGetBoolOutput :: String -> Bool
runGetBoolOutput = toNum . runGetOutput
 where
  toNum b | b == trueNode  = True
          | b == falseNode = False
          | otherwise      = error "runGetBoolOutput: not a boolean result"

showOutput :: [TiState] -> String
showOutput trace = showOutputNode h output
 where
  (_, _, h, _, _) = last trace
  output          = getOutput trace

runShowOutput :: String -> String
runShowOutput = showOutput . run
