module Evaluators.GMachine
  ( GmState
  , runProg
  , eval
  , compile
  , showTrace
  , showOutput
  , exportAsm
  ) where

import           Evaluators.GMachine.Compiler
import           Evaluators.GMachine.DumpAsm
import           Evaluators.GMachine.Evaluator
import           Evaluators.GMachine.PrintUtils
import           Evaluators.GMachine.State

import           Parser

runProg :: String -> String
runProg = showTrace . eval . compile . parse
