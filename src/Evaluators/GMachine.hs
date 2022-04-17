module Evaluators.GMachine where

import           Evaluators.GMachine.Compiler
import           Evaluators.GMachine.Evaluator
import           Evaluators.GMachine.State

import           Language
import           Parser

runProg :: String -> String
runProg = showResults . eval . compile . parse

compile :: CoreProgram -> GmState
compile prog = GmState initialCode [] h e statInitial
  where (h, e) = buildInitialHeap prog

showResults :: [GmState] -> String
showResults = undefined
