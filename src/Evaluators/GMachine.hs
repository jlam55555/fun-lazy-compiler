module Evaluators.GMachine where

import           Evaluators.GMachine.Evaluator
import           Evaluators.GMachine.State
import           Language
import           Parser

runProg :: String -> String
runProg = showResults . eval . compile . parse

compile :: CoreProgram -> GmState
compile = undefined

showResults :: [GmState] -> String
showResults = undefined
