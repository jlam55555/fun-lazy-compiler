{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Driver
  ( compileEvalPrint
  ) where

import           Config

import           CorePrelude
import           Evaluator
import           Lexer
import           Parser
import           PrettyPrint

-- Driver for executing the compilation stages and printing
compileEvalPrint :: FLCConfig -> [String] -> IO ()
compileEvalPrint FLCConfig { verbose, evaluator, compileOnly } fileContents
  | compileOnly = putStrLn compiled
  | verbose     = mapM_ printOutput verboseOutputs
  | otherwise   = putStrLn output
 where
  printOutput (label, text) = putStrLn $ "\n\n" ++ label ++ ":\n" ++ text
  verboseOutputs =
    [ ("Lexed tokens"          , show tokens)
    , ("Parsed AST"            , show program)
    , ("Prelude", pprint $ preludeDefs ++ extraPreludeDefs)
    , ("Pretty-printed program", pprint program)
    , ("Evaluation trace"      , trace)
    , ("Program output"        , output)
    ]

  tokens     = clex <$> fileContents
  asts       = syntax <$> tokens
  program    = concat asts

  -- Somewhat redundant due to difficulty in making type applications polymorphic
  compiledGm = compile @GmState $ program
  compiledTi = compile @TiState $ program

  compiled   = compiled' evaluator
  compiled' EvaluatorGM = showCompiled compiledGm
  compiled' EvaluatorTI = showCompiled compiledTi

  traceGm = eval compiledGm
  traceTi = eval compiledTi

  trace   = trace' evaluator
  trace' EvaluatorGM = showTrace traceGm
  trace' EvaluatorTI = showTrace traceTi

  output = output' evaluator
  output' EvaluatorGM = showOutput traceGm
  output' EvaluatorTI = showOutput traceTi
