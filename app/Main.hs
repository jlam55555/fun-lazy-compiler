module Main
  ( main
  ) where

import           Options.Applicative

import           Evaluators.TemplateInstantiation.Evaluator
import           Evaluators.TemplateInstantiation.State
import           Language
import           Lexer
import           Parser
import           PrettyPrint

-- Definition for argument parsing
data FLCConfig = FLCConfig
  { inputFiles :: [String]
  , verbose    :: Bool
  }

flcConfig :: Parser FLCConfig
flcConfig =
  FLCConfig
    <$> some (argument str (metavar "FILES" <> help "source files"))
    <*> switch
          (long "verbose" <> short 'v' <> help
            "Print extra debugging information"
          )

-- Compiler entrypoint
main :: IO ()
main = getSourceFile =<< execParser opts
 where
  opts = info
    (flcConfig <**> helper)
    (  fullDesc
    <> progDesc "Compile and evaluate a program in the Core language"
    <> header
         "fun-lazy-compiler -- a compiler and runtime for the Core lazy functional language"
    )
  getSourceFile config = do
    sources <- mapM getFileContents $ inputFiles config
    performCompileAndPrint config sources
  getFileContents "-" = getContents
  getFileContents s   = readFile s

-- Driver for executing the compilation stages and printing
performCompileAndPrint :: FLCConfig -> [String] -> IO ()
performCompileAndPrint FLCConfig { verbose = True } fileContents = mapM_
  printOutput
  verboseOutputs
 where
  printOutput (label, text) = putStrLn $ "\n\n" ++ label ++ ":\n" ++ text
  verboseOutputs =
    [ ("Lexed tokens"          , show tokens)
    , ("Parsed AST"            , show program)
    , ("Pretty-printed program", pprint program)
    , ("Evaluation trace"      , showResults results)
    , ("Program output"        , show result)
    ]
  (tokens, program, results, result) = performCompile fileContents
performCompileAndPrint _ fileContents = putStrLn $ show result
  where (_, _, _, result) = performCompile fileContents

-- Helper to get outputs of compilation stages
-- Note: separately lexes and parses each files, and joins together
-- the scDefs into one program.
performCompile :: [String] -> ([Token], CoreProgram, [TiState], Int)
performCompile fileContents = (concat tokens, program, results, result)
 where
  result  = getResult results
  results = eval . compile $ program
  program = concat asts
  asts    = syntax <$> tokens
  tokens  = clex <$> fileContents
