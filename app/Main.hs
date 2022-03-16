module Main
  ( main
  ) where

import           Options.Applicative

import           Control.Exception
import           System.IO.Error

import           CorePrelude
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
    <$> many (argument str (metavar "FILES" <> help "source files"))
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
    sources <- mapM getFileContents $ addDefault $ inputFiles config
    performCompileAndPrint config sources
  getFileContents "-" = readStdin
  getFileContents s   = readFile s
  -- If no files specified, read from stdin; same as
  -- calling `flc -`
  addDefault [] = ["-"]
  addDefault f  = f

-- Read until EOF from stdin; allows repeatedly reading from
-- stdin, unlike `getContents`/other stdlib IO functions
-- Source: https://stackoverflow.com/a/56223271
readStdin :: IO String
readStdin = go [] where
  handler cs err | isEOFError err = return $ reverse cs
                 | otherwise      = throwIO err
  go cs = catch go' $ handler cs where go' = getChar >>= go . (: cs)

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
    , ("Prelude", pprint $ preludeDefs ++ extraPreludeDefs)
    , ("Pretty-printed program", pprint program)
    , ("Evaluation trace"      , showResults results)
    , ("Program output"        , result)
    ]
  (tokens, program, results, result) = performCompile fileContents
performCompileAndPrint _ fileContents = putStrLn result
  where (_, _, _, result) = performCompile fileContents

-- Helper to get outputs of compilation stages
-- Note: separately lexes and parses each files, and joins together
-- the scDefs into one program.
performCompile :: [String] -> ([Token], CoreProgram, [TiState], String)
performCompile fileContents = (concat tokens, program, results, result)
 where
  result  = showDataNode . getResult $ results
  results = eval . compile $ program
  program = concat asts
  asts    = syntax <$> tokens
  tokens  = clex <$> fileContents
