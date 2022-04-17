module Config
  ( FLCConfig(..)
  , EvaluatorType(..)
  , flcConfig
  ) where

import           Options.Applicative

-- Indicate type of evaluator (Template Instantiation or G-Machine)
data EvaluatorType = EvaluatorTI | EvaluatorGM

-- Definition for argument parsing
data FLCConfig = FLCConfig
  { inputFiles  :: [String]
  , evaluator   :: EvaluatorType
  , compileOnly :: Bool
  , verbose     :: Bool
  }

-- https://stackoverflow.com/a/46183087
evaluatorType :: ReadM EvaluatorType
evaluatorType = str >>= \s -> case s of
  "ti" -> return EvaluatorTI
  "gm" -> return EvaluatorGM
  _    -> readerError "Accepted evaluators are 'ti' and 'gm'."

flcConfig :: Parser FLCConfig
flcConfig =
  FLCConfig
    <$> many (argument str (metavar "FILES" <> help "Source file(s)"))
    <*> option
          evaluatorType
          (  long "evaluator"
          <> short 'e'
          <> help "Evaluator type (ti, gm)"
          <> value EvaluatorGM
          )
    <*> switch
          (long "compile" <> short 'c' <> help
            "Export compiled program (gm only)"
          )
    <*> switch
          (long "verbose" <> short 'v' <> help
            "Print extra debugging information"
          )
