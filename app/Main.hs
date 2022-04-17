module Main
  ( main
  ) where

import           Config
import           Driver
import           IOUtils

import           Options.Applicative

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
    compileEvalPrint config sources
  getFileContents "-" = readStdin
  getFileContents s   = readFile s
  -- If no files specified, read from stdin; same as
  -- calling `flc -`
  addDefault [] = ["-"]
  addDefault f  = f
