module Main
  ( main
  ) where

import           Options.Applicative
import           System.IO

data FLCConfig = FLCConfig String

flcConfig :: Parser FLCConfig
flcConfig = FLCConfig <$> argument str (metavar "FILE")

main :: IO ()
main = main' =<< execParser opts
 where
  opts = info
    (flcConfig <**> helper)
    (  fullDesc
    <> progDesc "Compile and evaluate a program in the Core language"
    <> header
         "fun-lazy-compiler -- a compiler and runtime for the Core lazy functional language"
    )

main' :: FLCConfig -> IO ()
main' (FLCConfig file) = do
  sourceFileContents <- readSourceFile file
  putStrLn $ "Got file contents: " ++ sourceFileContents

-- Read a source file, or stdin if `-` is specified
readSourceFile :: String -> IO String
readSourceFile "-" = hGetContents stdin
readSourceFile s   = readFile s
