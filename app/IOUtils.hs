module IOUtils
  ( readStdin
  ) where

import           Control.Exception
import           System.IO.Error

-- Read until EOF from stdin; allows repeatedly reading from
-- stdin, unlike `getContents`/other stdlib IO functions
-- Source: https://stackoverflow.com/a/56223271
readStdin :: IO String
readStdin = go [] where
  handler cs err | isEOFError err = return $ reverse cs
                 | otherwise      = throwIO err
  go cs = catch go' $ handler cs where go' = getChar >>= go . (: cs)
