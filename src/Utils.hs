module Utils
  ( NotImplementedError(..)
  , mapAccuml
  , space
  ) where

import           Control.Exception

-- NotImplementedError for incomplete programs
data NotImplementedError = NotImplementedError
  deriving Show

instance Exception NotImplementedError

-- Map/fold combination
mapAccuml :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccuml _ acc []       = (acc, [])
mapAccuml f acc (x : xs) = (acc2, x' : xs')
 where
  (acc1, x' ) = f acc x
  (acc2, xs') = mapAccuml f acc1 xs

-- Generate arbitrary-length strings of spaces
space :: Int -> String
space n = take n (repeat ' ')

-- Note: the following definitions in the tutorial's Utils module
-- were not reproduced here:
-- 
-- - The initial definitions, which were meant to make Gofer (Haskell)
--   code more Miranda-friendly. Since we are using Haskell, there is
--   no need for this.
-- - Set implementation. We can use the Data.Set module.
-- - A sort function. We can use the Data.Sort module.
-- - Assoclist implementation. We can use the Data.AssocList module
--   (from the hxt package).
-- - The Heap, Addr, and NameSupply classes were moved into separate
--   modules.
