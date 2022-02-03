module Parser.CoreTests
  ( tests
  ) where

import           Test.HUnit

tests :: Test
tests = test ["helloworld" ~: (2 :: Integer) ~=? 4]
