module Parser.SubparserTests
  ( tests
  ) where

import           Test.HUnit

tests :: Test
tests = test ["helloworld" ~: (2 :: Integer) ~=? 4]
