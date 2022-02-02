-- In haskell-interactive-mode, use M-x haskell-session-change-target
-- to switch to the flc-test target
module Main
  ( main
  ) where

import           AllocTests
import           Test.HUnit

main :: IO Counts
main = runTestTT $ test ["Alloc" ~: AllocTests.tests]
