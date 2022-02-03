module ParserTests
  ( tests
  ) where

import           CorePrelude
import           Parser
import           PrettyPrint
import           Test.HUnit

tests :: Test
tests =
  test [ -- parse, syntax
        "parse prelude" ~: preludeDefs ~=? (parse . pprint) preludeDefs]
