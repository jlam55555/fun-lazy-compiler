module UtilsTests
  ( tests
  ) where

import           Test.HUnit
import           Utils

tests :: Test
tests = test
  [ -- mapAccuml
    "mapAccuml" ~: (15 :: Integer, [2, 4 .. 10]) ~=? mapAccuml
    (\acc val -> (acc + val, val * 2))
    0
    [1 .. 5]
  , -- space
    "space 0" ~: "" ~=? space 0
  , "space -1" ~: "" ~=? space (-1)
  , "space 1" ~: " " ~=? space 1
  , "space 10" ~: "          " ~=? space 10
  ]
