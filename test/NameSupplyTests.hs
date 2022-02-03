module NameSupplyTests
  ( tests
  ) where

import           NameSupply
import           Test.HUnit

_ns1 :: NameSupply
name1, name2, renamed1, renamed2 :: String
renameds :: [String]

name1 = "var1"
name2 = "sometohervar"

(_ns1, renamed1) = getName initialNameSupply name1
(_, renamed2) = getName _ns1 name2

(_, renameds) = getNames initialNameSupply [name1, name2]

tests :: Test
tests = test
  [ -- getName
    "getName " ++ name1 ~: "var1_0" ~=? renamed1
  , "getName " ++ name2 ~: "sometohervar_1" ~=? renamed2
  , -- getNames
    "getNames ("
  ++  name1
  ++  ", "
  ++  name2
  ++  ")"
  ~:  ["var1_0", "sometohervar_1"]
  ~=? renameds
  ]
