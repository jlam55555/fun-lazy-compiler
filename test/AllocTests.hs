module AllocTests
  ( tests
  ) where

import           Alloc
import           Control.Exception
import           Test.HUnit
import           TestUtil

-- Heap test case
h :: Heap String
addr1, addr2 :: Addr
(h, addr1, addr2) =
  let (h1, addr1') = hAlloc hInitial "hello"
  in  let (h2, addr2') = hAlloc h1 "world" in (h2, addr1', addr2')

tests :: Test
tests = test
  [-- hSize
    "size of heap(\"hello\",\"world\")" ~: hSize h ~=? 2
  , -- hIsnull
    "hIsnull hNull" ~: hIsnull hNull @? "hIsnull hNull should be true"
  , "hIsnull non-null"
  ~: not (hIsnull addr1)
  @? "hIsnull is not null on a alloced address"
  , -- error on bad lookup
    "hLookup fail" ~: assertError "lookup on empty heap" $ evaluate $ hLookup
    hInitial
    hNull
  , -- good lookup
    "hLookup success" ~: "hello" ~=? hLookup h addr1
  , "hLookup success" ~: "world" ~=? hLookup h addr2
  ]
