module AllocTests
  ( tests
  ) where

import           Alloc
import           Test.HUnit

tests :: Test
tests = test
  [ "size of heap(\"hello\",\"world\")"
  ~:  (let (h1, _) = hAlloc hInitial "hello"
       in  let (h2, _) = hAlloc h1 "world" in hSize h2
      )
  ~=? 2
  , "hIsnull hNull" ~: hIsnull hNull @? "hIsnull hNull should be true"
  , "hIsnull non-null"
  ~: not (let (_, addr) = hAlloc hInitial (3 :: Integer) in hIsnull addr)
  @? "hIsnull is not null on a alloced address"
  ]
