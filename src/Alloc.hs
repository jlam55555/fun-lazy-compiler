module Alloc
  ( Heap
  , Addr
  , badAddr
  , hInitial
  , hAlloc
  , hUpdate
  , hFree
  , hLookup
  , hAddresses
  , hSize
  , hNull
  , hIsnull
  , hStatsGetAllocs
  , hStatsGetUpdates
  , hStatsGetFrees
  ) where

import           Data.AssocList

-- Exercise 2.7: Collect statistics about heap operations.
-- As the exercise points out, this only works for operations
-- that return an updated heap (i.e., not `hLookup`).
-- - `na`: # calls to `hAlloc`
-- - `nu`: # calls to `hUpdate`
-- - `nf`: # calls to `hFree`
type HeapStats = (Int, Int, Int)

hStatsInitial :: HeapStats
hStatsInitial = (0, 0, 0)

hStatsIncAlloc :: HeapStats -> HeapStats
hStatsIncAlloc (na, nu, nf) = (na + 1, nu, nf)

hStatsIncUpdate :: HeapStats -> HeapStats
hStatsIncUpdate (na, nu, nf) = (na, nu + 1, nf)

hStatsIncFree :: HeapStats -> HeapStats
hStatsIncFree (na, nu, nf) = (na, nu, nf + 1)

hStatsGetAllocs :: Heap a -> Int
hStatsGetAllocs (Heap (_, _, _, (na, _, _))) = na

hStatsGetUpdates :: Heap a -> Int
hStatsGetUpdates (Heap (_, _, _, (_, nu, _))) = nu

hStatsGetFrees :: Heap a -> Int
hStatsGetFrees (Heap (_, _, _, (_, _, nf))) = nf

-- A heap is the product of:
-- - Its current size
-- - An iterator of free addresses
-- - A mapping from addresses to heap nodes
-- - Heap statistics
newtype Heap a = Heap (Int, [Addr], [(Addr, a)], HeapStats)

instance Show (Heap a) where
  show h = "Heap of size " ++ show (hSize h)

newtype Addr = Addr Int
  deriving   Eq

instance Show Addr where
  show (Addr a) = "#" ++ show a

badAddr :: Addr
badAddr = Addr (-1)

hInitial :: Heap a
hInitial = Heap (0, Addr <$> [1 ..], [], hStatsInitial)

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap (_, [], _, _)) _ = error "hAlloc: free addresses list is empty"
hAlloc (Heap (size, (next : free), cts, hs)) n =
  (Heap (size + 1, free, (next, n) : cts, hStatsIncAlloc hs), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap (size, free, cts, hs)) a n =
  Heap (size, free, (a, n) : hRemove cts a, hStatsIncUpdate hs)

hFree :: Heap a -> Addr -> Heap a
hFree (Heap (size, free, cts, hs)) a =
  Heap (size - 1, a : free, hRemove cts a, hStatsIncFree hs)

hLookup :: Heap a -> Addr -> a
hLookup (Heap (_, _, cts, _)) a =
  lookupDef (error ("hLookup: failed lookup " ++ show a ++ " in heap")) a cts

hAddresses :: Heap a -> [Addr]
hAddresses (Heap (_, _, cts, _)) = [ addr | (addr, _) <- cts ]

hSize :: Heap a -> Int
hSize (Heap (size, _, _, _)) = size

hNull :: Addr
hNull = Addr 0

hIsnull :: Addr -> Bool
hIsnull = (== Addr 0)

-- Helper function for `hUpdate`, `hFree`
hRemove :: [(Addr, a)] -> Addr -> [(Addr, a)]
hRemove [] a =
  error ("remove: attempt to update or free a nonexistent address " ++ show a)
hRemove ((a', n) : cts) a | a == a'   = cts
                          | otherwise = (a', n) : hRemove cts a
