module Alloc
  ( Heap
  , Addr
  , hInitial
  , hAlloc
  , hUpdate
  , hFree
  , hLookup
  , hAddresses
  , hSize
  , hNull
  , hIsnull
  ) where

import           Control.Exception
import           Data.AssocList

newtype Heap a = Heap (Int, [Addr], [(Addr, a)])

instance Show (Heap a) where
  show h = "Heap of size " ++ show (hSize h)

newtype Addr = Addr Int
  deriving Eq

instance Num Addr where
  (+) (Addr n1) (Addr n2) = Addr (n1 + n2)
  (-) (Addr n1) (Addr n2) = Addr (n1 - n2)
  (*) (Addr n1) (Addr n2) = Addr (n1 * n2)
  abs (Addr n) = Addr (abs (n))
  fromInteger n = Addr (fromInteger n)
  signum (Addr n) = Addr (signum n)

instance Show Addr where
  show (Addr a) = "#" ++ show a

data InvalidHeapError = InvalidHeapError
  deriving Show
instance Exception InvalidHeapError

hInitial :: Heap a
hInitial = Heap (0, map Addr [1 ..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap (_, [], _)) _ = throw InvalidHeapError
hAlloc (Heap (size, (next : free), cts)) n =
  (Heap (size + 1, free, (next, n) : cts), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap (size, free, cts)) a n = Heap (size, free, (a, n) : remove cts a)

hFree :: Heap a -> Addr -> Heap a
hFree (Heap (size, free, cts)) a = Heap (size - 1, a : free, remove cts a)

hLookup :: Heap a -> Addr -> a
hLookup (Heap (_, _, cts)) a =
  lookupDef (error ("Can't find node " ++ show a ++ " in heap")) a cts

hAddresses :: Heap a -> [Addr]
hAddresses (Heap (_, _, cts)) = [ addr | (addr, _) <- cts ]

hSize :: Heap a -> Int
hSize (Heap (size, _, _)) = size

hNull :: Addr
hNull = 0

hIsnull :: Addr -> Bool
hIsnull a = a == 0

remove :: [(Addr, a)] -> Addr -> [(Addr, a)]
remove [] a =
  error ("Attempt to update or free a nonexistent address " ++ show a)
remove ((a', n) : cts) a | a == a'   = cts
                         | otherwise = (a', n) : remove cts a
