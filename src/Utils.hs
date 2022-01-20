module Utils where

-- A.1 Heap and address types
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

hInitial :: Heap a
hInitial = Heap (0, map Addr [1 ..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (Heap (size, (next : free), cts)) n =
  (Heap (size + 1, free, (next, n) : cts), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (Heap (size, free, cts)) a n = Heap (size, free, (a, n) : remove cts a)

hFree :: Heap a -> Addr -> Heap a
hFree (Heap (size, free, cts)) a = Heap (size - 1, a : free, remove cts a)

hLookup :: Heap a -> Addr -> a
hLookup (Heap (_, _, cts)) a =
  aLookup cts a (error "can't find node " ++ show a ++ " in heap")

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
remove ((a', n) : cts) a | a == a' = cts
                         | a /= a' = (a', n) : remove cts a

-- A.2 Association list type
type Assoc a b = [(a, b)]

alookup :: (Assoc a b) -> a -> b -> b
alookup [] k' def = def
aLookup ((k, v) : bs) k' def | k == k' = v
                             | k /= k' = aLookup bs k' def

aDomain :: Assoc a b -> [a]
aDomain alist = [ key | (key, _) <- alist ]

aRange :: Assoc a b -> [b]
aRange alist = [ val | (_, val) <- alist ]

aEmpty :: Assoc a b
aEmpty = []

-- A.3 Generating unique names
type NameSupply = Int

getName :: NameSupply -> [Char] -> (NameSupply, [Char])
getName name_supply prefix = (name_supply + 1, makeName prefix name_supply)

getNames :: NameSupply -> [[Char]] -> (NameSupply, [[Char]])
getNames name_supply prefixes =
  (name_supply + length prefixes, zipWith makeName prefixes [name_supply ..])

initialNameSupply :: NameSupply
initialNameSupply = 0

makeName :: String -> NameSupply -> String
makeName prefix ns = prefix ++ "_" ++ show ns

-- Map/fold combination
mapAccuml :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccuml f acc []       = (acc, [])
mapAccuml f acc (x : xs) = (acc2, x' : xs')
 where
  (acc1, x' ) = f acc x
  (acc2, xs') = mapAccuml f acc1 xs

-- Generate arbitrary-length strings of spaces
space :: Int -> String
space n = take n (repeat ' ')

-- Note: the following definitions in the tutorial's Utils module
-- were not reproduced here:
-- 
-- - The initial definitions, which were meant to make Gofer (Haskell)
--   code more Miranda-friendly. Since we are using Haskell, there is
--   no need for this.
-- - Set operations. We can use the Data.Set module.
-- - A sort function. We can use the Data.Sort module.
