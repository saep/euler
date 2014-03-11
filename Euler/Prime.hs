module Euler.Prime where

import Data.Bits
import Data.List
import qualified Data.Vector as V

type BitSet = Integer
type Offset = Int
type PrimeSieve = V.Vector Int

-- | Take the sqrt from the given integer value and round it down.
sqrt' :: Integral n => n -> n
sqrt' = floor . sqrt . fromIntegral

-- | Factorize a single number by
factorizeSingleNumber :: Int -> [Int]
factorizeSingleNumber = f [] 2
  where
    f ps p n
      | r == 0    = f (p:ps) p d
      | p > (sqrt' n) = reverse (n:ps)
      | otherwise = f ps (p+1) n
      where
        (d,r) = n `divMod` p


-- | Create a list of primes using the sieve of Eratosthenes.
sieveOfEratosthenes :: Int -> PrimeSieve
sieveOfEratosthenes n =
  let primeBitSet = foldl' (markPrime n Nothing 0) 3 [2..(sqrt' n)]
  in V.unfoldr (mkvec n 0 primeBitSet) 2

-- | Given the sieve result of an Integer where an unset bit indicates
-- a prime, return a Maybe (Int, Int) value where the first value is
-- the next prime and the second one is the next index to test. Return
-- Nothing when the range of primes is exceeded.
mkvec :: Int -> Offset -> BitSet -> Int -> Maybe (Int, Int)
mkvec n offset bs i
  | i+offset > n = Nothing
  | testBit bs i = mkvec n offset bs (i+1)
  | otherwise    = Just (i+offset, i+1)

-- | Create a list of primes using the sieve of Erathosthenes with a
-- precalculated vector of primes.
sieveOfEratosthenes' :: Int -> PrimeSieve -> PrimeSieve
sieveOfEratosthenes' n ps
  | V.null ps = sieveOfEratosthenes n
  | n <= maxp = ps
  | otherwise = ps V.++ V.unfoldr (mkvec n offset (extendSieve 0)) 0

  where
    maxp = V.last ps
    offset = maxp + 1

    extendSieve :: BitSet -> BitSet
    extendSieve bitset = V.foldl' go bitset ps

    go bs p = let start = p * (1 + (maxp `div` p))
                  bitset = markPrime n (Just start) offset bs p
              in foldl' (markPrime n Nothing offset) bitset [offset..(sqrt' n)]

-- | Mark a number in the BitSet starting at an offset as not
-- prime. If you provie anything other than Nothing as the second
-- argument, then the value will be used as the start value and is not
-- skipped.
markPrime :: Int -> Maybe Int -> Offset -> BitSet -> Int -> BitSet
markPrime n (Just start) offset bs p =
  foldl' setBit bs $ fmap (subtract offset) [start,start+p..n]
markPrime n Nothing offset bs p
  | testBit bs (p-offset) = bs
  | otherwise = foldl' setBit bs $ fmap (subtract offset) [2*p,3*p..n]


findNthPrime :: Int -> V.Vector Int -> Int
findNthPrime n ps
  | V.length ps < n = findNthPrime n $ sieveOfEratosthenes' n' ps
  | otherwise = ps V.! (n-1)
  where
    n' = max (8 * n) (2 * V.last ps)

-- | Two integers are coprime if their common denominator is 1.
coPrime :: Integral n => n -> n -> Bool
coPrime a b = 1 == gcd a b
