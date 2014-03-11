module Euler.Prime where

import Data.Bits
import Data.List
import qualified Data.Vector as V

-- | Factorize a single number by
factorizeSingleNumber :: Int -> [Int]
factorizeSingleNumber = f [] 2
  where
    f ps p n
      | p > ((floor . sqrt . fromIntegral) n) = reverse (n:ps)
      | r == 0    = f (p:ps) p d
      | otherwise = f ps (p+1) n
      where (d,r) = n `divMod` p


-- | Create a list of primes using the sieve of Eratosthenes.
sieveOfEratosthenes :: Int -> V.Vector Int
sieveOfEratosthenes n = V.unfoldr (mkvec n (foldl' go 3 [2..sqrtn])) 2
  where
    sqrtn = (floor . sqrt) (fromIntegral n :: Double)
    go :: Integer -> Int -> Integer
    go bs p
      | testBit bs p = bs
      | otherwise    = foldl' setBit bs [2*p,3*p..n]

mkvec :: Int -> Integer -> Int -> Maybe (Int, Int)
mkvec n bs i
  | i > n        = Nothing
  | testBit bs i = mkvec n bs (i+1)
  | otherwise    = Just (i, i+1)
