module Euler.Prime
       ( sqrt'
       , factorizeSingleNumber
       , factors
       , divisors
       , coPrime
       , primes
       , sunion
       ) where

import Data.List
import Euler.SList

-- | Take the sqrt from the given integer value and round it down.
sqrt' :: Integral n => n -> n
sqrt' = floor . (sqrt :: Double -> Double) . fromIntegral

-- | Factorize a single number by testing all nmbers up to the square
-- root of the given number.
factorizeSingleNumber :: Int -> [Int]
factorizeSingleNumber = f [] 2
  where
    f ps p n
      | r == 0    = f (p:ps) p d
      | p > (sqrt' n) = reverse (n:ps)
      | otherwise = f ps (p+1) n
      where
        (d,r) = n `divMod` p

factors :: Int -> [Int]
factors x = f (sqrt' x) primes x
  where
    f :: Int-> [Int] -> Int -> [Int]
    f sqrtn (p:ps) n
      | p > sqrtn  = if n > 1 then [n] else []
      | r == 0 = p : f (sqrt' d) (p:ps) d
      | otherwise = f sqrtn ps n
      where
        (d,r) = n `divMod` p

divisors :: Int -> Int
divisors = product . fmap (succ . length) . group . factors

-- | Two integers are coprime if their common denominator is 1.
coPrime :: Integral n => n -> n -> Bool
coPrime a b = 1 == gcd a b

primes :: [Int]
primes = primesTMWE

_Y :: (t -> t) -> t
_Y g = g (_Y g)

joinT :: Ord a => [[a]] -> [a]
joinT ((x:xs):t) = x : (sunion xs . joinT . pairs) t
  where
    pairs :: Ord a => [[a]] -> [[a]]
    pairs (xxs:yys:tt) = sunion xxs yys : pairs tt

primesTMWE :: [Int]
primesTMWE = [2,3,5,7] ++ _Y ((11:) . tail . gapsW 11 wheel
                              . joinT . hitsW 11 wheel)

gapsW :: Int -> [Int] -> [Int] -> [Int]
gapsW k (d:w) s@(c:cs)
  | k < c = k : gapsW (k+d) w s
  | otherwise = gapsW (k+d) w cs

hitsW :: Int -> [Int] -> [Int] -> [[Int]]
hitsW k (d:w) s@(p:ps)
  | k < p = hitsW (k+d) w s
  | otherwise = scanl (\c d' -> c+p*d') (p*p) (d:w) : hitsW (k+d) w ps

wheel :: [Int]
wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:
        4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel
