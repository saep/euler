module Euler.Prime
       ( sqrt'
       , factorizeSingleNumber
       , isPrime
       , factors
       , divisors
       , coPrime
       , primes
       , sunion
       , atkin
       ) where

import Data.Bits
import Data.List
import Euler.SList
import qualified Data.IntSet as S

-- | Take the sqrt from the given integer value and round it down.
sqrt' :: Integral n => n -> n
sqrt' = floor . (sqrt :: Double -> Double) . fromIntegral

isPrime :: Int -> Bool
isPrime n = n /= 1 && foldr test True [2..sqrt' n]
  where
    test d r = n `mod` d /= 0 && r

-- | Factorize a single number by testing all nmbers up to the square
-- root of the given number.
factorizeSingleNumber :: Int -> [Int]
factorizeSingleNumber = f [] 2
  where
    f ps p n
      | r == 0      = f (p:ps) p d
      | p > sqrt' n = reverse (n:ps)
      | otherwise   = f ps (p+1) n
      where
        (d,r) = n `divMod` p

factors :: Int -> [Int]
factors x = f (sqrt' x) primes x
  where
    f :: Int-> [Int] -> Int -> [Int]
    f sqrtn (p:ps) n
      | p > sqrtn  = [ n | n > 1 ]
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

atkin :: Int -> S.IntSet
atkin m = let bs = atkin' m
          in S.fromAscList (2:[ x | x <- [3,5..m], testBit bs x ])

atkin' :: Int -> Integer
atkin' m =
    let sqm = 1 + sqrt' m -- to avoid missing one edge case
        cps = foldl' complementBit 12 $ concat
            [ [ n | x <- [1..sqm`div`2], y <- [1..(sqm+2*x)*(sqm-2*x)]
              , let n = 4*x*x + y*y
                    -- 4x^2 + y^2 <= m --> y <= sqrt(m - 4x^2)
                    -- <=> y <= (sqm + 2x)(sqm - 2x)
              , n `mod` 12 == 1 || n `mod` 12 == 5
              , n <= m
              ]
            , [ n | let sq3 = sqrt' 3
              , x <- [1..sqm `div` sq3], y <- [1..(sqm+sq3*x)*(sqm-sq3*x)]
              , let n = 3*x*x + y*y
                    -- 3x^2 + y^2 <= m --> y <= sqrt(m - 3x^2)
                    -- <=> y <= (sqm + sq3*x)(sqm - sq3*x)
              , n `mod` 12 == 7
              , n <= m
              ]
            , [ n | x <- [1..sqm], y <- [1..x-1]
              , let n = 3*x*x - y*y
              , n `mod` 12 == 11
              , n <= m
              ]
            ]
    in foldl' clearBit cps [ k | n <- [5..sqm], k <- [n*n,2*n*n..m] ]
