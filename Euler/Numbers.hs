{- |
Module      :  Euler.Numbers
Description :  Number theoretic functions
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

This module gather number theoretic functions.
-}
module Euler.Numbers
       ( WithDivisors
       , initialWithDivisorsState
       , isAmicable
       , isPerfect
       , isAbundant
       , amicableNumbersTo
       , toDigits
       , toNum
       , pythagoreanTriplets
       , pentagonal
       , pentagonals
       , triangular
       , triangulars
       , hexagonal
       , hexagonals
       , heptagonal
       , heptagonals
       , octagonal
       , octagonals
       , module Control.Monad.State
       ) where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.IntMap         as M
import           Data.List           (foldl')
import           Data.Maybe
import           Euler.Prime
import           Euler.SList

-- | Simply an IntMap containing a list of Ints.
type DivisorMap = M.IntMap [Int]
-- | Convenience type to store a DivisorMap in a state monad.
type WithDivisors = State DivisorMap

-- | Create an initial map of divisors by creating an [1] entry for
-- all primes up to n (exclusive).
initialWithDivisorsState :: Int -> DivisorMap
initialWithDivisorsState n = M.fromList $ (1,[]) : zip (takeWhile (< n) primes) (repeat [1])

-- | A list of amicable numbers wrapped up in a state monad.
amicableNumbersTo :: Int -> WithDivisors [Int]
amicableNumbersTo n = filterM isAmicable ([2..n] `sremoveAll` primes)

-- | Monady predicate to test whether the given number is an amicable number.
isAmicable :: Int -> WithDivisors Bool
isAmicable n = do
  dn <- sumDivisors n
  if n == dn
    then return False
    else (==n) <$> sumDivisors dn

isPerfect :: Int -> WithDivisors Bool
isPerfect n = (n==) <$> sumDivisors n

isAbundant :: Int -> WithDivisors Bool
isAbundant n = (n<) <$> sumDivisors n

-- | Calculate the sum of divisors of n by using the (possibly)
-- precalculated state.
sumDivisors :: Int -> WithDivisors Int
sumDivisors n = do
  m <- get
  case M.lookup n m of
    Just xs -> return $ sum xs
    Nothing -> modify (mkDivisors n) >> sumDivisors n

-- | Internal function to calculate a new divisormap with the value n
-- by using the precalculated map m.
mkDivisors :: Int -> DivisorMap -> DivisorMap
mkDivisors n m
  | n == fd      = M.insert n [1] m
  | isNothing pd = mkDivisors n $ mkDivisors p m
  | otherwise =
    let pd' = fromJust pd ++ [p]
        -- new divisors
        nd = filter (\x -> x <= n && 0 ==  n `mod` x) $ fmap (*fd) pd'
        -- proper divisors of n
        pdn = pd' `sunion` insert fd nd
    in M.insert n ((init . snub) pdn) m

  where
    fd = (head . factors) n -- first divisor
    p = n `div` fd          -- previous
    pd = M.lookup p m       -- previous divisors

-- | Convert an integer to a list of its digits.
toDigits :: Int -> [Int]
toDigits n = go (n `divMod` 10)
  where
    go (d,r)
      | d == 0 = [r]
      | otherwise = r:go (d `divMod` 10)

-- | Convert a digit list to an Int.  Only works for lists with
-- elements in [0..9].
toNum :: [Int] -> Int
toNum = fst . foldl' (\(acc,n) x -> (acc+x*n, n*10)) (0,1) . reverse

-- | The following formula describes how to calculate the pythagorean
-- triplets if m and n are coprime (i.e. their greatest common
-- denominator is 1).
--
-- Assume that m > n:
-- a = k (m^2 - n^2)
-- b = 2 k m n
-- c = k (m^2 + n^2)
pythagoreanTriplets :: [[(Int, Int, Int)]]
pythagoreanTriplets = go 3 2
  where
    go m n
      | m <= n = go (m+1) 1
      | coPrime m n = fmap (\k -> (a k,b k,c k)) [1..] : go m (n+1)
      | otherwise = go m (n+1)
      where
        a k = k*(m*m - n*n)
        b k = k*2*m*n
        c k = k*(m*m + n*n)

-- | sum_{k=1}^n k
triangular :: Integral n => n -> n
triangular n = n * (n+1) `div` 2

triangulars :: Integral n => [n]
triangulars = scanl1 (+) [1..]

-- | n(3n-1)/2
pentagonal :: Integral n => n -> n
pentagonal n = n * (3*n - 1) `div` 2

pentagonals :: Integral n => [n]
pentagonals = scanl1 (+) [1,4..]

hexagonal :: Integral n => n -> n
hexagonal n = n*(2*n-1)

hexagonals :: Integral n => [n]
hexagonals = scanl1 (+) [1,5..]

heptagonal :: Integral n => n -> n
heptagonal n = n*(5*n-3)`div`2

heptagonals :: Integral n => [n]
heptagonals = scanl1 (+) [1,6..]

octagonal :: Integral n => n -> n
octagonal n = n*(3*n-2)

octagonals :: Integral n => [n]
octagonals = scanl1 (+) [1,7..]

