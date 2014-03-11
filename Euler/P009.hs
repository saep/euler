{- |
Module      :  Euler.P009
Description :  Problem 9
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Special Pythagorean triplet

A Pythagorean triplet is a set of three natural numbers, a < b < c,
for which, a2 + b2 = c2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c =
1000.  Find the product abc.

-}
module Euler.P009
       ( solve
       ) where

import Euler.Prime (coPrime)

solve :: IO ()
solve = print . (\(a,b,c) -> a*b*c) $ findPythagoreanTriplet 3 2 1000

-- | The following formula describes how to calculate the pythagorean
-- triplets if m and n are coprime (i.e. their greatest common
-- denominator is 1).
--
-- Assume that m > n:
-- a = k (m^2 - n^2)
-- b = 2 k m n
-- c = k (m^2 + n^2)
--
-- So the following equatino holds:
-- k (m^2 - n^2) + 2 k m n + k (m^2 + n^2) = s
-- \<=\> k (2 m^2 + 2 m n) =  2 k m (m + n) = s
findPythagoreanTriplet :: Int -> Int -> Int -> (Int, Int, Int)
findPythagoreanTriplet m n s
  | m <= n = findPythagoreanTriplet (m+1) 1 s
  | coPrime m n && candidate == s =
     (k * (m^2 - n^2), 2*k*m*n, k*(m^2 + n^2))
  | otherwise = findPythagoreanTriplet m (n+1) s
  where
    sumConstant :: Int
    sumConstant =  2 * m * (m+n)

    candidate :: Int
    candidate = case (dropWhile (< s) . fmap (sumConstant *)) [1..] of
      [] -> s+1
      ts -> head ts

    k :: Int
    k = candidate `div` sumConstant
