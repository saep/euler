{- |
Module      :  Euler.P053
Description :  Problem 53
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Combinatoric selections

There are exactly ten ways of selecting three from five, 12345:

123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

In combinatorics, we use the notation, C 5 3 = 10.

In general, C n r = n!/(r!(n−r)!) ,where r ≤ n, n! = n×(n−1)×...×3×2×1,
and 0! = 1.

It is not until n = 23, that a value exceeds one-million: C 23 10 = 1144066.

How many, not necessarily distinct, values of  C n r, for 1 ≤ n ≤ 100, are
greater than one-million?
-}
module Euler.P053
       ( solve
       ) where

-- c (n+1) r = (n+1)! / (r!(n+1-r)!)
-- = (n+1)n! / (r!(n+1-r)(n-r)!)
-- = (n+1)/(n+1-r) * n!/(r!(n-r)!)
-- = (n+1)/(n+1-r) * c n r
-- --> c (n+1) r ≥ c n r \forall 0 ≤ r ≤ n
--
-- c n (r-1) = n!/(r-1)!(n+1-r)!
-- = n!r/(r!(n+1-r)(n-r)!
-- = r/(n+1-r) * n!/(r!(n-r)!)
-- = r/(n+1-r) * c n r
--
-- c n (r+1) = n! / ((r+1)r!(n-1-r)!)
-- = (n-r)/(r+1) * c n r
--
solve :: Monad m => m Int
solve = return . sum $ f (23, 9, (1144066 * 5) `div` 7)

f :: (Int, Int, Int) -> [Int]
f (n,r,cnr)
    | n > 100 = []
    | cnr > 1000000 = f (n, r-1, cnr `div` (n+1-r) * r)
    | otherwise = n-2*r-1 : f (n+1,r,cnr `div` (n+1-r) * (n+1) )

