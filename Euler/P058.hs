{- |
Module      :  Euler.P058
Description :  Problem 58
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Spiral primes
Problem 58

Starting with 1 and spiralling anticlockwise in the following way, a square
spiral with side length 7 is formed.

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right
diagonal, but what is more interesting is that 8 out of the 13 numbers lying
along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.

If one complete new layer is wrapped around the spiral above, a square spiral
with side length 9 will be formed. If this process is continued, what is the
side length of the square spiral for which the ratio of primes along both
diagonals first falls below 10%?
-}
module Euler.P058
       ( solve
       ) where

import Euler.Prime (isPrimeMillerRabin)

solve :: Monad m => m Int
solve = return . fst
        $ until (\(pl,ps) -> pl*2-1 > 10*ps ) nextSprialWithPrimePercentage (7, 8)

nextSprialWithPrimePercentage :: (Int,Int) -> (Int,Int)
nextSprialWithPrimePercentage (pl, pp) =
    let pl' = pl+2
        cns = filter isPrimeMillerRabin $ fmap (\i -> pl*pl + i*(pl+1)) [1,2,3]
        pp' = length cns + pp
    in (pl', pp')

