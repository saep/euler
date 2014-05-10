{- |
Module      :  Euler.P023
Description :  Problem 23
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Non-abundant sums

A perfect number is a number for which the sum of its proper divisors
is exactly equal to the number. For example, the sum of the proper
divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28
is a perfect number.

A number n is called deficient if the sum of its proper divisors is
less than n and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
smallest number that can be written as the sum of two abundant numbers
is 24. By mathematical analysis, it can be shown that all integers
greater than 28123 can be written as the sum of two abundant
numbers. However, this upper limit cannot be reduced any further by
analysis even though it is known that the greatest number that cannot
be expressed as the sum of two abundant numbers is less than this
limit.

Find the sum of all the positive integers which cannot be written as
the sum of two abundant numbers.

-}
module Euler.P023
       ( solve
       ) where

import Control.Monad
import Euler.SList
import Euler.Prime
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VMU

limit :: Int
limit = 28123

isAbundant :: Int -> Bool
isAbundant n = sumOfProperDivisors n > n
{-
Since 18 and 12 are abundant numbers, we can conclude that any number greater
than 18, which is evenly divisable by 6 is the composite of two abundant
numbers. This leaves the numbers to check, which have a remainder of [1..5]. As
20 is the next abundant number, any number with a modulo 6 remainder of 2 ≥ 32
is also the sum of two abundant numbers. As 2*20 will yield a modulo 6 remainder
of 4, every number ≥ 52 will also be composable of two abundant numbers.
So every number that is divisable by 2 is the sum of to abundant numbers above
46. So it probably saves some calculation time to manually add the even numbers
below 46.
-}
solve :: Monad m => m Int
solve = return . sum $ [1..23]
                ++ [26] -- `mod` 6 == 2
                ++ [28,34,46] -- `mod` 6 == 4
                ++ [ x | x <- [25,27..limit], isNotSumOfAbundantNumbers x ]

isNotSumOfAbundantNumbers :: Int -> Bool
isNotSumOfAbundantNumbers n =
    null [ () | x <- takeWhile (< n) oddAbundantNumbers
         , abundantNumbersVector VU.! (n-x)
         ]

oddAbundantNumbers :: [Int]
oddAbundantNumbers = filter (abundantNumbersVector VU.!) [1,3..limit]

abundantNumbersVector :: VU.Vector Bool
abundantNumbersVector = VU.create $ do
    v <- VMU.replicate (limit+1) False

    forM_ ([12,18..limit] `sunion` [20,40..limit])
        $ \i -> VMU.write v i True

    forM_ ([13..limit] `sremoveAll` primes) $ \i -> do
        a <- VMU.read v i
        when (not a && isAbundant i) $ VMU.write v i True

    return v
