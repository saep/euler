module Euler.P023
       ( solve
       ) where

import           Control.Monad
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VMU
import           Euler.Prime                 (primes, sumOfProperDivisors)
import           Euler.SList                 (sremoveAll, sunion)

limit :: Int
limit = 28123

isAbundant :: Int -> Bool
isAbundant n = sumOfProperDivisors n > n

{-
Since 18 and 12 are abundant numbers, we can conclude that any number greater
than 18 which is evenly divisable by 6 is the composite of two abundant
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

