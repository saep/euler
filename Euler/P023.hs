{- |
Module      :  Euler.P023
Description :  Problem 023
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

import qualified Data.Set as S
import Euler.SList
import Euler.Numbers
import Euler.Prime

solve :: IO ()
solve =
  let limit = 28123
      seed = [12..limit] `sremoveAll` primes
      -- abundant numbers
      as = evalState (filterM isAbundant seed) (initialWithDivisorsState limit)
  in print $ abundantSumpairs limit as

-- | Given an upper limit and a list of abundant numbers, calculate
-- all the pairs of two abundant numbers.
abundantSumpairs :: Int -> [Int] -> Int
abundantSumpairs limit abundantNumbers =
    S.foldl' (+) 0 . fst
    $ foldl step (S.fromAscList [1..limit], abundantNumbers) abundantNumbers

  where
    step :: (S.Set Int, [Int]) -> Int -> (S.Set Int, [Int])
    step (xs, a:as) n = let aps = S.fromAscList
                                    . takeWhile (<= limit) $ fmap (+n) (a:as)
                          in (xs S.\\ aps, as)
