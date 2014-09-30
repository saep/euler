{- |
Module      :  Euler.P030
Description :  Problem 030
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Digit fifth powers

Surprisingly there are only three numbers that can be written as the
sum of fourth powers of their digits:

    1634 = 1^4 + 6^4 + 3^4 + 4^4
    8208 = 8^4 + 2^4 + 0^4 + 8^4
    9474 = 9^4 + 4^4 + 7^4 + 4^4

As 1 = 1^4 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of
fifth powers of their digits.

-}
module Euler.P030
       ( solve
       ) where

import           Control.Monad
import           Data.List     (sort)
import           Data.Maybe
import           Euler.Numbers (toDigits)

solve :: Monad m => m Int
solve = return . sum $ nthPowerDigits 5

-- | Calculate the numbers which are equal to the sum of ther digits
-- to the nth power.
nthPowerDigits :: Int -> [Int]
nthPowerDigits n =
  drop 2 -- 0 and 1 are found too, so drop them
  . mapMaybe (digitSumEqualsNumber n)
  $ concatMap (unorderedPossibilities 0) [1..ub]

  where
    -- calculate the upper bound
    ub = head $ dropWhile (\x -> x*9^n > 10^x) [1..]

unorderedPossibilities :: Int -> Int -> [[Int]]
unorderedPossibilities l n
  | n > 0     = [ x:xs | x <- [l..9], xs <- unorderedPossibilities x (n-1) ]
  | otherwise = [[]]

-- | Return "Just" the sum of the given list of numbers by the power of p.
digitSumEqualsNumber :: Int -> [Int] -> Maybe Int
digitSumEqualsNumber p ds = guard (digits == ds) >> return dsum
  where
    dsum = sum $ fmap (^p) ds
    digits = sort $ toDigits dsum
