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
