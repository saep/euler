{- |
Module      :  Euler.P032
Description :  Problem 032
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Pandigital products

We shall say that an n-digit number is pandigital if it makes use of
all the digits 1 to n exactly once; for example, the 5-digit number,
15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254,
containing multiplicand, multiplier, and product is 1 through 9
pandigital.

Find the sum of all products whose multiplicand/multiplier/product
identity can be written as a 1 through 9 pandigital.  HINT: Some
products can be obtained in more than one way so be sure to only
include it once in your sum.

-}
module Euler.P032
       ( solve
       ) where

import Data.Bits
import Data.List (nub, sort, (\\), group)
import Euler.Numbers (toDigits, toNum)

solve :: IO Int
solve = return . sum . fmap head . group . sort $ pandigitalProducts 9

pandigitalProducts :: Int -> [Int]
pandigitalProducts n = fmap (\(a,b,_) -> a*b) . filter isPandigital $ candidates n

isPandigital :: (Int, Int, [Int]) -> Bool
isPandigital (a,b,rs) = (rs==) . sort $ toDigits (a*b)

-- | Truncate the possibilities to try by looking at the indexes at
-- which the possible solutions are split.
--
-- i < j : This would collide with the parition function and should
-- have been covered before when i's and j's value were swapped.
--
-- The highest possible digit count for the multiplication of two
-- numbers i,j is (digits i)+(digits k) and the smallest possible
-- digit count is (digits i)+(digits k). (Just take 10*10 and 99*99 as
-- an example for this). And since k = j-i the following conditions
-- must be fulfilled.
--
-- hasValidDimensions :: Int -> Int -> Int -> Bool
-- hasValidDimensions i j n = i < j && (2*j == n || n == 2*j-1)

validDimensions :: Int -> [(Int, Int)]
validDimensions n = let js = nub [n `div` 2, (n+1) `div` 2]
                    in [ (i,j) | j <- js, i <- [1..j-1], j-i >= i ]

partition :: Int -> Int -> [Int] -> (Int, Int, [Int])
partition i j xs = let (a,r) = splitAt i xs
                       (b,c) = splitAt (j-i) r
                   in (toNum a, toNum b,c)

candidates :: Int -> [(Int, Int, [Int])]
candidates n = [ partition i j (ps++([1..n] \\ ps))
               | (i,j) <- validDimensions n
               , ps <- perms j [1..n] 0 ]

perms :: Int -> [Int] -> Int -> [[Int]]
perms n ns bs
  | n <= 0 = [[]]
  | otherwise = [ x:xs | x <- ns, (not . testBit bs) x, xs <- perms (n-1) ns (setBit bs x) ]
