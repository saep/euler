{- |
Module      :  Euler.P028
Description :  Problem 028
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Number spiral diagonals

Starting with the number 1 and moving to the right in a clockwise
direction a 5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is
101.

What is the sum of the numbers on the diagonals in a 1001 by 1001
spiral formed in the same way?

-}
module Euler.P028
       ( solve
       ) where

solve :: IO ()
solve = print $ f 1001
  where
    f :: Int -> Int
    -- f n = 1 + sum $ fmap (\n -> n^2 + n^2 - n + 1 + n^2 - 2*n +2 + n^2 - 3*n + 3) [3,5..n]
    -- f n = (1+) . sum $ fmap (\n -> 4 * n^2 - 6*n + 6) [3,5..n]
    -- f n = (1+) . sum $ fmap (\n -> 4 * (n+1)^2 - 6 * (n+1) + 6) [2,4..n-1]
    -- f n = (1+) . sum $ fmap (\n -> 4 * (2*n+1)^2 - 6 * (2*n+1) + 6) [1..(n-1) `div` 2]
    -- f n = (1+) . sum $ fmap (\n -> 4 * 4*n^2 + 16*n + 4 - 12 *n + 6 + 6) [1..(n-1) `div` 2]
    -- f n = (1+) . sum $ fmap (\n -> 16*n^2 + 4*n + 16) [1..(n-1) `div` 2]
    f n = let n' = (n-1) `div` 2
          -- Let's make a formula from this:
          --
          -- in 1 + 16*(n'*(n'+1)*(2*n'+1))`div`6 + 4*(n'*(n'+1) `div` 2) + 16*n'
          -- in 1 + 2*( 4*(n'*(n'+1)*(2*n'+1))`div`3 + n'*(n'+1) + 8*n')
          -- in 1 + 2*( 4*(2*n'^3+3*n'^2 + n')`div`3 + n'^2 + n' + 8*n')
          -- in 1 + 2*( (8*n'^3+15*n'^2 + 31*n')`div`3)
          in (16*n'^3 + 30*n'^2 + 62*n' + 3) `div` 3

-- solve = print . fst
--         . foldl (\(a,l) b -> (a+l+b,l+b)) (1,1)
--         $ concatMap (replicate 4) [2,4..1000]
