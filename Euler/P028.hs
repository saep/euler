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

-- diagonalSum (2*k+1) = 1 + Σ_(i=1)^k 1 + 4(2i+1)^2 - 12*i
-- = 1 + Σ_(i=1)^k 16i^2 + 4i + 4
-- = 1 + 16k(k+1)(2k+1)/6 + 4k(k+1)/2 + Σ_(k=1)^k 4
-- = 1 + 8k(k+1)(2k+1)/3 + 2k(k+1) + 4*k

solve :: IO Int
solve = return $ diagonalSum 1001

diagonalSum :: Int -> Int
diagonalSum n = let k = (n-1) `div` 2
                in 1 + (8*k*(k+1)*(2*k+1)) `div` 3 + 2*k*(k+1) + 4*k


