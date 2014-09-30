{- |
Module      :  Euler.P004
Description :  Solution for riddle 4.
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Largest palindrome product

A palindromic number reads the same both ways. The largest palindrome
made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit
numbers.

-}
module Euler.P004
       -- ( solve
       -- ) where
       where

solve :: Monad m => m Int
solve = return (findPalindrome 0 999 999)

-- naive = maximum $ [ i*j | i <- [999,998..100], j <- [999,998..100], isPalindrome (i*j) ]

findPalindrome :: Int -> Int -> Int -> Int
findPalindrome m i j
  | i < 100            = m
  | ij <= m || j < 100 = findPalindrome m (i-1) (i-1)
  | isPalindrome ij    = findPalindrome ij i (j-1)
  | otherwise          = findPalindrome m i (j-1)
  where
    ij = i*j

-- findLargest3DigitPalindrome = head . takeWhile (\n -> n*n

isPalindrome :: Show a => a -> Bool
isPalindrome a = show a == reverse (show a)
