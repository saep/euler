{- |
Module      :  Euler.P036
Description :  Problem 036
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Double-base palindromes

The decimal number, 585 = 1001001001_2 (binary), is palindromic in both
bases.

Find the sum of all numbers, less than one million, which are
palindromic in base 10 and base 2.

(Please note that the palindromic number, in either base, may not
include leading zeros.)

-}
module Euler.P036
       ( solve
       ) where

import           Data.Bits
import           Euler.Numbers (toNum)

solve :: IO Int
solve = return . sum $ [ x | n <- [2,4,6], x <- base10candidates n, isBinaryPalindrome x ]

isBinaryPalindrome :: Int -> Bool
isBinaryPalindrome n = foldr test True [0..u`div`2]
  where
    u = floor $ logBase 2 (fromIntegral n)

    test i r = testBit n i == testBit n (u-i) && r

-- This works only for even n which suffices here.
base10candidates :: Int -> [Int]
base10candidates n = concat [ pals (x:xs) | x <- [1,3..9], xs <- pick ((n`div`2)-1) ]
  where
    pick 0 = [[]]
    pick l = [ x:xs | x <- [0..9], xs <- pick (l-1) ]

    pals xs = fmap toNum [xs ++ reverse xs, xs ++ tail (reverse xs)]
