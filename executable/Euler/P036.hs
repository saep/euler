module Euler.P036
       ( solve
       ) where

import           Data.Bits
import           Euler.Numbers (toNum)

solve :: Monad m => m Int
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
