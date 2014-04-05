{- |
Module      :  Euler.P024
Description :  Problem 024
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Lexicographic permutations

A permutation is an ordered arrangement of objects. For example, 3124
is one possible permutation of the digits 1, 2, 3 and 4. If all of the
permutations are listed numerically or alphabetically, we call it
lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2,
3, 4, 5, 6, 7, 8 and 9?

-}
module Euler.P024
       ( solve
       ) where

import Data.Char

solve :: IO ()
solve = putStrLn . fmap (chr . (+) (ord '0')) $ findNthPermutation 999999 [0..9]

fac :: Integral i => i -> Integer
fac n = product [1..fromIntegral n]

findNthPermutation :: Integer -> [a] -> [a]
findNthPermutation n xxs@(x:xs)
  | n == 0 = xxs
  | f > n = x:findNthPermutation n xs
  | otherwise = let i = n `div` f
                    n' = n - i*f
                    (as,b:bs) = splitAt (fromIntegral i) xxs
                in b : findNthPermutation n' (as++bs)
  where
    f = fac $ length xs

findNthPermutation _ _ = []
