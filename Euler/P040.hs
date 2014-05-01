{- |
Module      :  Euler.P040
Description :  Problem 040
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Champernowne's constant

An irrational decimal fraction is created by concatenating the
positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12th digit of the fractional part is 1.

If d(n) represents the nth digit of the fractional part, find the value
of the following expression.

d(1) × d(10) × d(100) × d(1.000) × d(10.000) × d(100.000) × d(1.000.000)

-}
module Euler.P040
       ( solve
       ) where

import Data.Char

solve :: IO ()
solve = print . product $ fmap (d . (10^)) [0..6]

d :: Integer -> Int
d i = d' 0 (i-1) [1..]

d' :: Integer -> Integer -> [Integer] -> Int
d' n i (d:ds)
  | i <= is = digitToInt $ show (n + 1 + i `div` d) !! fromInteger (i`mod`d)
  | otherwise = d' (n + is`div`d) (i-is) ds
  where
    is = d*9*10^(d-1)
