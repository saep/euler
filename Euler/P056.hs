{- |
Module      :  Euler.P056
Description :  Problem 56
Copyright   :  (c) Sebastian Witte
License     :  WTFPL

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  portable

Powerful digit sum

A googol (10^100) is a massive number: one followed by one-hundred zeros;
100^100 is almost unimaginably large: one followed by two-hundred zeros. Despite
their size, the sum of the digits in each number is only 1.

Considering natural numbers of the form, a^b, where a, b < 100, what is the
maximum digital sum?
-}
module Euler.P056
       ( solve
       ) where

import Data.Char

solve :: IO Int
solve = return $ maximum [ digitSum (a^b) | a <- [1..99] , b <- [1..99] ]

digitSum :: Integer -> Int
digitSum = sum . fmap digitToInt . show

